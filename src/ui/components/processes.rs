use super::*;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;
use std::str::FromStr;

/* process list components */
#[derive(Debug)]
pub struct ProcessList {
    page_movement: Option<PageMovement>,
    pid_max: usize,
    cursor: usize,
    dirty: bool,
}

enum State {
    /* Z  Zombie */
    Zombie,
    /* R  Running */
    Running,
    /* S  Sleeping in an interruptible wait */
    Sleeping,
    /* D  Waiting in uninterruptible disk sleep */
    Waiting,
    /* T  Stopped (on a signal) or (before Linux 2.6.33) trace stopped */
    Stopped,
    /* t  Tracing stop (Linux 2.6.33 onward) */
    Tracing,
    /*W  Paging (only before Linux 2.6.0) */
    Paging,
    /* X  Dead (from Linux 2.6.0 onward) */
    Dead,
    /* K  Wakekill (Linux 2.6.33 to 3.13 only) */
    Wakekill,
    /* W  Waking (Linux 2.6.33 to 3.13 only) */
    Waking,
    /* P  Parked (Linux 3.9 to 3.13 only) */
    Parked,
}

impl From<char> for State {
    fn from(val: char) -> State {
        match val {
            'R' => State::Running,
            'I' => State::Sleeping,
            'S' => State::Sleeping,
            'D' => State::Waiting,
            'Z' => State::Zombie,
            'T' => State::Stopped,
            't' => State::Tracing,
            'W' => State::Paging,
            'X' => State::Dead,
            'x' => State::Dead,
            'K' => State::Wakekill,
            'W' => State::Waking,
            'P' => State::Parked,
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                State::Running => 'R',
                State::Sleeping => 'S',
                State::Waiting => 'D',
                State::Zombie => 'Z',
                State::Stopped => 'T',
                State::Tracing => 't',
                State::Paging => 'W',
                State::Dead => 'X',
                State::Dead => 'x',
                State::Wakekill => 'K',
                State::Waking => 'W',
                State::Parked => 'P',
                _ => unreachable!(),
            }
        )
    }
}

struct Process {
    pid: usize,
    ppid: usize,
    vm_size: usize,
    state: State,
    uid: usize,
    cmd_line: String,
}

impl fmt::Display for ProcessList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "process list")
    }
}

impl ProcessList {
    pub fn new() -> Self {
        let mut file = File::open("/proc/sys/kernel/pid_max").unwrap();
        let mut pid_max = String::new();
        file.read_to_string(&mut pid_max).unwrap();
        ProcessList {
            cursor: 0,
            page_movement: None,
            pid_max: usize::from_str(pid_max.trim()).unwrap(),
            dirty: true,
        }
    }
}

impl Component for ProcessList {
    fn draw(
        &mut self,
        grid: &mut CellBuffer,
        area: Area,
        dirty_areas: &mut VecDeque<Area>,
        tick: bool,
    ) {
        if !is_valid_area!(area) {
            return;
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        /* Reserve first row for column headers */
        let height = height!(area) - 1;
        let width = width!(area);
        clear_area(grid, area);
        dirty_areas.push_back(area);

        let mut processes: Vec<(String, String, String, String, String)> = Vec::with_capacity(1024);
        let mut pid = 0;
        let mut maxima = ("PID".len(), "PPID".len(), "VM_SIZE".len(), "STATE".len());
        for entry in std::fs::read_dir("/proc/").unwrap() {
            let dir = entry.unwrap();
            if let Some(fname) = dir.file_name().to_str() {
                if !fname.chars().all(|c| c.is_numeric()) {
                    continue;
                }
            } else {
                continue;
            }
            let process = get_pid_info(dir.path());
            let strings = (
                process.pid.to_string(),
                process.ppid.to_string(),
                Bytes(process.vm_size * 1024).as_convenient_string(),
                process.state.to_string(),
                process.cmd_line,
            );
            maxima.0 = std::cmp::max(maxima.0, strings.0.len());
            maxima.1 = std::cmp::max(maxima.1, strings.1.len());
            maxima.2 = std::cmp::max(maxima.2, strings.2.len());
            maxima.3 = std::cmp::max(maxima.3, strings.3.len());
            processes.push(strings);
        }

        processes.sort_unstable_by(|a, b| a.4.cmp(&b.4));

        if let Some(mvm) = self.page_movement.take() {
            match mvm {
                PageMovement::Home => {
                    self.cursor = 0;
                }
                PageMovement::PageUp => {
                    self.cursor = self.cursor.saturating_sub(height);
                }
                PageMovement::PageDown => {
                    self.cursor =
                        std::cmp::min(processes.len().saturating_sub(1), self.cursor + height);
                }
                PageMovement::End => {
                    self.cursor = processes.len().saturating_sub(1);
                }
            }
        }
        if self.dirty {
            /* Write column headers */
            let (x, y) = write_string_to_grid(
                &format!(
                    "{:>maxima0$}  {:>maxima1$}  {:>maxima2$}  {:>maxima3$}  {}",
                    "PID",
                    "PPID",
                    "VM_SIZE",
                    "STATE",
                    "CMD_LINE",
                    maxima0 = maxima.0,
                    maxima1 = maxima.1,
                    maxima2 = maxima.2,
                    maxima3 = maxima.3,
                ),
                grid,
                Color::Black,
                Color::White,
                Attr::Default,
                (pos_inc(upper_left, (0, 2)), bottom_right),
                false,
            );
            change_colors(
                grid,
                ((x, y), set_y(bottom_right, y)),
                Color::Black,
                Color::White,
            );
        }

        let mut y_offset = 0;
        let pages = self.cursor / height;
        let p_len = processes.len();
        for (pid, ppid, vm_size, state, cmd_line) in processes.into_iter().skip(pages * height) {
            let (x, y) = write_string_to_grid(
                &format!(
                    "{:>maxima0$}  {:>maxima1$}  {:>maxima2$}  {:>maxima3$}  {}",
                    pid,
                    ppid,
                    vm_size,
                    state,
                    cmd_line,
                    maxima0 = maxima.0,
                    maxima1 = maxima.1,
                    maxima2 = maxima.2,
                    maxima3 = maxima.3,
                ),
                grid,
                if pages * height + y_offset == self.cursor {
                    Color::Black
                } else {
                    Color::Default
                },
                if pages * height + y_offset == self.cursor {
                    Color::Byte(243)
                } else {
                    Color::Default
                },
                Attr::Default,
                (pos_inc(upper_left, (0, y_offset + 3)), bottom_right),
                false,
            );
            y_offset += 1;
            if y_offset >= height {
                break;
            }
        }
    }

    fn process_event(&mut self, event: &mut UIEvent) {
        match event {
            UIEvent::Input(Key::Up) => {
                self.cursor = self.cursor.saturating_sub(1);
            }
            UIEvent::Input(Key::Down) => {
                self.cursor += 1;
            }
            UIEvent::Input(Key::Home) => {
                self.page_movement = Some(PageMovement::Home);
            }
            UIEvent::Input(Key::PageUp) => {
                self.page_movement = Some(PageMovement::PageUp);
            }
            UIEvent::Input(Key::PageDown) => {
                self.page_movement = Some(PageMovement::PageDown);
            }
            UIEvent::Input(Key::End) => {
                self.page_movement = Some(PageMovement::End);
            }
            _ => {}
        }
    }

    fn is_dirty(&self) -> bool {
        true
    }

    fn set_dirty(&mut self) {}
}

fn get_pid_info(mut path: PathBuf) -> Process {
    path.push("status");
    let mut file: File = File::open(&path).unwrap();
    let mut res = String::with_capacity(2048);
    file.read_to_string(&mut res).unwrap();
    let mut lines_iter = res.lines();
    let mut ret = Process {
        pid: 0,
        ppid: 0,
        vm_size: 0,
        uid: 0,
        state: State::Waiting,
        cmd_line: String::new(),
    };
    let mut line;

    loop {
        let line_opt = lines_iter.next();
        if line_opt.is_none() {
            break;
        }
        line = line_opt.unwrap();
        let mut mut_value_iter = line.split_whitespace();
        match mut_value_iter.next().unwrap() {
            "Name:" => {
                ret.cmd_line = mut_value_iter.next().unwrap().to_string();
            }
            "VmSize:" => {
                ret.vm_size = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
            }
            "State:" => {
                ret.state = State::from(mut_value_iter.next().unwrap().chars().next().unwrap());
            }
            "Pid:" => {
                ret.pid = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
            }
            "PPid:" => {
                ret.ppid = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
            }
            "Uid:" => {
                ret.uid = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
            }
            _ => {}
        }
    }
    path.pop();
    path.push("cmdline");
    let mut file: File = File::open(&path).unwrap();
    res.clear();
    file.read_to_string(&mut res).unwrap();
    if !res.is_empty() {
        ret.cmd_line = format!("{}", res.split('\0').collect::<Vec<&str>>().join(" "));
    }
    ret
}
