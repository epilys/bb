use super::*;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;
use std::str::FromStr;

type Pid = usize;

/* process list components */
#[derive(Debug)]
pub struct ProcessList {
    page_movement: Option<PageMovement>,
    cpu_stat: Stat,
    pid_max: usize,
    cursor: usize,
    dirty: bool,
    processes: HashMap<Pid, usize>,
    /* refresh process list every 4 cycles */
    cycle: u8,
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
    vm_rss: usize,
    state: State,
    uid: u32,
    cmd_line: String,
    utime: usize,
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
            cpu_stat: get_stat(&mut 0).remove(0),
            pid_max: usize::from_str(pid_max.trim()).unwrap(),
            processes: Default::default(),
            cycle: 0,
            dirty: true,
        }
    }
}

/* Prints the process list */
impl Component for ProcessList {
    fn draw(
        &mut self,
        grid: &mut CellBuffer,
        area: Area,
        dirty_areas: &mut VecDeque<Area>,
        mut tick: bool,
    ) {
        if !is_valid_area!(area) {
            return;
        }

        if !self.dirty && !tick {
            return;
        }

        let upper_left = pos_inc(upper_left!(area), (1, 0));
        let bottom_right = pos_dec(bottom_right!(area), (1, 1));
        /* Reserve first row for column headers */
        let height = height!(area) - 1;
        let old_pages = self.cursor / height;
        let width = width!(area);

        let old_cursor = self.cursor;
        if let Some(mvm) = self.page_movement.take() {
            match mvm {
                PageMovement::Up => {
                    self.cursor = self.cursor.saturating_sub(1);
                }
                PageMovement::Down => {
                    self.cursor =
                        std::cmp::min(self.processes.len().saturating_sub(1), self.cursor + 1);
                }
                PageMovement::Home => {
                    self.cursor = 0;
                }
                PageMovement::PageUp => {
                    self.cursor = self.cursor.saturating_sub(height);
                }
                PageMovement::PageDown => {
                    self.cursor =
                        std::cmp::min(self.processes.len().saturating_sub(1), self.cursor + height);
                }
                PageMovement::End => {
                    self.cursor = self.processes.len().saturating_sub(1);
                }
            }
        }
        let pages = self.cursor / height;
        if pages != old_pages {
            tick = true;
        }

        if tick {
            clear_area(grid, area);
            dirty_areas.push_back(area);

            let mut processes: Vec<(String, String, String, usize, String, String, String)> =
                Vec::with_capacity(1024);

            let cpu_stat = get_stat(&mut 0).remove(0);

            /* Keep tabs on biggest element in each column */
            let mut maxima = (
                "PID".len(),
                "PPID".len(),
                "VM_RSS".len(),
                "CPU%".len(),
                " ".len(),
                "USER".len(),
            );

            for entry in std::fs::read_dir("/proc/").unwrap() {
                let dir = entry.unwrap();
                if let Some(fname) = dir.file_name().to_str() {
                    if !fname.chars().all(|c| c.is_numeric()) {
                        continue;
                    }
                } else {
                    continue;
                }

                let process = if let Ok(p) = get_pid_info(dir.path()) {
                    p
                } else {
                    continue;
                };

                if process.cmd_line.is_empty() {
                    /* This is a kernel thread, skip for now */
                    continue;
                }

                let strings = (
                    process.pid.to_string(),
                    process.ppid.to_string(),
                    Bytes(process.vm_rss * 1024).as_convenient_string(),
                    (100.0
                        * ((process.utime
                            - self
                                .processes
                                .get(&process.pid)
                                .map(|v| *v)
                                .unwrap_or(process.utime)) as f64
                            / ((cpu_stat.total_time() - self.cpu_stat.total_time()) as f64)))
                        as usize,
                    process.state.to_string(),
                    process.cmd_line,
                    crate::ui::username(process.uid),
                );

                self.processes.insert(process.pid, process.utime);

                maxima.0 = std::cmp::max(maxima.0, strings.0.len());
                maxima.1 = std::cmp::max(maxima.1, strings.1.len());
                maxima.2 = std::cmp::max(maxima.2, strings.2.len());
                maxima.4 = std::cmp::max(maxima.4, strings.4.len());
                maxima.5 = std::cmp::max(maxima.5, strings.6.len());
                processes.push(strings);
            }
            self.cpu_stat = cpu_stat;

            processes.sort_unstable_by(|a, b| b.3.cmp(&a.3));

            /* Write column headers */
            let (x, y) = write_string_to_grid(
                &format!(
                    "{:>maxima0$}  {:>maxima1$}  {:>maxima5$}  {:>maxima2$}  {:>maxima3$}  {:>maxima4$}  {}",
                    "PID",
                    "PPID",
                    "USER",
                    "VM_RSS",
                    "CPU%",
                    " ",
                    "CMD_LINE",
                    maxima0 = maxima.0,
                    maxima1 = maxima.1,
                    maxima2 = maxima.2,
                    maxima3 = maxima.3,
                    maxima4 = maxima.4,
                    maxima5 = maxima.5,
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
                Some(Color::Black),
                Some(Color::White),
            );

            let mut y_offset = 0;
            let p_len = processes.len();
            for (pid, ppid, vm_rss, cpu, state, cmd_line, username) in
                processes.into_iter().skip(pages * height)
            {
                let fg_color = Color::Default;
                let bg_color = if pages * height + y_offset == self.cursor {
                    Color::Byte(235)
                } else {
                    Color::Default
                };
                match executable_path_color(&cmd_line) {
                    Ok((path, bin, rest)) => {
                        let (x, y) = write_string_to_grid(
                            &format!(
                            "{:>maxima0$}  {:>maxima1$}  {:>maxima5$}  {:>maxima2$}  {:>maxima3$}%  {:>maxima4$}",
                            pid,
                            ppid,
                            username,
                            vm_rss,
                            cpu,
                            state,
                            maxima0 = maxima.0,
                            maxima1 = maxima.1,
                            maxima2 = maxima.2,
                            maxima3 = maxima.3,
                            maxima4 = maxima.4,
                            maxima5 = maxima.5,
                        ),
                            grid,
                            fg_color,
                            bg_color,
                            Attr::Default,
                            (pos_inc(upper_left, (0, y_offset + 3)), bottom_right),
                            false,
                        );
                        let (x, _) = write_string_to_grid(
                            path,
                            grid,
                            Color::Byte(243),
                            bg_color,
                            Attr::Default,
                            (pos_inc(upper_left, (x, y_offset + 3)), bottom_right),
                            false,
                        );
                        let (x, y) = write_string_to_grid(
                            bin,
                            grid,
                            Color::Byte(34),
                            bg_color,
                            Attr::Default,
                            (pos_inc(upper_left, (x - 1, y_offset + 3)), bottom_right),
                            false,
                        );
                        let (x, y) = write_string_to_grid(
                            rest,
                            grid,
                            fg_color,
                            bg_color,
                            Attr::Default,
                            (pos_inc(upper_left, (x - 1, y_offset + 3)), bottom_right),
                            false,
                        );
                        change_colors(
                            grid,
                            ((x, y), set_y(bottom_right, y)),
                            Some(fg_color),
                            Some(bg_color),
                        );
                    }
                    Err((bin, rest)) => {
                        let(x,y)=write_string_to_grid(
                            &format!(
                                "{:>maxima0$}  {:>maxima1$}  {:>maxima5$}  {:>maxima2$}  {:>maxima3$}%  {:>maxima4$}  ",
                                pid,
                                ppid,
                                username,
                                vm_rss,
                                cpu,
                                state,
                                maxima0 = maxima.0,
                                maxima1 = maxima.1,
                                maxima2 = maxima.2,
                                maxima3 = maxima.3,
                                maxima4 = maxima.4,
                                maxima5 = maxima.5,
                                ),
                                grid,
                                fg_color,
                                bg_color,
                                Attr::Default,
                                (pos_inc(upper_left, (0, y_offset + 3)), bottom_right),
                                false,
                                );
                        let (x, y) = write_string_to_grid(
                            bin,
                            grid,
                            Color::Byte(34),
                            bg_color,
                            Attr::Default,
                            (pos_inc(upper_left, (x - 1, y_offset + 3)), bottom_right),
                            false,
                        );
                        let (x, y) = write_string_to_grid(
                            rest,
                            grid,
                            fg_color,
                            bg_color,
                            Attr::Default,
                            (pos_inc(upper_left, (x - 1, y_offset + 3)), bottom_right),
                            false,
                        );
                        change_colors(
                            grid,
                            ((x, y), set_y(bottom_right, y)),
                            Some(fg_color),
                            Some(bg_color),
                        );
                    }
                }
                y_offset += 1;
                if y_offset >= height {
                    break;
                }
            }
            self.cycle += 1;
            self.cycle %= 4;
        } else if old_cursor != self.cursor {
            let new_area = (
                pos_inc(upper_left, (0, self.cursor - pages * height + 3)),
                set_y(
                    bottom_right,
                    get_y(upper_left) + self.cursor - pages * height + 3,
                ),
            );
            let old_area = (
                pos_inc(upper_left, (0, old_cursor - old_pages * height + 3)),
                set_y(
                    bottom_right,
                    get_y(upper_left) + old_cursor - old_pages * height + 3,
                ),
            );
            change_colors(grid, new_area, None, Some(Color::Byte(235)));
            change_colors(grid, old_area, None, Some(Color::Default));
            dirty_areas.push_back(old_area);
            dirty_areas.push_back(new_area);
        }
        self.dirty = false;
    }

    fn process_event(&mut self, event: &mut UIEvent) {
        match event {
            UIEvent::Input(Key::Up) => {
                self.page_movement = Some(PageMovement::Up);
                self.dirty = true;
            }
            UIEvent::Input(Key::Down) => {
                self.page_movement = Some(PageMovement::Down);
                self.dirty = true;
            }
            UIEvent::Input(Key::Home) => {
                self.page_movement = Some(PageMovement::Home);
                self.dirty = true;
            }
            UIEvent::Input(Key::PageUp) => {
                self.page_movement = Some(PageMovement::PageUp);
                self.dirty = true;
            }
            UIEvent::Input(Key::PageDown) => {
                self.page_movement = Some(PageMovement::PageDown);
                self.dirty = true;
            }
            UIEvent::Input(Key::End) => {
                self.page_movement = Some(PageMovement::End);
                self.dirty = true;
            }
            _ => {}
        }
    }

    fn is_dirty(&self) -> bool {
        true
    }

    fn set_dirty(&mut self) {}
}

/* proc file structure can be found in man 5 proc */
fn get_pid_info(mut path: PathBuf) -> Result<Process, std::io::Error> {
    path.push("status");
    let mut file: File = File::open(&path)?;
    let mut res = String::with_capacity(2048);
    file.read_to_string(&mut res)?;
    let mut lines_iter = res.lines();
    let mut ret = Process {
        pid: 0,
        ppid: 0,
        vm_rss: 0,
        uid: 0,
        utime: 0,
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
            "VmRSS:" => {
                ret.vm_rss = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
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
                ret.uid = u32::from_str(mut_value_iter.next().unwrap()).unwrap();
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
        /* values are separated by null bytes */
        ret.cmd_line = format!("{}", res.split('\0').collect::<Vec<&str>>().join(" "));
    }
    path.pop();
    path.push("stat");
    let mut file: File = File::open(&path).unwrap();
    res.clear();
    file.read_to_string(&mut res).unwrap();
    /* values are separated by whitespace and are in a specific order */
    if !res.is_empty() {
        let mut vals = res.split_whitespace().skip(13);
        ret.utime = usize::from_str(vals.next().unwrap()).unwrap();
        ret.utime += usize::from_str(vals.next().unwrap()).unwrap();
        ret.utime += usize::from_str(vals.next().unwrap()).unwrap();
        ret.utime += usize::from_str(vals.next().unwrap()).unwrap();
    }
    Ok(ret)
}

fn executable_path_color(p: &str) -> Result<(&str, &str, &str), (&str, &str)> {
    if !p.starts_with("/") {
        return if let Some(first_whitespace) = p.as_bytes().iter().position(|c| *c == b' ') {
            Err(p.split_at(first_whitespace))
        } else {
            Err((p, ""))
        };
    }

    if let Some(first_whitespace) = p.as_bytes().iter().position(|c| *c == b' ') {
        if let Some(path_end) = p[0..first_whitespace]
            .as_bytes()
            .iter()
            .rposition(|c| *c == b'/')
        {
            let (path, rest) = p.split_at(path_end + 1);
            if let Some(first_whitespace) = rest.as_bytes().iter().position(|c| *c == b' ') {
                let (bin, rest) = rest.split_at(first_whitespace);
                Ok((path, bin, rest))
            } else {
                Ok((path, rest, ""))
            }
        } else {
            return Err(p.split_at(first_whitespace));
        }
    } else {
        if let Some(path_end) = p.as_bytes().iter().rposition(|c| *c == b'/') {
            let (path, bin) = p.split_at(path_end + 1);
            Ok((path, bin, ""))
        } else {
            return Err((p, ""));
        }
    }
}
