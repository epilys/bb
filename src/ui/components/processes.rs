use super::*;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;
use std::str::FromStr;

/* Hold maximum width for each column */
#[derive(Debug)]
struct ColumnWidthMaxima {
    pid: usize,
    ppid: usize,
    vm_rss: usize,
    cpu_percent: usize,
    state: usize,
    username: usize,
}

impl ColumnWidthMaxima {
    fn new() -> ColumnWidthMaxima {
        ColumnWidthMaxima {
            pid: "PID".len(),
            ppid: "PPID".len(),
            vm_rss: "VM_RSS".len(),
            cpu_percent: "CPU%".len(),
            state: 1,
            username: "USER".len(),
        }
    }
}

macro_rules! define_column_string {
    ($($typename: tt),+) => {
        $(
          #[derive(Debug)]
          struct $typename(String);

          impl $typename {
              fn len(&self) -> usize {
                  self.0.len()
              }
          }

          impl std::fmt::Display for $typename {
              fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                  if let Some(width) =  f.width() {
                      write!(f, "{:>width$}", self.0, width = width)
                  } else {
                      write!(f, "{:>}", self.0)
                  }
              }
          })+
    }
}

define_column_string!(
    PidString,
    PpidString,
    VmRssString,
    CmdLineString,
    UserString
);

type Pid = usize;

/* Wrapper type for display strings */
#[derive(Debug)]
struct ProcessDisplay {
    i: Pid,
    pid: PidString,
    ppid: PpidString,
    vm_rss: VmRssString,
    cpu_percent: usize,
    state: State,
    cmd_line: CmdLineString,
    username: UserString,
    utime: usize,
}

/* process list components */
#[derive(Debug)]
pub struct ProcessList {
    page_movement: Option<PageMovement>,
    cpu_stat: Stat,
    pid_max: usize,
    cursor: usize,
    height: usize,
    dirty: bool,
    maxima: ColumnWidthMaxima,
    /* stop updating data */
    freeze: bool,
    processes_times: HashMap<Pid, usize>,
    processes: Vec<ProcessDisplay>,
}

#[derive(Debug, PartialEq)]
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
            processes: Vec::with_capacity(1024),
            processes_times: Default::default(),
            height: 0,
            maxima: ColumnWidthMaxima::new(),
            freeze: false,
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
        let height = height!(area) - 5;
        let old_pages = self.cursor / height;
        let width = width!(area);

        let old_cursor = self.cursor;
        if let Some(mvm) = self.page_movement.take() {
            match mvm {
                PageMovement::Up => {
                    self.cursor = self.cursor.saturating_sub(1);
                }
                PageMovement::Down => {
                    self.cursor = std::cmp::min(self.height.saturating_sub(1), self.cursor + 1);
                }
                PageMovement::Home => {
                    self.cursor = 0;
                }
                PageMovement::PageUp => {
                    self.cursor = self.cursor.saturating_sub(height);
                }
                PageMovement::PageDown => {
                    self.cursor =
                        std::cmp::min(self.height.saturating_sub(1), self.cursor + height);
                }
                PageMovement::End => {
                    self.cursor = self.height.saturating_sub(1);
                }
            }
        }

        if !self.dirty && (!tick/* implies freeze */) && old_cursor != self.cursor {
            /* Nothing to update */
            return;
        }

        let pages = self.cursor / height;
        if pages != old_pages {
            tick = true;
        }

        if tick || self.freeze {
            if tick || old_cursor != self.cursor {
                clear_area(grid, area);
            }

            dirty_areas.push_back(area);

            if !self.freeze {
                self.processes.clear();

                let cpu_stat = get_stat(&mut 0).remove(0);

                /* Keep tabs on biggest element in each column */
                let mut maxima = ColumnWidthMaxima::new();

                self.height = 0;
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

                    let process_display = ProcessDisplay {
                        i: process.pid,
                        pid: PidString(process.pid.to_string()),
                        ppid: PpidString(process.ppid.to_string()),
                        vm_rss: VmRssString(Bytes(process.vm_rss * 1024).as_convenient_string()),
                        cpu_percent: (100.0
                            * ((process.utime
                                - self
                                    .processes_times
                                    .get(&process.pid)
                                    .map(|v| *v)
                                    .unwrap_or(process.utime))
                                as f64
                                / ((cpu_stat.total_time() - self.cpu_stat.total_time()) as f64)))
                            as usize,
                        utime: process.utime,
                        state: process.state,
                        cmd_line: CmdLineString(process.cmd_line),
                        username: UserString(crate::ui::username(process.uid)),
                    };

                    self.processes_times
                        .insert(process.pid, process_display.utime);

                    maxima.pid = std::cmp::max(maxima.pid, process_display.pid.len());
                    maxima.ppid = std::cmp::max(maxima.ppid, process_display.ppid.len());
                    maxima.vm_rss = std::cmp::max(maxima.vm_rss, process_display.vm_rss.len());
                    maxima.username =
                        std::cmp::max(maxima.username, process_display.username.len());
                    self.processes.push(process_display);
                    self.height += 1;
                }
                self.cpu_stat = cpu_stat;
                self.cursor = std::cmp::min(self.height.saturating_sub(1), self.cursor);
                self.maxima = maxima;
            }

            /* Write column headers */
            let (x, y) = write_string_to_grid(
                &format!(
                    "{pid:>max_pid$}  {ppid:>max_ppid$}  {username:>max_username$}  {vm_rss:>max_vm_rss$}  {cpu_percent:>max_cpu_percent$}  {state:>max_state$}  {cmd_line}",
                    pid = "PID",
                    ppid ="PPID",
                    username = "USER",
                    vm_rss = "VM_RSS",
                    cpu_percent = "CPU%",
                    state = " ",
                    cmd_line = "CMD_LINE",
                    max_pid = self.maxima.pid,
                    max_ppid = self.maxima.ppid,
                    max_username = self.maxima.username,
                    max_vm_rss = self.maxima.vm_rss,
                    max_cpu_percent = self.maxima.cpu_percent,
                    max_state = self.maxima.state,
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

            let mut processes = self.processes.iter().collect::<Vec<&ProcessDisplay>>();
            processes.sort_unstable_by(|a, b| b.cpu_percent.cmp(&a.cpu_percent));

            for p in processes.iter().skip(pages * height).take(height) {
                let fg_color = Color::Default;
                let bg_color = if pages * height + y_offset == self.cursor {
                    Color::Byte(235)
                } else {
                    Color::Default
                };
                match executable_path_color(&p.cmd_line) {
                    Ok((path, bin, rest)) => {
                        let (x, y) = write_string_to_grid(
                            &format!(
                    "{pid:>max_pid$}  {ppid:>max_ppid$}  {username:>max_username$}  {vm_rss:>max_vm_rss$}  {cpu_percent:>max_cpu_percent$}%  {state:>max_state$}  ",
                            pid = p.pid,
                            ppid = p.ppid,
                            username = p.username,
                            vm_rss = p.vm_rss,
                            cpu_percent = p.cpu_percent,
                            state = p.state,
                            max_pid = self.maxima.pid,
                            max_ppid = self.maxima.ppid,
                            max_username = self.maxima.username,
                            max_vm_rss = self.maxima.vm_rss,
                            max_cpu_percent = self.maxima.cpu_percent,
                            max_state = self.maxima.state,
                                ),
                            grid,
                            fg_color,
                            bg_color,
                            Attr::Default,
                            (pos_inc(upper_left, (0, y_offset + 3)), bottom_right),
                            false,
                        );
                        if p.state == State::Running {
                            grid[pos_inc(
                                upper_left,
                                (
                                    2 * 5
                                        + self.maxima.pid
                                        + self.maxima.ppid
                                        + self.maxima.username
                                        + self.maxima.vm_rss
                                        + self.maxima.cpu_percent,
                                    y_offset + 3,
                                ),
                            )]
                            .set_fg(if self.freeze {
                                Color::Byte(12)
                            } else {
                                Color::Byte(10)
                            });
                        }
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
                            if self.freeze {
                                Color::Byte(32)
                            } else {
                                Color::Byte(34)
                            },
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
                        let (x,y) = write_string_to_grid(
                            &format!(
                            "{pid:>max_pid$}  {ppid:>max_ppid$}  {username:>max_username$}  {vm_rss:>max_vm_rss$}  {cpu_percent:>max_cpu_percent$}%  {state:>max_state$}  ",
                            pid = p.pid,
                            ppid = p.ppid,
                            username = p.username,
                            vm_rss = p.vm_rss,
                            cpu_percent = p.cpu_percent,
                            state = p.state,
                            max_pid = self.maxima.pid,
                            max_ppid = self.maxima.ppid,
                            max_username = self.maxima.username,
                            max_vm_rss = self.maxima.vm_rss,
                            max_cpu_percent = self.maxima.cpu_percent,
                            max_state = self.maxima.state,
                            ),
                            grid,
                            fg_color,
                            bg_color,
                            Attr::Default,
                            (pos_inc(upper_left, (0, y_offset + 3)), bottom_right),
                            false,
                        );
                        if p.state == State::Running {
                            grid[pos_inc(
                                upper_left,
                                (
                                    2 * 5
                                        + 1
                                        + self.maxima.pid
                                        + self.maxima.ppid
                                        + self.maxima.username
                                        + self.maxima.vm_rss
                                        + self.maxima.cpu_percent,
                                    y_offset + 3,
                                ),
                            )]
                            .set_fg(if self.freeze {
                                Color::Byte(12)
                            } else {
                                Color::Byte(10)
                            });
                        }
                        let (x, y) = write_string_to_grid(
                            bin,
                            grid,
                            if self.freeze {
                                Color::Byte(32)
                            } else {
                                Color::Byte(34)
                            },
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
        let map = &self.get_shortcuts()[""];
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
            UIEvent::Input(k) if *k == map["freeze updates"] => {
                self.freeze = !self.freeze;
                self.dirty = true;
            }
            _ => {}
        }
    }

    fn is_dirty(&self) -> bool {
        true
    }

    fn set_dirty(&mut self) {}
    fn get_shortcuts(&self) -> ShortcutMaps {
        let mut map: ShortcutMap = Default::default();
        map.insert("freeze updates", Key::Char('f'));
        let mut ret: ShortcutMaps = Default::default();
        ret.insert("".to_string(), map);
        ret
    }
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

fn executable_path_color(p: &CmdLineString) -> Result<(&str, &str, &str), (&str, &str)> {
    let p = &p.0;
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
