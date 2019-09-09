/*
 * bb
 *
 * Copyright 2019 Manos Pitsidianakis
 *
 * This file is part of bb.
 *
 * bb is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * bb is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with bb. If not, see <http://www.gnu.org/licenses/>.
 */

use super::*;
use crossbeam::{Receiver, Sender};

const SIGNAL_LIST: &[(i32, &'static str)] = &[
    (1, "1 HUP"),
    (2, "2 INT"),
    (3, "3 QUIT"),
    (4, "4 ILL"),
    (5, "5 TRAP"),
    (6, "6 ABRT"),
    (7, "7 BUS"),
    (8, "8 FPE"),
    (9, "9 KILL"),
    (10, "10 USR1"),
    (11, "11 SEGV"),
    (12, "12 USR2"),
    (13, "13 PIPE"),
    (14, "14 ALRM"),
    (15, "15 TERM"),
    (16, "16 STKFLT"),
    (17, "17 CHLD"),
    (18, "18 CONT"),
    (19, "19 STOP"),
    (20, "20 TSTP"),
    (21, "21 TTIN"),
    (22, "22 TTOU"),
    (23, "23 URG"),
    (24, "24 XCPU"),
    (25, "25 XFSZ"),
    (26, "26 VTALRM"),
    (27, "27 PROF"),
    (28, "28 WINCH"),
    (29, "29 POLL"),
    (30, "30 PWR"),
    (31, "31 SYS"),
];

/* Hold maximum width for each column */
#[derive(Debug)]
pub struct ColumnWidthMaxima {
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
            cpu_percent: " CPU%".len(),
            state: 1,
            username: "USER".len(),
        }
    }
}

macro_rules! define_column_string {
    ($($typename: tt),+) => {
        $(
          #[derive(Debug)]
          pub struct $typename(pub String);

          impl $typename {
              #[allow(dead_code)]
              pub fn len(&self) -> usize {
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

pub type Pid = i32;

/* Wrapper type for display strings */
#[derive(Debug)]
pub struct ProcessDisplay {
    pub i: Pid,
    pub pid: PidString,
    pub ppid: PpidString,
    pub vm_rss: VmRssString,
    pub cpu_percent: usize,
    pub state: State,
    pub cmd_line: CmdLineString,
    pub username: UserString,
    pub utime: usize,
}

/* process list components */
#[derive(Debug)]
pub struct ProcessList {
    sender: Sender<Vec<ProcessDisplay>>,
    rcver: Receiver<Vec<ProcessDisplay>>,
    page_movement: Option<PageMovement>,
    cpu_stat: Stat,
    cursor: usize,
    height: usize,
    dirty: bool,
    maxima: ColumnWidthMaxima,
    /* stop updating data */
    freeze: bool,
    processes_times: HashMap<Pid, usize>,
    processes: Vec<ProcessDisplay>,
    mode: ProcessListMode,
}

#[derive(Debug, PartialEq)]
enum ProcessListMode {
    Normal,
    Kill(u16),
}

use ProcessListMode::*;

#[derive(Debug, PartialEq)]
pub enum State {
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
    /* X  Dead (from Linux 2.6.0 onward) */
    Dead,
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
            'X' => State::Dead,
            'x' => State::Dead,
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
                State::Dead => 'X',
            }
        )
    }
}

pub struct Process {
    pub pid: i32,
    pub ppid: i32,
    pub vm_rss: usize,
    pub state: State,
    pub uid: u32,
    pub cmd_line: String,
    pub utime: usize,
}

impl fmt::Display for ProcessList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "process list")
    }
}

impl ProcessList {
    pub fn new(sender: Sender<Vec<ProcessDisplay>>, rcver: Receiver<Vec<ProcessDisplay>>) -> Self {
        ProcessList {
            sender,
            rcver,
            cursor: 0,
            page_movement: None,
            cpu_stat: get_stat(&mut 0).remove(0),
            processes: Vec::with_capacity(1024),
            processes_times: Default::default(),
            height: 0,
            maxima: ColumnWidthMaxima::new(),
            freeze: false,
            mode: Normal,
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
        let height = height!(area) - 2;
        let old_pages = (self.cursor) / height;

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

        let pages = (self.cursor) / height;
        if pages != old_pages {
            tick = true;
        }

        let update_maxima = if tick && !self.freeze {
            match self.rcver.recv_timeout(std::time::Duration::new(0, 0)) {
                Ok(new_processes) => {
                    self.sender
                        .send(std::mem::replace(&mut self.processes, new_processes))
                        .unwrap();
                    true
                }
                Err(_) => false,
            }
        } else {
            false
        };

        if tick || self.freeze {
            if tick || old_cursor != self.cursor {
                clear_area(grid, area);
            }

            dirty_areas.push_back(area);

            if !self.freeze && update_maxima {
                /* Keep tabs on biggest element in each column */
                self.maxima = ColumnWidthMaxima::new();

                for p in &self.processes {
                    self.maxima.pid = std::cmp::max(self.maxima.pid, p.pid.len());
                    self.maxima.ppid = std::cmp::max(self.maxima.ppid, p.ppid.len());
                    self.maxima.vm_rss = std::cmp::max(self.maxima.vm_rss, p.vm_rss.len());
                    self.maxima.username = std::cmp::max(self.maxima.username, p.username.len());
                }

                self.height = self.processes.len();
                self.cursor = std::cmp::min(self.height.saturating_sub(1), self.cursor);
            }

            /* Write column headers */
            let (x, y) = write_string_to_grid(
                &format!(
                    "{pid:>max_pid$}  {ppid:>max_ppid$}  {username:>max_username$}  {vm_rss:>max_vm_rss$}  {cpu_percent:>max_cpu_percent$}  {state:>max_state$}  {cmd_line}",
                    pid = "PID",
                    ppid ="PPID",
                    username = "USER",
                    vm_rss = "VM_RSS",
                    cpu_percent = "  CPU%",
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
                (pos_inc(upper_left, (0, 1)), bottom_right),
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
                            (pos_inc(upper_left, (0, y_offset + 2)), bottom_right),
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
                                    y_offset + 2,
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
                            (pos_inc(upper_left, (x - 1, y_offset + 2)), bottom_right),
                            false,
                        );
                        let (x, _) = write_string_to_grid(
                            bin,
                            grid,
                            if self.freeze {
                                Color::Byte(32)
                            } else {
                                Color::Byte(34)
                            },
                            bg_color,
                            Attr::Default,
                            (pos_inc(upper_left, (x - 1, y_offset + 2)), bottom_right),
                            false,
                        );
                        let (x, _) = write_string_to_grid(
                            rest,
                            grid,
                            fg_color,
                            bg_color,
                            Attr::Default,
                            (pos_inc(upper_left, (x - 1, y_offset + 2)), bottom_right),
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
                        let (x, _) = write_string_to_grid(
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
                            (pos_inc(upper_left, (0, y_offset + 2)), bottom_right),
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
                                    y_offset + 2,
                                ),
                            )]
                            .set_fg(if self.freeze {
                                Color::Byte(12)
                            } else {
                                Color::Byte(10)
                            });
                        }
                        let (x, _) = write_string_to_grid(
                            bin,
                            grid,
                            if self.freeze {
                                Color::Byte(32)
                            } else {
                                Color::Byte(34)
                            },
                            bg_color,
                            Attr::Default,
                            (pos_inc(upper_left, (x - 1, y_offset + 2)), bottom_right),
                            false,
                        );
                        let (x, y) = write_string_to_grid(
                            rest,
                            grid,
                            fg_color,
                            bg_color,
                            Attr::Default,
                            (pos_inc(upper_left, (x - 1, y_offset + 2)), bottom_right),
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
            }
        } else if old_cursor != self.cursor {
            let new_area = (
                pos_inc(upper_left, (0, self.cursor + 2 - pages * height)),
                set_y(
                    bottom_right,
                    get_y(upper_left) + self.cursor + 2 - pages * height,
                ),
            );
            let old_area = (
                pos_inc(upper_left, (0, old_cursor + 2 - old_pages * height)),
                set_y(
                    bottom_right,
                    get_y(upper_left) + old_cursor + 2 - old_pages * height,
                ),
            );
            change_colors(grid, new_area, None, Some(Color::Byte(235)));
            change_colors(grid, old_area, None, Some(Color::Default));
            dirty_areas.push_back(old_area);
            dirty_areas.push_back(new_area);
        }

        if let Kill(ref n) = self.mode {
            let (cols, rows) = grid.size();
            let margin_left = (cols / 2).saturating_sub(16);
            let margin_top = (rows / 2).saturating_sub(12);
            let box_area = (
                (margin_left, margin_top),
                (margin_left + 32, margin_top + 12),
            );
            clear_area(grid, box_area);
            create_box(grid, box_area);
            let mut x = 1;
            for s in SIGNAL_LIST.chunks(11) {
                let mut y = 0;
                for sig in s {
                    write_string_to_grid(
                        sig.1,
                        grid,
                        Color::Default,
                        Color::Default,
                        Attr::Default,
                        (pos_inc(upper_left!(box_area), (x, 1 + y)), bottom_right),
                        false,
                    );
                    y += 1;
                }
                x += 11;
            }
            let box_area = (
                (margin_left, margin_top + 13),
                (margin_left + 32, margin_top + 16),
            );
            clear_area(grid, box_area);
            create_box(grid, box_area);
            let signal_fmt = if *n == 0 {
                format!("__")
            } else if *n < 32 {
                format!("{} [{}]", SIGNAL_LIST[*n as usize - 1].1, *n)
            } else {
                format!("invalid [{}]", *n)
            };
            write_string_to_grid(
                &format!(
                    "{cmd_line} [{pid}]",
                    pid = self.processes[self.cursor].i,
                    cmd_line = &self.processes[self.cursor].cmd_line.0
                        [0..std::cmp::min(26, self.processes[self.cursor].cmd_line.len())],
                ),
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                (
                    pos_inc(upper_left!(box_area), (1, 1)),
                    bottom_right!(box_area),
                ),
                false,
            );
            write_string_to_grid(
                &format!("send {signal}", signal = signal_fmt,),
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                (
                    pos_inc(upper_left!(box_area), (1, 2)),
                    bottom_right!(box_area),
                ),
                false,
            );
            /*
            let rows = SIGNAL_LIST.len() / width!(area) + 6;
            let box_area = (pos_inc(upper_left, (0, height - rows)), bottom_right);
            clear_area(grid, box_area);
            create_box(grid, box_area);
            let write_area = (
                pos_inc(upper_left, (3, height - rows + 1)),
                pos_dec(bottom_right, (3, 0)),
            );
            let mut processes = self.processes.iter().collect::<Vec<&ProcessDisplay>>();
            processes.sort_unstable_by(|a, b| b.cpu_percent.cmp(&a.cpu_percent));
            let (_, y) = write_string_to_grid(
                &format!("pid {}", processes[self.cursor].i,),
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                write_area,
                false,
            );
            let (_, y) = write_string_to_grid(
                &format!("cmd_line {}", processes[self.cursor].cmd_line),
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                (
                    (get_x(upper_left!(write_area)), y + 1),
                    bottom_right!(write_area),
                ),
                false,
            );
            let (_, y) = write_string_to_grid(
                &format!("sig_no {}", n),
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                (
                    (get_x(upper_left!(write_area)), y + 1),
                    bottom_right!(write_area),
                ),
                false,
            );
            write_string_to_grid(
                SIGNAL_LIST,
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                (
                    (get_x(upper_left!(write_area)), y + 1),
                    bottom_right!(write_area),
                ),
                true,
            );
            */
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
            UIEvent::Input(k) if *k == map["freeze updates"] && self.mode == Normal => {
                self.freeze = !self.freeze;
                self.dirty = true;
            }
            UIEvent::Input(k) if *k == map["kill process"] => {
                self.mode = Kill(0);
                self.freeze = true;
                self.dirty = true;
            }
            UIEvent::Input(k) if *k == map["cancel"] => {
                if let Kill(_) = self.mode {
                    self.mode = Normal;
                    self.freeze = false;
                    self.dirty = true;
                }
            }
            UIEvent::Input(Key::Char(f)) if self.mode != Normal && f.is_numeric() => {
                if let Kill(ref mut n) = self.mode {
                    *n = *n * 10 + (f.to_digit(10).unwrap() as u16);
                }
            }
            UIEvent::Input(Key::Backspace) if self.mode != Normal => {
                if let Kill(ref mut n) = self.mode {
                    *n = *n / 10;
                }
            }
            UIEvent::Input(Key::Char('\n')) if self.mode != Normal => {
                let mut processes = self.processes.iter().collect::<Vec<&ProcessDisplay>>();
                processes.sort_unstable_by(|a, b| b.cpu_percent.cmp(&a.cpu_percent));
                if let Kill(ref mut n) = self.mode {
                    use nix::sys::signal::kill;
                    kill(
                        nix::unistd::Pid::from_raw(processes[self.cursor].i),
                        nix::sys::signal::Signal::from_c_int(*n as i32).unwrap(),
                    )
                    .ok()
                    .take();
                }
                self.mode = Normal;
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
        map.insert("kill process", Key::Char('k'));
        map.insert("cancel", Key::Esc);
        let mut ret: ShortcutMaps = Default::default();
        ret.insert("".to_string(), map);
        ret
    }
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
