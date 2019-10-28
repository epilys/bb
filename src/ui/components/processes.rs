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
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;
use std::str::FromStr;
#[derive(Debug)]
pub struct ProcessData {
    cpu_stat: Stat,
    processes_times: HashMap<Pid, usize>,
    parents: HashMap<Pid, Vec<Pid>>,
    processes_index: HashMap<Pid, usize>,
    tree_index: HashMap<Pid, usize>,
    tree: Vec<(usize, Pid)>,
}

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

#[derive(Debug, Copy, Clone)]
enum Sort {
    UserAsc,
    UserDesc,
    VmRssAsc,
    VmRssDesc,
    CpuAsc,
    CpuDesc,
    CmdLineAsc,
    CmdLineDesc,
}

/* Wrapper type for display strings */
#[derive(Debug)]
pub struct ProcessDisplay {
    pub i: Pid,
    pub p: Pid,
    pub pid: PidString,
    pub ppid: PpidString,
    pub vm_rss: VmRssString,
    vm_rss_value: usize,
    pub cpu_percent: usize,
    pub state: State,
    pub cmd_line: CmdLineString,
    pub username: UserString,
    pub utime: usize,
}

/* process list components */
#[derive(Debug)]
pub struct ProcessList {
    page_movement: Option<PageMovement>,
    cpu_stat: Stat,
    data: ProcessData,
    cursor: usize,
    height: usize,
    dirty: bool,
    force_redraw: bool,
    filter_term: Option<String>,
    maxima: ColumnWidthMaxima,
    /* stop updating data */
    freeze: bool,
    draw_tree: bool,
    processes_times: HashMap<Pid, usize>,
    processes: Vec<ProcessDisplay>,
    sort: Sort,
    mode: ProcessListMode,
}

#[derive(Debug, PartialEq)]
enum ProcessListMode {
    Normal,
    Follow(Pid),
    Locate(Pid),
    Kill(u16),
}

impl ProcessListMode {
    fn is_normal(&self) -> bool {
        *self == Normal
    }

    fn is_follow(&self) -> bool {
        match self {
            Follow(_) => true,
            _ => false,
        }
    }

    fn is_locate(&self) -> bool {
        match self {
            Locate(_) => true,
            _ => false,
        }
    }
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
    pub fn new() -> Self {
        let data = ProcessData {
            cpu_stat: get_stat(&mut 0).remove(0),
            processes_times: Default::default(),
            processes_index: Default::default(),
            tree_index: Default::default(),
            parents: Default::default(),
            tree: Default::default(),
        };
        ProcessList {
            cursor: 0,
            page_movement: None,
            cpu_stat: get_stat(&mut 0).remove(0),
            data,
            processes: Vec::with_capacity(1024),
            processes_times: Default::default(),
            filter_term: None,
            height: 0,
            maxima: ColumnWidthMaxima::new(),
            freeze: false,
            draw_tree: false,
            mode: Normal,
            dirty: true,
            sort: Sort::CpuDesc,
            force_redraw: false,
        }
    }

    fn follow(&self) -> Option<Pid> {
        match self.mode {
            ProcessListMode::Follow(pid) => Some(pid),
            _ => None,
        }
    }

    fn get_pid_under_cursor(&self, cursor: usize) -> Pid {
        if self.draw_tree {
            self.data.tree[cursor].1
        } else {
            let mut processes = self.processes.iter().collect::<Vec<&ProcessDisplay>>();
            processes.sort_unstable_by(|a, b| match self.sort {
                Sort::CpuAsc => a.cpu_percent.cmp(&b.cpu_percent),
                Sort::CpuDesc => b.cpu_percent.cmp(&a.cpu_percent),
                Sort::VmRssAsc => a.vm_rss_value.cmp(&b.vm_rss_value),
                Sort::VmRssDesc => b.vm_rss_value.cmp(&a.vm_rss_value),
                Sort::UserAsc => a.username.0.cmp(&b.username.0),
                Sort::UserDesc => b.username.0.cmp(&a.username.0),
                Sort::CmdLineAsc => a.cmd_line.0.cmp(&b.cmd_line.0),
                Sort::CmdLineDesc => b.cmd_line.0.cmp(&a.cmd_line.0),
            });
            if self.filter_term.is_some() {
                processes.retain(|process| {
                    process
                        .cmd_line
                        .0
                        .contains(self.filter_term.as_ref().unwrap())
                });
            }
            processes[cursor].i
        }
    }

    fn draw_tree_list(&self, grid: &mut CellBuffer, area: Area, pages: usize, height: usize) {
        let (upper_left, bottom_right) = area;

        let mut y_offset = 0;

        let mut branches = vec![];
        let mut child_counters = vec![0];

        let mut lines = Vec::with_capacity(2048);
        let mut iter = self.data.tree.iter().peekable();
        while let Some((ind, pid)) = iter.next() {
            let p = &self.processes[self.data.processes_index[pid]];
            let has_sibling: bool = child_counters
                .get(*ind)
                .map(|n| {
                    *n != self
                        .data
                        .parents
                        .get(&p.p)
                        .and_then(|v| Some(v.len() - 1))
                        .unwrap_or(0)
                })
                .unwrap_or(false);

            let is_first: bool = child_counters.last().map(|n| *n == 0).unwrap_or(false);

            if let Some(counter) = child_counters.get_mut(*ind) {
                *counter += 1;
            }

            let mut s = String::with_capacity(16);

            for i in 0..*ind {
                if branches.len() > i && branches[i] {
                    s.push('│');
                } else {
                    s.push(' ');
                }
                if i > 0 {}
                s.push(' ');
            }

            if *ind > 0 || (has_sibling || is_first) {
                if has_sibling && is_first {
                    s.push('├');
                } else if has_sibling {
                    s.push('├');
                } else {
                    s.push('└');
                }
                s.push('─');
                s.push('>');
            }

            lines.push(s);
            match iter.peek() {
                Some((n, _)) if *n > *ind => {
                    child_counters.push(0);
                    if has_sibling {
                        branches.push(true);
                    } else {
                        branches.push(false);
                    }
                }
                Some((n, _)) if *n < *ind => {
                    for _ in 0..(ind - *n) {
                        branches.pop();
                        child_counters.pop();
                    }
                }
                _ => {}
            }
        }

        for ((_, pid), s) in self
            .data
            .tree
            .iter()
            .zip(lines.iter())
            .skip(pages * height)
            .take(height)
        {
            let (fg_color, bg_color) = if pages * height + y_offset == self.cursor {
                (Color::White, Color::Byte(235))
            } else if let Locate(highlighted_pid) = self.mode {
                if highlighted_pid == *pid {
                    (Color::Red, Color::Yellow)
                } else {
                    (Color::Default, Color::Default)
                }
            } else {
                (Color::Default, Color::Default)
            };
            let p = &self.processes[self.data.processes_index[pid]];
            match executable_path_color(&p.cmd_line) {
                Ok((path, bin, rest)) => {
                    let (x, y) = write_string_to_grid(
                            &format!(
                    "{pid:>max_pid$}  {ppid:>max_ppid$}  {username:>max_username$}  {vm_rss:>max_vm_rss$}  {cpu_percent:>max_cpu_percent$}%  {state:>max_state$}  {branches}",
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
                            branches = s,
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
                            "{pid:>max_pid$}  {ppid:>max_ppid$}  {username:>max_username$}  {vm_rss:>max_vm_rss$}  {cpu_percent:>max_cpu_percent$}%  {state:>max_state$}  {branches}",
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
                            branches = s,
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

        if self.force_redraw {
            self.force_redraw = false;
            tick = true;
        }

        if !self.dirty && !tick {
            if let Follow(ref pid) = self.mode {
                let (_, y) = write_string_to_grid(
                    &format!("Following PID == {pid} || PPID == {pid}", pid = pid),
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Bold,
                    (pos_inc(upper_left!(area), (1, 1)), bottom_right!(area)),
                    false,
                );
                dirty_areas.push_back((
                    pos_inc(upper_left!(area), (0, 1)),
                    set_y(bottom_right!(area), y),
                ));
            } else if let Locate(ref pid) = self.mode {
                let (_, y) = write_string_to_grid(
                    &format!("Highlighting PID == {pid}", pid = pid),
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Bold,
                    (pos_inc(upper_left!(area), (1, 1)), bottom_right!(area)),
                    false,
                );
                dirty_areas.push_back((
                    pos_inc(upper_left!(area), (0, 1)),
                    set_y(bottom_right!(area), y),
                ));
            }

            return;
        }

        let mut upper_left = pos_inc(upper_left!(area), (1, 0));
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
            if let Follow(ref pid) = self.mode {
                let (_, y) = write_string_to_grid(
                    &format!("Following PID == {pid} || PPID == {pid}", pid = pid),
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Bold,
                    (pos_inc(upper_left!(area), (0, 1)), bottom_right!(area)),
                    false,
                );
                dirty_areas.push_back((
                    pos_inc(upper_left!(area), (0, 1)),
                    set_y(bottom_right!(area), y),
                ));
            } else if let Locate(ref pid) = self.mode {
                let (_, y) = write_string_to_grid(
                    &format!("Highlighting PID == {pid}", pid = pid),
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Bold,
                    (pos_inc(upper_left!(area), (0, 1)), bottom_right!(area)),
                    false,
                );
                dirty_areas.push_back((
                    pos_inc(upper_left!(area), (0, 1)),
                    set_y(bottom_right!(area), y),
                ));
            }
            /* Nothing to update */
            return;
        }

        let pages = (self.cursor) / height;
        if pages != old_pages {
            tick = true;
        }

        let update_maxima = tick && !self.freeze;

        if update_maxima {
            let follow = self.follow();
            self.processes = get(&mut self.data, follow, self.sort);
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

                self.cursor = std::cmp::min(self.height.saturating_sub(1), self.cursor);
            }

            if let Follow(ref pid) = self.mode {
                write_string_to_grid(
                    &format!("Following PID == {pid} || PPID == {pid}", pid = pid),
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Bold,
                    (pos_inc(upper_left, (0, 1)), bottom_right),
                    false,
                );

                upper_left = pos_inc(upper_left, (0, 2));
            } else if let Locate(ref pid) = self.mode {
                write_string_to_grid(
                    &format!("Highlighting PID == {pid}", pid = pid),
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Bold,
                    (pos_inc(upper_left, (0, 1)), bottom_right),
                    false,
                );

                upper_left = pos_inc(upper_left, (0, 2));
            }
            let cmd_header = self
                .filter_term
                .as_ref()
                .map(|filter_term| format!("CMD_LINE (filter: {})", filter_term));

            /* Write column headers */
            let (x, y) = write_string_to_grid(
                &format!(
                    "{pid:>max_pid$}  {ppid:>max_ppid$}  {username:>max_username$}{usernamesort} {vm_rss:>max_vm_rss$}{vmrsssort} {cpu_percent:>max_cpu_percent$}{cpusort} {state:>max_state$}  {cmd_line}{cmd_linesort}",
                    pid = "PID",
                    ppid ="PPID",
                    username = "USER",
                    vm_rss = "VM_RSS",
                    cpu_percent = "  CPU%",
                    state = " ",
                    cmd_line = if let Some(ref cmd_header) = cmd_header { cmd_header } else { "CMD_LINE"} ,
                    max_pid = self.maxima.pid,
                    max_ppid = self.maxima.ppid,
                    max_username = self.maxima.username,
                    max_vm_rss = self.maxima.vm_rss,
                    max_cpu_percent = self.maxima.cpu_percent,
                    max_state = self.maxima.state,
                    usernamesort = if let Sort::UserAsc = self.sort { "↑" } else if let Sort::UserDesc = self.sort { "↓" } else { " " },
                    vmrsssort = if let Sort::VmRssAsc = self.sort { "↑" } else if let Sort::VmRssDesc = self.sort { "↓" } else { " " },
                    cpusort = if let Sort::CpuAsc = self.sort { "↑" } else if let Sort::CpuDesc = self.sort { "↓" } else { " " },
                    cmd_linesort = if let Sort::CmdLineAsc = self.sort { "↑" } else if let Sort::CmdLineDesc = self.sort { "↓" } else { "" },
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

            if self.draw_tree {
                self.draw_tree_list(grid, (upper_left, bottom_right), pages, height);
                self.height = self.data.tree.len();
                self.cursor = std::cmp::min(self.height, self.cursor);
            } else {
                let mut processes = self.processes.iter().collect::<Vec<&ProcessDisplay>>();
                processes.sort_unstable_by(|a, b| match self.sort {
                    Sort::CpuAsc => a.cpu_percent.cmp(&b.cpu_percent),
                    Sort::CpuDesc => b.cpu_percent.cmp(&a.cpu_percent),
                    Sort::VmRssAsc => a.vm_rss_value.cmp(&b.vm_rss_value),
                    Sort::VmRssDesc => b.vm_rss_value.cmp(&a.vm_rss_value),
                    Sort::UserAsc => a.username.0.cmp(&b.username.0),
                    Sort::UserDesc => b.username.0.cmp(&a.username.0),
                    Sort::CmdLineAsc => a.cmd_line.0.cmp(&b.cmd_line.0),
                    Sort::CmdLineDesc => b.cmd_line.0.cmp(&a.cmd_line.0),
                });
                if self.filter_term.is_some() {
                    processes.retain(|process| {
                        process
                            .cmd_line
                            .0
                            .contains(self.filter_term.as_ref().unwrap())
                    });
                }
                self.height = processes.len();
                self.cursor = std::cmp::min(self.height, self.cursor);

                for p in processes.iter().skip(pages * height).take(height) {
                    let (fg_color, bg_color) = if pages * height + y_offset == self.cursor {
                        (Color::White, Color::Byte(235))
                    } else if let Locate(highlighted_pid) = self.mode {
                        if highlighted_pid == p.i {
                            (Color::Red, Color::Yellow)
                        } else {
                            (Color::Default, Color::Default)
                        }
                    } else {
                        (Color::Default, Color::Default)
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
            }
        } else if old_cursor != self.cursor {
            if let Follow(ref pid) = self.mode {
                let (_, y) = write_string_to_grid(
                    &format!("Following PID == {pid} || PPID == {pid}", pid = pid),
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Bold,
                    (pos_inc(upper_left, (0, 1)), bottom_right),
                    false,
                );
                dirty_areas.push_back((pos_inc(upper_left, (0, 1)), set_y(bottom_right!(area), y)));

                upper_left = pos_inc(upper_left, (0, 2));
            } else if let Locate(ref pid) = self.mode {
                let (_, y) = write_string_to_grid(
                    &format!("Highlighting PID == {pid}", pid = pid),
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Bold,
                    (pos_inc(upper_left, (0, 1)), bottom_right),
                    false,
                );
                dirty_areas.push_back((pos_inc(upper_left, (0, 1)), set_y(bottom_right!(area), y)));

                upper_left = pos_inc(upper_left, (0, 2));
            }

            let new_area = (
                pos_inc(upper_left, (0, self.cursor + 2 - pages * height)),
                set_y(
                    bottom_right,
                    get_y(upper_left) + self.cursor + 2 - pages * height,
                ),
            );
            let old_pid = self.get_pid_under_cursor(old_cursor);
            change_colors(grid, new_area, None, Some(Color::Byte(235)));
            let bg_color = if let Locate(highlighted_pid) = self.mode {
                if highlighted_pid == old_pid {
                    Color::Yellow
                } else {
                    Color::Default
                }
            } else {
                Color::Default
            };
            let old_area = (
                pos_inc(upper_left, (0, old_cursor + 2 - old_pages * height)),
                set_y(
                    bottom_right,
                    get_y(upper_left) + old_cursor + 2 - old_pages * height,
                ),
            );
            change_colors(grid, old_area, None, Some(bg_color));
            dirty_areas.push_back(old_area);
            dirty_areas.push_back(new_area);
        } else if let Follow(ref pid) = self.mode {
            let (_, y) = write_string_to_grid(
                &format!("Following PID == {pid} || PPID == {pid}", pid = pid),
                grid,
                Color::Default,
                Color::Default,
                Attr::Bold,
                (pos_inc(upper_left, (0, 1)), bottom_right),
                false,
            );
            dirty_areas.push_back((pos_inc(upper_left, (0, 1)), set_y(bottom_right, y)));
        } else if let Locate(ref pid) = self.mode {
            let (_, y) = write_string_to_grid(
                &format!("Highlighting PID == {pid}", pid = pid),
                grid,
                Color::Default,
                Color::Default,
                Attr::Bold,
                (pos_inc(upper_left, (0, 1)), bottom_right),
                false,
            );
            dirty_areas.push_back((pos_inc(upper_left, (0, 1)), set_y(bottom_right, y)));
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
            let pid = self.get_pid_under_cursor(self.cursor);
            write_string_to_grid(
                &format!(
                    "{cmd_line} [{pid}]",
                    pid = pid,
                    cmd_line = &self.processes[self.data.processes_index[&pid]].cmd_line.0[0
                        ..std::cmp::min(
                            26,
                            self.processes[self.data.processes_index[&pid]]
                                .cmd_line
                                .len()
                        )],
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
            UIEvent::Input(Key::F(f)) => {
                use Sort::*;
                self.sort = match (self.sort, f) {
                    (Sort::UserAsc, 1) => UserDesc,
                    (Sort::UserDesc, 1) | (_, 1) => UserAsc,
                    (Sort::VmRssDesc, 2) => VmRssAsc,
                    (Sort::VmRssAsc, 2) | (_, 2) => VmRssDesc,
                    (Sort::CpuDesc, 3) => CpuAsc,
                    (Sort::CpuAsc, 3) | (_, 3) => CpuDesc,
                    (Sort::CmdLineDesc, 4) => CmdLineAsc,
                    (Sort::CmdLineAsc, 4) | (_, 4) => CmdLineDesc,
                    _ => return,
                };

                self.force_redraw = true;
                self.dirty = true;
            }
            UIEvent::Input(Key::Char(c)) if self.filter_term.is_some() && self.mode == Normal => {
                if let Some(ref mut filter_term) = self.filter_term {
                    if !c.is_ascii_control() {
                        filter_term.push(*c);
                        self.force_redraw = true;
                        self.dirty = true;
                    }
                }
            }
            UIEvent::Input(k) if *k == map["filter"] && self.mode.is_normal() => {
                self.filter_term = Some(String::new());
                self.draw_tree = false;
                self.force_redraw = true;
                self.dirty = true;
            }
            UIEvent::Input(k) if *k == map["follow process group"] && !self.mode.is_follow() => {
                let pid = self.get_pid_under_cursor(self.cursor);
                self.mode = Follow(pid);
                self.freeze = false;
                self.force_redraw = true;
                self.dirty = true;
            }
            UIEvent::Input(k)
                if *k == map["freeze updates"]
                    && (self.mode.is_normal()
                        || self.mode.is_follow()
                        || self.mode.is_locate()) =>
            {
                self.freeze = !self.freeze;
                self.force_redraw = true;
                self.dirty = true;
            }
            UIEvent::Input(k) if *k == map["locate process by pid"] && self.mode.is_normal() => {
                self.mode = Locate(0);
                self.freeze = true;
                self.dirty = true;
                self.force_redraw = true;
            }
            UIEvent::Input(k) if *k == map["kill process"] => {
                self.mode = Kill(0);
                self.freeze = true;
                self.dirty = true;
                self.force_redraw = true;
            }
            UIEvent::Input(k) if *k == map["cancel"] => {
                /* layered cancelling */
                if self.mode != Normal {
                    self.mode = Normal;
                } else if self.filter_term.is_some() {
                    self.filter_term = None;
                } else {
                    self.freeze = false;
                }
                self.force_redraw = true;
                self.dirty = true;
            }
            UIEvent::Input(k)
                if *k == map["toggle tree view"]
                    && !(self.draw_tree && self.filter_term.is_some()) =>
            {
                self.draw_tree = !self.draw_tree;
                self.force_redraw = true;
                self.dirty = true;
            }
            UIEvent::Input(Key::Char(f)) if self.mode != Normal && f.is_numeric() => {
                if let Kill(ref mut n) = self.mode {
                    if let Some(add) = (*n).checked_mul(10) {
                        *n = add
                            .checked_add(f.to_digit(10).unwrap() as u16)
                            .unwrap_or(*n);
                    }
                } else if let Locate(ref mut p) = self.mode {
                    if let Some(add) = (*p).checked_mul(10) {
                        *p = add
                            .checked_add(f.to_digit(10).unwrap() as i32)
                            .unwrap_or(*p);
                    }
                    self.dirty = true;
                }
            }
            UIEvent::Input(Key::Backspace) if self.mode != Normal => {
                if let Kill(ref mut n) = self.mode {
                    *n = *n / 10;
                } else if let Locate(ref mut p) = self.mode {
                    *p = *p / 10;
                }
                self.dirty = true;
                self.force_redraw = true;
            }
            UIEvent::Input(Key::Backspace) if self.filter_term.is_some() => {
                let filter_term = self.filter_term.as_mut();
                if filter_term.as_ref().unwrap().is_empty() {
                    self.filter_term = None;
                } else {
                    // TODO pop grapheme
                    filter_term.unwrap().pop();
                }
                self.dirty = true;
                self.force_redraw = true;
            }
            UIEvent::Input(Key::Char('\n')) if self.mode != Normal => {
                if let Kill(ref n) = self.mode {
                    use nix::sys::signal::kill;

                    kill(
                        nix::unistd::Pid::from_raw(self.get_pid_under_cursor(self.cursor)),
                        nix::sys::signal::Signal::from_c_int(*n as i32).unwrap(),
                    )
                    .ok()
                    .take();
                    self.mode = Normal;
                    self.dirty = true;
                    self.force_redraw = true;
                }
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
        map.insert("follow process group", Key::Char('F'));
        map.insert("locate process by pid", Key::Char('L'));
        map.insert("freeze updates", Key::Char('f'));
        map.insert("toggle tree view", Key::Char('t'));
        map.insert("kill process", Key::Char('k'));
        map.insert("filter", Key::Char('/'));
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

fn get(data: &mut ProcessData, follow_pid: Option<Pid>, sort: Sort) -> Vec<ProcessDisplay> {
    data.tree.clear();
    data.parents.clear();
    data.processes_index.clear();
    data.tree_index.clear();

    let ProcessData {
        ref mut parents,
        ref mut processes_index,
        ref mut tree_index,
        ref mut tree,
        ref mut processes_times,
        cpu_stat: ref mut data_cpu_stat,
    } = data;

    let mut processes = Vec::with_capacity(2048);
    let cpu_stat = get_stat(&mut 0).remove(0);
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

        let mut process_display = ProcessDisplay {
            i: process.pid,
            p: process.ppid,
            pid: PidString(process.pid.to_string()),
            ppid: PpidString(process.ppid.to_string()),
            vm_rss: VmRssString(Bytes(process.vm_rss * 1024).as_convenient_string()),
            vm_rss_value: process.vm_rss * 1024,
            cpu_percent: (100.0
                * ((process.utime
                    - processes_times
                        .get(&process.pid)
                        .map(|v| *v)
                        .unwrap_or(process.utime)) as f64
                    / ((cpu_stat.total_time() - data_cpu_stat.total_time()) as f64)))
                as usize,
            utime: process.utime,
            state: process.state,
            cmd_line: CmdLineString(process.cmd_line),
            username: UserString(crate::ui::username(process.uid)),
        };
        if process_display.cpu_percent > 100 {
            process_display.cpu_percent = 0;
        }

        processes_times.insert(process.pid, process_display.utime);
        parents.entry(process.ppid).or_default().push(process.pid);

        processes_index.insert(process.pid, processes.len());
        processes.push(process_display);
    }
    *data_cpu_stat = cpu_stat;
    let mut keep_list = HashSet::new();
    if follow_pid.is_some() && processes_index.contains_key(follow_pid.as_ref().unwrap()) {
        let mut stack = Vec::with_capacity(32);
        let p = &processes[processes_index[&follow_pid.unwrap()]];
        keep_list.insert(p.i);
        if let Some(children) = parents.get(&p.i) {
            stack.extend(children.iter().cloned());
        }
        while let Some(pid) = stack.pop() {
            keep_list.insert(pid);
            if let Some(children) = parents.get(&pid) {
                stack.extend(children.iter().cloned());
            }
        }
        stack.push(p.p);
        while let Some(pid) = stack.pop() {
            if pid == 0 {
                continue;
            }
            keep_list.insert(pid);
            if processes[processes_index[&pid]].p != 0 {
                stack.push(processes[processes_index[&pid]].p);
            }
        }
        processes_index.clear();
        processes.retain(|entry| keep_list.contains(&entry.i));
        for (i, p) in processes.iter().enumerate() {
            processes_index.insert(p.i, i);
        }
    }

    let mut stack = Vec::with_capacity(processes.len());
    stack.push((0, 1));
    while let Some((ind, pid)) = stack.pop() {
        tree_index.insert(pid, tree.len());
        tree.push((ind, pid));
        if let Some(children) = parents.get_mut(&pid) {
            if !keep_list.is_empty() {
                children.retain(|c| keep_list.contains(c));
            }
            children.sort_unstable_by(|a, b| {
                let a = &processes[processes_index[a]];
                let b = &processes[processes_index[b]];
                match sort {
                    Sort::CpuAsc => a.cpu_percent.cmp(&b.cpu_percent),
                    Sort::CpuDesc => b.cpu_percent.cmp(&a.cpu_percent),
                    Sort::VmRssAsc => a.vm_rss_value.cmp(&b.vm_rss_value),
                    Sort::VmRssDesc => b.vm_rss_value.cmp(&a.vm_rss_value),
                    Sort::UserAsc => a.username.0.cmp(&b.username.0),
                    Sort::UserDesc => b.username.0.cmp(&a.username.0),
                    Sort::CmdLineAsc => a.cmd_line.0.cmp(&b.cmd_line.0),
                    Sort::CmdLineDesc => b.cmd_line.0.cmp(&a.cmd_line.0),
                }
            });
            stack.extend(children.iter().map(|p| (ind + 1, *p)).rev());
        }
    }
    processes
}

/* Might return Error if process has disappeared
 * during the function's run */
fn get_pid_info(mut path: PathBuf) -> Result<Process, std::io::Error> {
    /* proc file structure can be found in man 5 proc.*/
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

    macro_rules! err {
        ($res:expr) => {
            match $res {
                Ok(v) => v,
                Err(_) => {
                    return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, ""));
                }
            }
        };
    }

    macro_rules! none_err {
        ($res:expr) => {
            if let Some(v) = $res {
                v
            } else {
                return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, ""));
            }
        };
    }

    let mut b = 0;
    while b < 5 {
        let line_opt = lines_iter.next();
        if line_opt.is_none() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("{} returned malformed input", path.display()),
            ));
        }
        line = none_err!(line_opt);
        let mut mut_value_iter = line.split_whitespace();
        match mut_value_iter.next() {
            Some("VmRSS:") => {
                ret.vm_rss = err!(usize::from_str(none_err!(mut_value_iter.next())));
                b += 1;
            }
            Some("State:") => {
                ret.state = State::from(none_err!(none_err!(mut_value_iter.next()).chars().next()));
                b += 1;
            }
            Some("Pid:") => {
                ret.pid = err!(i32::from_str(none_err!(mut_value_iter.next())));
                b += 1;
            }
            Some("PPid:") => {
                ret.ppid = err!(i32::from_str(none_err!(mut_value_iter.next())));
                b += 1;
            }
            Some("Uid:") => {
                ret.uid = err!(u32::from_str(none_err!(mut_value_iter.next())));
                b += 1;
            }
            None => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!(
                        "{} returned malformed input. Original error was while parsing file",
                        path.display(),
                    ),
                ));
            }
            _ => {}
        }
    }

    path.pop();
    path.push("cmdline");
    let mut file: File = File::open(&path)?;
    res.clear();
    file.read_to_string(&mut res)?;
    if !res.is_empty() {
        /* values are separated by null bytes */
        ret.cmd_line = format!("{}", res.split('\0').collect::<Vec<&str>>().join(" "));
    }
    path.pop();
    path.push("stat");
    let mut file: File = File::open(&path)?;
    res.clear();
    file.read_to_string(&mut res)?;
    /* values are separated by whitespace and are in a specific order */
    if !res.is_empty() {
        let mut vals = res.split_whitespace().skip(13);
        ret.utime = err!(usize::from_str(none_err!(vals.next())));
        ret.utime += err!(usize::from_str(none_err!(vals.next())));
        ret.utime += err!(usize::from_str(none_err!(vals.next())));
        ret.utime += err!(usize::from_str(none_err!(vals.next())));
    }
    Ok(ret)
}
