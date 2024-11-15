// bb
//
// Copyright 2019 Manos Pitsidianakis
//
// This file is part of bb.
//
// bb is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// bb is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with bb. If not, see <http://www.gnu.org/licenses/>.

//! Various useful utilities

extern crate cassowary;
use std::{fs::File, io::prelude::*, str::FromStr};

use cassowary::{
    strength::{REQUIRED, STRONG, WEAK},
    Solver, Variable,
    WeightedRelation::*,
};

use super::*;

#[derive(Debug)]
pub enum PageMovement {
    Up,
    Down,
    Home,
    PageUp,
    PageDown,
    End,
}

const KILOBYTE: f64 = 1024.0;
const MEGABYTE: f64 = KILOBYTE * 1024.0;
const GIGABYTE: f64 = MEGABYTE * 1024.0;
const PETABYTE: f64 = GIGABYTE * 1024.0;

pub struct Bytes(pub usize);

impl Bytes {
    pub fn as_convenient_string(&self) -> String {
        let bytes = self.0 as f64;
        if bytes == 0.0 {
            "0".to_string()
        } else if bytes < KILOBYTE {
            format!("{:.2} bytes", bytes)
        } else if bytes < MEGABYTE {
            format!("{:.2} KiB", bytes / KILOBYTE)
        } else if bytes < GIGABYTE {
            format!("{:.2} MiB", bytes / MEGABYTE)
        } else if bytes < PETABYTE {
            format!("{:.2} GiB", bytes / GIGABYTE)
        } else {
            format!("{:.2} PiB", bytes / PETABYTE)
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Stat {
    pub user_time: usize,
    pub nice_time: usize,
    pub system_time: usize,
    pub idle_time: usize,
    pub iowait_time: usize,
    pub irq: usize,
    pub soft_irq: usize,
    pub steal: usize,
    pub guest: usize,
    pub guest_nice: usize,
}

impl Stat {
    #[inline(always)]
    pub fn total_time(&self) -> usize {
        self.user_time
            + self.system_time
            + self.irq
            + self.soft_irq
            + self.nice_time
            + self.idle_time
            + self.iowait_time
            + self.guest
            + self.guest_nice
            + self.steal
    }

    #[inline(always)]
    pub fn busy_time(&self) -> usize {
        self.user_time
            + self.system_time
            + self.irq
            + self.soft_irq
            + self.nice_time
            + self.iowait_time
            + self.guest
            + self.guest_nice
            + self.steal
    }
}

pub fn get_stat(boot_time: &mut usize) -> Vec<Stat> {
    let mut file = File::open("/proc/stat").expect(crate::PROC_FS_ERROR_STR);
    let mut res = String::with_capacity(2048);
    file.read_to_string(&mut res).unwrap();
    let mut lines_iter = res.lines();
    let mut ret = Vec::with_capacity(8);
    let mut line;
    loop {
        line = lines_iter.next().unwrap();
        if !line.starts_with("cpu") {
            break;
        }

        let mut mut_value_iter = line.split_whitespace().skip(1);

        let user_time = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
        let nice_time = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
        let system_time = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
        let idle_time = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
        let iowait_time = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
        let irq = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
        let soft_irq = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
        let steal = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
        let guest = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
        let guest_nice = usize::from_str(mut_value_iter.next().unwrap()).unwrap();
        ret.push(Stat {
            user_time,
            system_time,
            nice_time,
            idle_time,
            iowait_time,
            irq,
            soft_irq,
            steal,
            guest,
            guest_nice,
        });
    }
    while !line.starts_with("btime") {
        line = lines_iter.next().unwrap();
    }
    *boot_time = usize::from_str(line.split_whitespace().nth(1).unwrap()).unwrap();

    ret
}

/// A horizontally split in half container.
#[derive(Debug)]
pub struct Window {
    top_bars: Box<dyn Component>,
    list: Box<dyn Component>,
}

impl fmt::Display for Window {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.top_bars, f)
    }
}

impl Window {
    pub fn new(top_bars: Box<dyn Component>, list: Box<dyn Component>) -> Self {
        Window { top_bars, list }
    }
}

struct Element {
    top: Variable,
    bottom: Variable,
}
impl Component for Window {
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
        let total_rows = get_y(bottom_right) - get_y(upper_left);
        let window_height = Variable::new();

        let top_bars = Element {
            top: Variable::new(),
            bottom: Variable::new(),
        };

        let list = Element {
            top: Variable::new(),
            bottom: Variable::new(),
        };

        let mut solver = Solver::new();
        solver
            .add_constraints(&[
                window_height | GE(REQUIRED) | 0.0, // positive window height
                top_bars.top | EQ(REQUIRED) | 0.0,  // top align
                list.bottom | EQ(REQUIRED) | window_height, // right align
                list.top | GE(REQUIRED) | top_bars.bottom, // no overlap
                // positive heights
                top_bars.top | LE(REQUIRED) | top_bars.bottom,
                list.top | LE(REQUIRED) | list.bottom,
                // preferred heights:
                (top_bars.bottom - top_bars.top) | GE(REQUIRED) | 6.0,
                (top_bars.bottom - top_bars.top) | EQ(WEAK) | 8.0,
                (top_bars.bottom - top_bars.top) | LE(REQUIRED) | 8.0,
                (list.bottom - list.top) | GE(REQUIRED) | 11.0,
            ])
            .unwrap();

        solver.add_edit_variable(window_height, STRONG).unwrap();
        solver
            .suggest_value(window_height, total_rows as f64)
            .unwrap();

        let changes = solver.fetch_changes();
        let mid = get_y(upper_left)
            + (*changes
                .iter()
                .find(|(a, _)| *a == top_bars.bottom)
                .map(|(_, b)| b)
                .unwrap() as usize);
        self.top_bars.draw(
            grid,
            (
                upper_left,
                (get_x(bottom_right), get_y(upper_left) + mid - 1),
            ),
            dirty_areas,
            tick,
        );
        self.list.draw(
            grid,
            ((get_x(upper_left), get_y(upper_left) + mid), bottom_right),
            dirty_areas,
            tick,
        );
    }

    fn process_event(&mut self, event: &mut UIEvent, ui_mode: &mut UIMode) {
        self.top_bars.process_event(event, ui_mode);
        self.list.process_event(event, ui_mode);
    }

    fn is_dirty(&self) -> bool {
        self.top_bars.is_dirty() || self.list.is_dirty()
    }

    fn set_dirty(&mut self) {
        self.top_bars.set_dirty();
        self.list.set_dirty();
    }

    fn get_shortcuts(&self) -> ShortcutMaps {
        let mut top_bars_map = self.top_bars.get_shortcuts();
        top_bars_map.extend(self.list.get_shortcuts());
        top_bars_map
    }
}
