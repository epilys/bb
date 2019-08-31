/*
 * meli - ui crate.
 *
 * Copyright 2017-2018 Manos Pitsidianakis
 *
 * This file is part of meli.
 *
 * meli is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * meli is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with meli. If not, see <http://www.gnu.org/licenses/>.
 */

/*! Various useful components that can be used in a generic fashion.
 */
use super::*;
use std::fs::File;
use std::io::prelude::*;
use std::str::FromStr;

mod widgets;

pub use self::widgets::*;

/// A horizontally split in half container.
#[derive(Debug)]
pub struct HSplit {
    top: Box<Component>,
    bottom: Box<Component>,
    show_divider: bool,
    ratio: usize, // bottom/whole height * 100
}

impl fmt::Display for HSplit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display subject/info
        Display::fmt(&self.top, f)
    }
}

impl HSplit {
    pub fn new(
        top: Box<Component>,
        bottom: Box<Component>,
        ratio: usize,
        show_divider: bool,
    ) -> Self {
        HSplit {
            top,
            bottom,
            show_divider,
            ratio,
        }
    }
}

impl Component for HSplit {
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
        let bottom_component_height = (self.ratio * total_rows) / 100;
        let mid = get_y(upper_left) + total_rows - bottom_component_height;

        if self.show_divider {
            for i in get_x(upper_left)..=get_x(bottom_right) {
                grid[(i, mid)].set_ch('─');
            }
            dirty_areas.push_back(((get_x(upper_left), mid), (get_x(bottom_right), mid)));
        }

        self.top.draw(
            grid,
            (
                upper_left,
                (get_x(bottom_right), get_y(upper_left) + mid - 1),
            ),
            dirty_areas,
            tick,
        );
        self.bottom.draw(
            grid,
            ((get_x(upper_left), get_y(upper_left) + mid), bottom_right),
            dirty_areas,
            tick,
        );
    }

    fn process_event(&mut self, event: &mut UIEvent) {
        self.top.process_event(event);
        self.bottom.process_event(event);
    }

    fn is_dirty(&self) -> bool {
        self.top.is_dirty() || self.bottom.is_dirty()
    }

    fn set_dirty(&mut self) {
        self.top.set_dirty();
        self.bottom.set_dirty();
    }

    fn get_shortcuts(&self) -> ShortcutMaps {
        let mut top_map = self.top.get_shortcuts();
        top_map.extend(self.bottom.get_shortcuts().into_iter());
        top_map
    }
}

/// A vertically split in half container.
#[derive(Debug)]
pub struct VSplit {
    left: Box<Component>,
    right: Box<Component>,
    show_divider: bool,
    /// This is the width of the right container to the entire width.
    ratio: usize, // right/(container width) * 100
}

impl fmt::Display for VSplit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display focused component
        Display::fmt(&self.right, f)
    }
}

impl VSplit {
    pub fn new(
        left: Box<Component>,
        right: Box<Component>,
        ratio: usize,
        show_divider: bool,
    ) -> Self {
        VSplit {
            left,
            right,
            show_divider,
            ratio,
        }
    }
}

impl Component for VSplit {
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
        let total_cols = get_x(bottom_right) - get_x(upper_left);
        let right_component_width = (self.ratio * total_cols) / 100;

        let mid = get_x(bottom_right) - right_component_width;

        if get_y(upper_left) > 1 {
            let c = grid
                .get(mid, get_y(upper_left) - 1)
                .map(Cell::ch)
                .unwrap_or_else(|| ' ');
            if let HORZ_BOUNDARY = c {
                grid[(mid, get_y(upper_left) - 1)].set_ch(LIGHT_DOWN_AND_HORIZONTAL);
            }
        }

        if self.show_divider && mid != get_x(upper_left) {
            for i in get_y(upper_left)..=get_y(bottom_right) {
                grid[(mid, i)].set_ch(VERT_BOUNDARY);
                grid[(mid, i)].set_fg(Color::Default);
                grid[(mid, i)].set_bg(Color::Default);
            }
            if get_y(bottom_right) > 1 {
                let c = grid
                    .get(mid, get_y(bottom_right) - 1)
                    .map(Cell::ch)
                    .unwrap_or_else(|| ' ');
                if let HORZ_BOUNDARY = c {
                    grid[(mid, get_y(bottom_right) + 1)].set_ch(LIGHT_UP_AND_HORIZONTAL);
                }
            }
            dirty_areas.push_back(((mid, get_y(upper_left)), (mid, get_y(bottom_right))));
        }

        if right_component_width == total_cols {
            self.right.draw(grid, area, dirty_areas, tick);
        } else if right_component_width == 0 {
            self.left.draw(grid, area, dirty_areas, tick);
        } else {
            self.left.draw(
                grid,
                (
                    upper_left,
                    (
                        if self.show_divider { mid - 1 } else { mid },
                        get_y(bottom_right),
                    ),
                ),
                dirty_areas,
                tick,
            );
            self.right.draw(
                grid,
                (set_x(upper_left, mid + 1), bottom_right),
                dirty_areas,
                tick,
            );
        }
    }

    fn process_event(&mut self, event: &mut UIEvent) {
        self.left.process_event(event);
        self.right.process_event(event);
    }

    fn is_dirty(&self) -> bool {
        self.left.is_dirty() || self.right.is_dirty()
    }

    fn set_dirty(&mut self) {
        self.left.set_dirty();
        self.right.set_dirty();
    }

    fn get_shortcuts(&self) -> ShortcutMaps {
        let mut right_map = self.right.get_shortcuts();
        right_map.extend(self.left.get_shortcuts().into_iter());
        right_map
    }
}

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
    pub system_time: usize,
    pub nice_time: usize,
    pub idle_time: usize,
    pub iowait_time: usize,
}

impl Stat {
    pub fn total_time(&self) -> usize {
        self.user_time + self.system_time + self.nice_time + self.idle_time + self.iowait_time
    }
}

pub fn get_stat(boot_time: &mut usize) -> Vec<Stat> {
    let mut file = File::open("/proc/stat").unwrap();
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

        let user_time = usize::from_str(&mut_value_iter.next().unwrap()).unwrap();
        /* skip nice time */
        let nice_time = usize::from_str(&mut_value_iter.next().unwrap()).unwrap();
        let system_time = usize::from_str(&mut_value_iter.next().unwrap()).unwrap();
        let idle_time = usize::from_str(&mut_value_iter.next().unwrap()).unwrap();
        let iowait_time = usize::from_str(&mut_value_iter.next().unwrap()).unwrap();
        ret.push(Stat {
            user_time,
            system_time,
            nice_time,
            idle_time,
            iowait_time,
        });
    }
    while !line.starts_with("btime") {
        line = lines_iter.next().unwrap();
    }
    *boot_time = usize::from_str(&line.split_whitespace().skip(1).next().unwrap()).unwrap();

    ret
}
