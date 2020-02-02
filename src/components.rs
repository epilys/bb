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

/*!
Components are ways to handle application data. They can draw on the terminal and receive events, but also do other stuff as well. (For example, see the `notifications` module.)

See the `Component` Trait for more details.
*/

use crate::state::*;
use crate::terminal::*;
mod utilities;
pub use utilities::*;

mod kernel;
pub use kernel::*;
pub mod processes;
pub use processes::*;

use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::fmt::{Debug, Display};

use super::{Key, UIEvent};
// The upper and lower boundary char.  const HORZ_BOUNDARY: char = '─';
/// The left and right boundary char.  const VERT_BOUNDARY: char = '│';

pub type ShortcutMap = HashMap<&'static str, Key>;
pub type ShortcutMaps = HashMap<String, ShortcutMap>;

/// Types implementing this Trait can draw on the terminal and receive events.
/// If a type wants to skip drawing if it has not changed anything, it can hold some flag in its
/// fields (eg self.dirty = false) and act upon that in their `draw` implementation.
pub trait Component: Display + Debug + Send {
    fn draw(
        &mut self,
        grid: &mut CellBuffer,
        area: Area,
        dirty_areas: &mut VecDeque<Area>,
        tick: bool,
    );
    fn process_event(&mut self, event: &mut UIEvent, ui_mode: &mut UIMode);
    fn is_dirty(&self) -> bool {
        true
    }
    fn set_dirty(&mut self);
    fn get_shortcuts(&self) -> ShortcutMaps {
        Default::default()
    }
}

pub fn create_box(grid: &mut CellBuffer, area: Area) {
    if !is_valid_area!(area) {
        return;
    }
    let upper_left = upper_left!(area);
    let bottom_right = bottom_right!(area);

    /*for x in get_x(upper_left)..=get_x(bottom_right) {
        grid[(x, get_y(upper_left))].set_ch(HORZ_BOUNDARY);
        grid[(x, get_y(bottom_right))].set_ch(HORZ_BOUNDARY);
        grid[(x, get_y(bottom_right))].set_ch('▒');
        grid[(x, get_y(bottom_right))].set_fg(Color::Byte(240));
    }
    */

    for y in get_y(upper_left)..=get_y(bottom_right) {
        //grid[(get_x(upper_left), y)].set_ch(VERT_BOUNDARY);
        grid[(get_x(bottom_right), y)]
            .set_ch('▒')
            .set_fg(Color::Byte(240));
    }
}
