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
Simple type definitions and macro helper for a (x,y) position on the terminal and the areas they define.

An `Area` consists of two points: the upper left and bottom right corners.
*/

/// A `(x, y)` position on screen.
pub type Pos = (usize, usize);

#[inline(always)]
pub fn get_x(p: Pos) -> usize {
    p.0
}
#[inline(always)]
pub fn get_y(p: Pos) -> usize {
    p.1
}
#[inline(always)]
pub fn set_x(p: Pos, new_x: usize) -> Pos {
    (new_x, p.1)
}
#[inline(always)]
pub fn set_y(p: Pos, new_y: usize) -> Pos {
    (p.0, new_y)
}
#[inline(always)]
pub fn pos_inc(p: Pos, inc: (usize, usize)) -> Pos {
    (p.0 + inc.0, p.1 + inc.1)
}

#[inline(always)]
pub fn pos_dec(p: Pos, dec: (usize, usize)) -> Pos {
    (p.0.saturating_sub(dec.0), p.1.saturating_sub(dec.1))
}

pub type Area = (Pos, Pos);
#[macro_export]
macro_rules! height {
    ($a:expr) => {
        (get_y(bottom_right!($a))).saturating_sub(get_y(upper_left!($a)))
    };
}

#[macro_export]
macro_rules! width {
    ($a:expr) => {
        (get_x(bottom_right!($a))).saturating_sub(get_x(upper_left!($a)))
    };
}

#[macro_export]
macro_rules! upper_left {
    ($a:expr) => {
        $a.0
    };
}

#[macro_export]
macro_rules! bottom_right {
    ($a:expr) => {
        $a.1
    };
}

#[macro_export]
macro_rules! is_valid_area {
    ($a:expr) => {{
        let upper_left = upper_left!($a);
        let bottom_right = bottom_right!($a);
        !(get_y(upper_left) > get_y(bottom_right) || get_x(upper_left) > get_x(bottom_right))
    }};
}
