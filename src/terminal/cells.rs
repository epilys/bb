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

//! Define a (x, y) point in the terminal display as a holder of a character,
//! foreground/background colors and attributes.

use std::{
    convert::From,
    fmt,
    ops::{Deref, DerefMut, Index, IndexMut},
};

use termion::color::{AnsiValue, Rgb as TermionRgb};

use super::position::*;
use crate::text_processing::wcwidth;

/// In a scroll region up and down cursor movements shift the region vertically.
/// The new lines are empty.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ScrollRegion {
    pub top: usize,
    pub bottom: usize,
    pub left: usize,
    pub right: usize,
}

/// An array of `Cell`s that represents a terminal display.
///
/// A `CellBuffer` is a two-dimensional array of `Cell`s, each pair of indices
/// correspond to a single point on the underlying terminal.
///
/// The first index, `Cellbuffer[y]`, corresponds to a row, and thus the y-axis.
/// The second index, `Cellbuffer[y][x]`, corresponds to a column within a row
/// and thus the x-axis.
#[derive(Clone, PartialEq, Eq)]
pub struct CellBuffer {
    cols: usize,
    rows: usize,
    buf: Vec<Cell>,
}

impl fmt::Debug for CellBuffer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "CellBuffer {{ cols: {}, rows: {}, buf: {} cells",
            self.cols,
            self.rows,
            self.buf.len()
        )
    }
}

impl CellBuffer {
    pub fn area(&self) -> Area {
        (
            (0, 0),
            (self.cols.saturating_sub(1), self.rows.saturating_sub(1)),
        )
    }
    pub fn set_cols(&mut self, new_cols: usize) {
        self.cols = new_cols;
    }

    /// Constructs a new `CellBuffer` with the given number of columns and rows,
    /// using the given `cell` as a blank.
    pub fn new(cols: usize, rows: usize, cell: Cell) -> CellBuffer {
        CellBuffer {
            cols,
            rows,
            buf: vec![cell; cols * rows],
        }
    }

    /// Resizes `CellBuffer` to the given number of rows and columns, using the
    /// given `Cell` as a blank.
    pub fn resize(&mut self, newcols: usize, newrows: usize, blank: Cell) {
        let newlen = newcols * newrows;
        if self.buf.len() == newlen {
            self.cols = newcols;
            self.rows = newrows;
            return;
        }

        if newlen >= 200_000 {
            return;
        }

        let mut newbuf: Vec<Cell> = Vec::with_capacity(newlen);
        for y in 0..newrows {
            for x in 0..newcols {
                let cell = self.get(x, y).unwrap_or(&blank);
                newbuf.push(*cell);
            }
        }
        self.buf = newbuf;
        self.cols = newcols;
        self.rows = newrows;
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    pub fn empty(&mut self) {
        self.buf.clear();
        self.cols = 0;
        self.rows = 0;
    }

    /// Clears `self`, using the given `Cell` as a blank.
    pub fn clear(&mut self, blank: Cell) {
        for cell in self.cellvec_mut().iter_mut() {
            *cell = blank;
        }
    }

    pub fn pos_to_index(&self, x: usize, y: usize) -> Option<usize> {
        let (cols, rows) = self.size();
        if x < cols && y < rows {
            Some((cols * y) + x)
        } else {
            None
        }
    }

    /// Returns a reference to the `Cell` at the given coordinates, or `None` if
    /// the index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```norun
    /// let mut term = Terminal::new().unwrap();
    ///
    /// let a_cell = term.get(5, 5);
    /// ```
    pub fn get(&self, x: usize, y: usize) -> Option<&Cell> {
        match self.pos_to_index(x, y) {
            Some(i) => self.cellvec().get(i),
            None => None,
        }
    }

    /// Returns a mutable reference to the `Cell` at the given coordinates, or
    /// `None` if the index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```norun
    /// let mut term = Terminal::new().unwrap();
    ///
    /// let a_mut_cell = term.get_mut(5, 5);
    /// ```
    pub fn get_mut(&mut self, x: usize, y: usize) -> Option<&mut Cell> {
        match self.pos_to_index(x, y) {
            Some(i) => self.cellvec_mut().get_mut(i),
            None => None,
        }
    }

    pub fn size(&self) -> (usize, usize) {
        (self.cols, self.rows)
    }

    pub fn cellvec(&self) -> &Vec<Cell> {
        &self.buf
    }

    pub fn cellvec_mut(&mut self) -> &mut Vec<Cell> {
        &mut self.buf
    }

    pub fn cols(&self) -> usize {
        self.size().0
    }

    pub fn rows(&self) -> usize {
        self.size().1
    }

    #[inline(always)]
    /// Performs the normal scroll up motion:
    ///
    /// First clear offset number of lines:
    ///
    /// For offset = 1, top = 1:
    ///
    ///  ```text
    ///  | 111111111111 |            |              |
    ///  | 222222222222 |            | 222222222222 |
    ///  | 333333333333 |            | 333333333333 |
    ///  | 444444444444 |    -->     | 444444444444 |
    ///  | 555555555555 |            | 555555555555 |
    ///  | 666666666666 |            | 666666666666 |
    ///  ```
    ///
    ///  In each step, swap the current line with the next by offset:
    ///
    ///  ```text
    ///  |              |            | 222222222222 |
    ///  | 222222222222 |            |              |
    ///  | 333333333333 |            | 333333333333 |
    ///  | 444444444444 |    -->     | 444444444444 |
    ///  | 555555555555 |            | 555555555555 |
    ///  | 666666666666 |            | 666666666666 |
    ///  ```
    ///
    ///  Result:
    ///  ```text
    ///    Before                      After
    ///  | 111111111111 |            | 222222222222 |
    ///  | 222222222222 |            | 333333333333 |
    ///  | 333333333333 |            | 444444444444 |
    ///  | 444444444444 |            | 555555555555 |
    ///  | 555555555555 |            | 666666666666 |
    ///  | 666666666666 |            |              |
    ///  ```
    pub fn scroll_up(&mut self, scroll_region: &ScrollRegion, top: usize, offset: usize) {
        let l = scroll_region.left;
        let r = if scroll_region.right == 0 {
            self.size().0
        } else {
            scroll_region.right
        };
        for y in top..=(top + offset - 1) {
            for x in l..r {
                self[(x, y)] = Cell::default();
            }
        }
        for y in top..=(scroll_region.bottom - offset) {
            for x in l..r {
                let temp = self[(x, y)];
                self[(x, y)] = self[(x, y + offset)];
                self[(x, y + offset)] = temp;
            }
        }
    }

    #[inline(always)]
    /// Performs the normal scroll down motion:
    ///
    /// First clear offset number of lines:
    ///
    /// For offset = 1, top = 1:
    ///
    ///  ```text
    ///  | 111111111111 |            | 111111111111 |
    ///  | 222222222222 |            | 222222222222 |
    ///  | 333333333333 |            | 333333333333 |
    ///  | 444444444444 |    -->     | 444444444444 |
    ///  | 555555555555 |            | 555555555555 |
    ///  | 666666666666 |            |              |
    ///  ```
    ///
    ///  In each step, swap the current line with the prev by offset:
    ///
    ///  ```text
    ///  | 111111111111 |            | 111111111111 |
    ///  | 222222222222 |            | 222222222222 |
    ///  | 333333333333 |            | 333333333333 |
    ///  | 444444444444 |    -->     | 444444444444 |
    ///  | 555555555555 |            |              |
    ///  |              |            | 555555555555 |
    ///  ```
    ///
    ///  Result:
    ///  ```text
    ///    Before                      After
    ///  | 111111111111 |            |              |
    ///  | 222222222222 |            | 111111111111 |
    ///  | 333333333333 |            | 222222222222 |
    ///  | 444444444444 |            | 333333333333 |
    ///  | 555555555555 |            | 444444444444 |
    ///  | 666666666666 |            | 555555555555 |
    ///  ```
    pub fn scroll_down(&mut self, scroll_region: &ScrollRegion, top: usize, offset: usize) {
        for y in (scroll_region.bottom - offset + 1)..=scroll_region.bottom {
            for x in 0..self.size().0 {
                self[(x, y)] = Cell::default();
            }
        }

        for y in ((top + offset)..=scroll_region.bottom).rev() {
            for x in 0..self.size().0 {
                let temp = self[(x, y)];
                self[(x, y)] = self[(x, y - offset)];
                self[(x, y - offset)] = temp;
            }
        }
    }

    /// See `BoundsIterator` documentation.
    pub fn bounds_iter(&self, area: Area) -> BoundsIterator {
        BoundsIterator {
            rows: std::cmp::min(self.rows.saturating_sub(1), get_y(upper_left!(area)))
                ..(std::cmp::min(self.rows, get_y(bottom_right!(area)) + 1)),
            cols: (
                std::cmp::min(self.cols.saturating_sub(1), get_x(upper_left!(area))),
                std::cmp::min(self.cols, get_x(bottom_right!(area)) + 1),
            ),
        }
    }

    /// See `RowIterator` documentation.
    pub fn row_iter(&self, bounds: std::ops::Range<usize>, row: usize) -> RowIterator {
        if row < self.rows {
            RowIterator {
                row,
                col: std::cmp::min(self.cols.saturating_sub(1), bounds.start)
                    ..(std::cmp::min(self.cols, bounds.end)),
            }
        } else {
            RowIterator { row, col: 0..0 }
        }
    }
}

impl Deref for CellBuffer {
    type Target = [Cell];

    fn deref(&self) -> &[Cell] {
        &self.buf
    }
}

impl DerefMut for CellBuffer {
    fn deref_mut(&mut self) -> &mut [Cell] {
        &mut self.buf
    }
}

impl Index<Pos> for CellBuffer {
    type Output = Cell;

    fn index(&self, index: Pos) -> &Cell {
        let (x, y) = index;
        self.get(x, y).expect("index out of bounds")
    }
}

impl IndexMut<Pos> for CellBuffer {
    fn index_mut(&mut self, index: Pos) -> &mut Cell {
        let (x, y) = index;
        self.get_mut(x, y).expect("index out of bounds")
    }
}

impl Default for CellBuffer {
    /// Constructs a new `CellBuffer` with a size of `(0, 0)`, using the default
    /// `Cell` as a blank.
    fn default() -> CellBuffer {
        CellBuffer::new(0, 0, Cell::default())
    }
}

impl fmt::Display for CellBuffer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        '_y: for y in 0..self.rows {
            for x in 0..self.cols {
                let c: &char = &self[(x, y)].ch();
                write!(f, "{}", *c).unwrap();
                if *c == '\n' {
                    continue '_y;
                }
            }
        }
        Ok(())
    }
}

/// A single point on a terminal display.
///
/// A `Cell` contains a character and style.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Cell {
    ch: char,

    empty: bool,
    fg: Color,
    bg: Color,
    attrs: Attr,
    keep_fg: bool,
    keep_bg: bool,
}

impl Cell {
    /// Creates a new `Cell` with the given `char`, `Color`s and `Attr`.
    ///
    /// # Examples
    ///
    /// ```norun
    /// let cell = Cell::new('x', Color::Default, Color::Green, Attr::Default);
    /// assert_eq!(cell.ch(), 'x');
    /// assert_eq!(cell.fg(), Color::Default);
    /// assert_eq!(cell.bg(), Color::Green);
    /// assert_eq!(cell.attrs(), Attr::Default);
    /// ```
    pub fn new(ch: char, fg: Color, bg: Color, attrs: Attr) -> Cell {
        Cell {
            ch,
            fg,
            bg,
            attrs,
            empty: false,
            keep_fg: false,
            keep_bg: false,
        }
    }

    /// Creates a new `Cell` with the given `char` and default style.
    ///
    /// # Examples
    ///
    /// ```norun
    /// let mut cell = Cell::with_char('x');
    /// assert_eq!(cell.ch(), 'x');
    /// assert_eq!(cell.fg(), Color::Default);
    /// assert_eq!(cell.bg(), Color::Default);
    /// assert_eq!(cell.attrs(), Attr::Default);
    /// ```
    pub fn with_char(ch: char) -> Cell {
        Cell::new(ch, Color::Default, Color::Default, Attr::Default)
    }

    /// Creates a new `Cell` with the given style and a blank `char`.
    ///
    /// # Examples
    ///
    /// ```norun
    /// let mut cell = Cell::with_style(Color::Default, Color::Red, Attr::Bold);
    /// assert_eq!(cell.fg(), Color::Default);
    /// assert_eq!(cell.bg(), Color::Red);
    /// assert_eq!(cell.attrs(), Attr::Bold);
    /// assert_eq!(cell.ch(), ' ');
    /// ```
    pub fn with_style(fg: Color, bg: Color, attr: Attr) -> Cell {
        Cell::new(' ', fg, bg, attr)
    }

    /// Returns the `Cell`'s character.
    ///
    /// # Examples
    ///
    /// ```norun
    /// let mut cell = Cell::with_char('x');
    /// assert_eq!(cell.ch(), 'x');
    /// ```
    pub fn ch(&self) -> char {
        self.ch
    }

    /// Sets the `Cell`'s character to the given `char`
    ///
    /// # Examples
    ///
    /// ```norun
    /// let mut cell = Cell::with_char('x');
    /// assert_eq!(cell.ch(), 'x');
    ///
    /// cell.set_ch('y');
    /// assert_eq!(cell.ch(), 'y');
    /// ```
    pub fn set_ch(&mut self, newch: char) -> &mut Cell {
        self.ch = newch;
        self.keep_fg = false;
        self.keep_bg = false;
        self
    }

    /// Returns the `Cell`'s foreground `Color`.
    ///
    /// # Examples
    ///
    /// ```norun
    /// let mut cell = Cell::with_style(Color::Blue, Color::Default, Attr::Default);
    /// assert_eq!(cell.fg(), Color::Blue);
    /// ```
    pub fn fg(&self) -> Color {
        self.fg
    }

    /// Sets the `Cell`'s foreground `Color` to the given `Color`.
    ///
    /// # Examples
    ///
    /// ```norun
    /// let mut cell = Cell::default();
    /// assert_eq!(cell.fg(), Color::Default);
    ///
    /// cell.set_fg(Color::White);
    /// assert_eq!(cell.fg(), Color::White);
    /// ```
    pub fn set_fg(&mut self, newfg: Color) -> &mut Cell {
        if !self.keep_fg {
            self.fg = newfg;
        }
        self
    }

    /// Returns the `Cell`'s background `Color`.
    ///
    /// # Examples
    ///
    /// ```norun
    /// let mut cell = Cell::with_style(Color::Default, Color::Green, Attr::Default);
    /// assert_eq!(cell.bg(), Color::Green);
    /// ```
    pub fn bg(&self) -> Color {
        self.bg
    }

    /// Sets the `Cell`'s background `Color` to the given `Color`.
    ///
    /// # Examples
    ///
    /// ```norun
    /// let mut cell = Cell::default();
    /// assert_eq!(cell.bg(), Color::Default);
    ///
    /// cell.set_bg(Color::Black);
    /// assert_eq!(cell.bg(), Color::Black);
    /// ```
    pub fn set_bg(&mut self, newbg: Color) -> &mut Cell {
        if !self.keep_bg {
            self.bg = newbg;
        }
        self
    }

    pub fn attrs(&self) -> Attr {
        self.attrs
    }

    pub fn set_attrs(&mut self, newattrs: Attr) -> &mut Cell {
        self.attrs = newattrs;
        self
    }

    /// Set a `Cell` as empty when a previous cell spans multiple columns and it
    /// would "overflow" to this cell.
    pub fn empty(&self) -> bool {
        self.empty
    }

    pub fn set_empty(&mut self, new_val: bool) -> &mut Cell {
        self.empty = new_val;
        self
    }

    /// Sets `keep_fg` field. If true, the foreground color will not be altered
    /// if attempted so until the character content of the cell is changed.
    pub fn set_keep_fg(&mut self, new_val: bool) -> &mut Cell {
        self.keep_fg = new_val;
        self
    }

    /// Sets `keep_bg` field. If true, the background color will not be altered
    /// if attempted so until the character content of the cell is changed.
    pub fn set_keep_bg(&mut self, new_val: bool) -> &mut Cell {
        self.keep_bg = new_val;
        self
    }
}

impl Default for Cell {
    /// Constructs a new `Cell` with a blank `char` and default `Color`s.
    ///
    /// # Examples
    ///
    /// ```norun
    /// let mut cell = Cell::default();
    /// assert_eq!(cell.ch(), ' ');
    /// assert_eq!(cell.fg(), Color::Default);
    /// assert_eq!(cell.bg(), Color::Default);
    /// ```
    fn default() -> Cell {
        Cell::new(' ', Color::Default, Color::Default, Attr::Default)
    }
}

/// The color of a `Cell`.
///
/// `Color::Default` represents the default color of the underlying terminal.
///
/// The eight basic colors may be used directly and correspond to 0x00..0x07 in
/// the 8-bit (256) color range; in addition, the eight basic colors coupled
/// with `Attr::Bold` correspond to 0x08..0x0f in the 8-bit color range.
///
/// `Color::Byte(..)` may be used to specify a color in the 8-bit range.
///
/// # Examples
///
/// ```norun
/// // The default color.
/// let default = Color::Default;
///
/// // A basic color.
/// let red = Color::Red;
///
/// // An 8-bit color.
/// let fancy = Color::Byte(0x01);
///
/// // Basic colors are also 8-bit colors (but not vice-versa).
/// assert_eq!(red.as_byte(), fancy.as_byte())
/// ```
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    Byte(u8),
    Rgb(u8, u8, u8),
    #[default]
    Default,
}

impl Color {
    /// Returns the `u8` representation of the `Color`.
    pub fn as_byte(self) -> u8 {
        match self {
            Color::Black => 0x00,
            Color::Red => 0x01,
            Color::Green => 0x02,
            Color::Yellow => 0x03,
            Color::Blue => 0x04,
            Color::Magenta => 0x05,
            Color::Cyan => 0x06,
            Color::White => 0x07,
            Color::Byte(b) => b,
            Color::Rgb(_, _, _) => unreachable!(),
            Color::Default => 0x00,
        }
    }

    pub fn from_byte(val: u8) -> Self {
        match val {
            0x00 => Color::Black,
            0x01 => Color::Red,
            0x02 => Color::Green,
            0x03 => Color::Yellow,
            0x04 => Color::Blue,
            0x05 => Color::Magenta,
            0x06 => Color::Cyan,
            0x07 => Color::White,
            _ => Color::Default,
        }
    }

    pub fn write_fg(self, stdout: &mut crate::state::StateStdout) -> std::io::Result<()> {
        use std::io::Write;
        match self {
            Color::Default => write!(stdout, "{}", termion::color::Fg(termion::color::Reset)),
            Color::Rgb(r, g, b) => write!(stdout, "{}", termion::color::Fg(TermionRgb(r, g, b))),
            _ => write!(stdout, "{}", termion::color::Fg(self.as_termion())),
        }
    }

    pub fn write_bg(self, stdout: &mut crate::state::StateStdout) -> std::io::Result<()> {
        use std::io::Write;
        match self {
            Color::Default => write!(stdout, "{}", termion::color::Bg(termion::color::Reset)),
            Color::Rgb(r, g, b) => write!(stdout, "{}", termion::color::Bg(TermionRgb(r, g, b))),
            _ => write!(stdout, "{}", termion::color::Bg(self.as_termion())),
        }
    }

    pub fn as_termion(self) -> AnsiValue {
        match self {
            b @ Color::Black
            | b @ Color::Red
            | b @ Color::Green
            | b @ Color::Yellow
            | b @ Color::Blue
            | b @ Color::Magenta
            | b @ Color::Cyan
            | b @ Color::White
            | b @ Color::Default => AnsiValue(b.as_byte()),
            Color::Byte(b) => AnsiValue(b),
            Color::Rgb(_, _, _) => AnsiValue(0),
        }
    }
}

/// The attributes of a `Cell`.
///
/// `Attr` enumerates all combinations of attributes a given style may have.
///
/// `Attr::Default` represents no attribute.
///
/// # Examples
///
/// ```norun
/// // Default attribute.
/// let def = Attr::Default;
///
/// // Base attribute.
/// let base = Attr::Bold;
///
/// // Combination.
/// let comb = Attr::UnderlineReverse;
/// ```
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum Attr {
    #[default]
    Default = 0b000,
    Bold = 0b001,
    Underline = 0b100,
    BoldUnderline = 0b011,
    Reverse = 0b010,
    BoldReverse = 0b101,
    UnderlineReverse = 0b110,
    BoldReverseUnderline = 0b111,
}

impl core::ops::BitOr for Attr {
    type Output = Attr;

    fn bitor(self, rhs: Self) -> Self::Output {
        match self as u8 | rhs as u8 {
            0b000 => Attr::Default,
            0b001 => Attr::Bold,
            0b100 => Attr::Underline,
            0b011 => Attr::BoldUnderline,
            0b010 => Attr::Reverse,
            0b101 => Attr::BoldReverse,
            0b110 => Attr::UnderlineReverse,
            0b111 => Attr::BoldReverseUnderline,
            _ => unsafe { std::hint::unreachable_unchecked() },
        }
    }
}

impl core::ops::BitAnd for Attr {
    type Output = bool;

    fn bitand(self, rhs: Self) -> Self::Output {
        self as u8 & rhs as u8 > 0
    }
}

impl core::ops::BitOrAssign for Attr {
    fn bitor_assign(&mut self, rhs: Attr) {
        use Attr::*;
        *self = match *self as u8 | rhs as u8 {
            0b000 => Default,
            0b001 => Bold,
            0b100 => Underline,
            0b011 => BoldUnderline,
            0b010 => Reverse,
            0b101 => BoldReverse,
            0b110 => UnderlineReverse,
            0b111 => BoldReverseUnderline,
            _ => unsafe { std::hint::unreachable_unchecked() },
        };
    }
}

impl Attr {
    pub fn write(self, prev: Attr, stdout: &mut crate::state::StateStdout) -> std::io::Result<()> {
        use std::io::Write;
        match (self & Attr::Bold, prev & Attr::Bold) {
            (true, true) | (false, false) => Ok(()),
            (false, true) => write!(stdout, "\x1B[22m"),
            (true, false) => write!(stdout, "\x1B[1m"),
        }
        .and_then(|_| match (self & Attr::Underline, prev & Attr::Underline) {
            (true, true) | (false, false) => Ok(()),
            (false, true) => write!(stdout, "\x1B[24m"),
            (true, false) => write!(stdout, "\x1B[4m"),
        })
        .and_then(|_| match (self & Attr::Reverse, prev & Attr::Reverse) {
            (true, true) | (false, false) => Ok(()),
            (false, true) => write!(stdout, "\x1B[27m"),
            (true, false) => write!(stdout, "\x1B[7m"),
        })
    }
}

pub fn copy_area_with_break(
    grid_dest: &mut CellBuffer,
    grid_src: &CellBuffer,
    dest: Area,
    src: Area,
) -> Pos {
    if !is_valid_area!(dest) || !is_valid_area!(src) {
        eprintln!(
            "BUG: Invalid areas in copy_area:\n src: {:?}\n dest: {:?}",
            src, dest
        );
        return upper_left!(dest);
    }

    if grid_src.is_empty() || grid_dest.is_empty() {
        return upper_left!(dest);
    }

    let mut ret = bottom_right!(dest);
    let mut src_x = get_x(upper_left!(src));
    let mut src_y = get_y(upper_left!(src));

    'y_: for y in get_y(upper_left!(dest))..=get_y(bottom_right!(dest)) {
        'x_: for x in get_x(upper_left!(dest))..=get_x(bottom_right!(dest)) {
            if grid_src[(src_x, src_y)].ch() == '\n' {
                src_y += 1;
                src_x = 0;
                if src_y >= get_y(bottom_right!(src)) {
                    ret.1 = y;
                    break 'y_;
                }
                continue 'y_;
            }

            grid_dest[(x, y)] = grid_src[(src_x, src_y)];
            src_x += 1;
            if src_x >= get_x(bottom_right!(src)) {
                src_y += 1;
                src_x = 0;
                if src_y >= get_y(bottom_right!(src)) {
                    // clear_area(grid_dest, ((get_x(upper_left!(dest)), y), bottom_right!(dest)));
                    ret.1 = y;
                    break 'y_;
                }
                break 'x_;
            }
        }
    }
    ret
}

/// Change foreground and background colors in an `Area`
pub fn change_colors(
    grid: &mut CellBuffer,
    area: Area,
    fg_color: Option<Color>,
    bg_color: Option<Color>,
) {
    for row in grid.bounds_iter(area) {
        for c in row {
            if let Some(fg_color) = fg_color {
                grid[c].set_fg(fg_color);
            }
            if let Some(bg_color) = bg_color {
                grid[c].set_bg(bg_color);
            }
        }
    }
}

macro_rules! inspect_bounds {
    ($grid:ident, $area:ident, $x: ident, $y: ident, $line_break:ident) => {
        let bounds = $grid.size();
        let (upper_left, bottom_right) = $area;
        if $x > (get_x(bottom_right)) || $x > get_x(bounds) {
            $x = get_x(upper_left);
            $y += 1;
            if $line_break.is_none() {
                break;
            } else {
                $x = $line_break.unwrap();
            }
        }
        if $y > (get_y(bottom_right)) || $y > get_y(bounds) {
            return ($x, $y - 1);
        }
    };
}

/// Write an `&str` to a `CellBuffer` in a specified `Area` with the passed
/// colors.
pub fn write_string_to_grid(
    s: &str,
    grid: &mut CellBuffer,
    fg_color: Color,
    bg_color: Color,
    attrs: Attr,
    area: Area,
    // The left-most x coordinate.
    line_break: Option<usize>,
) -> Pos {
    let bounds = grid.size();
    let upper_left = upper_left!(area);
    let bottom_right = bottom_right!(area);
    let (mut x, mut y) = upper_left;
    if y == get_y(bounds) || x == get_x(bounds) {
        return (x, y);
    }

    if y > (get_y(bottom_right))
        || x > get_x(bottom_right)
        || y > get_y(bounds)
        || x > get_x(bounds)
    {
        return (x, y);
    }
    for c in s.chars() {
        if c == '\r' {
            continue;
        }
        if c == '\t' {
            grid[(x, y)].set_ch(' ');
            x += 1;
            inspect_bounds!(grid, area, x, y, line_break);
            grid[(x, y)].set_ch(' ');
        } else {
            grid[(x, y)].set_ch(c);
        }
        grid[(x, y)]
            .set_fg(fg_color)
            .set_bg(bg_color)
            .set_attrs(attrs);

        match wcwidth(u32::from(c)) {
            Some(0) | None => {
                // Skip drawing zero width characters
                grid[(x, y)].empty = true;
            }
            Some(2) => {
                // Grapheme takes more than one column, so the next cell will be
                // drawn over. Set it as empty to skip drawing it.
                x += 1;
                inspect_bounds!(grid, area, x, y, line_break);
                grid[(x, y)] = Cell::default();
                grid[(x, y)]
                    .set_fg(fg_color)
                    .set_bg(bg_color)
                    .set_attrs(attrs)
                    .set_empty(true);
            }
            _ => {}
        }
        x += 1;

        inspect_bounds!(grid, area, x, y, line_break);
    }
    (x, y)
}

/// Completely clear an `Area` with an empty char and the terminal's default
/// colors.
pub fn clear_area(grid: &mut CellBuffer, area: Area) {
    if !is_valid_area!(area) {
        return;
    }
    for row in grid.bounds_iter(area) {
        for c in row {
            grid[c] = Cell::default();
        }
    }
}

/// Use `RowIterator` to iterate the cells of a row without the need to do any
/// bounds checking; the iterator will simply return `None` when it reaches the
/// end of the row. `RowIterator` can be created via the `CellBuffer::row_iter`
/// method and can be returned by `BoundsIterator` which iterates each row.
/// ```norun
/// for c in grid.row_iter(
///     x..(x + 11),
///     0,
/// ) {
///     grid[c].set_ch('w');
/// }
/// ```
pub struct RowIterator {
    row: usize,
    col: std::ops::Range<usize>,
}

/// `BoundsIterator` iterates each row returning a `RowIterator`.
/// ```norun
/// /* Visit each `Cell` in `area`. */
/// for c in grid.bounds_iter(area) {
///     grid[c].set_ch('w');
/// }
/// ```
pub struct BoundsIterator {
    rows: std::ops::Range<usize>,
    cols: (usize, usize),
}

impl Iterator for BoundsIterator {
    type Item = RowIterator;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next_row) = self.rows.next() {
            Some(RowIterator {
                row: next_row,
                col: self.cols.0..self.cols.1,
            })
        } else {
            None
        }
    }
}

impl Iterator for RowIterator {
    type Item = (usize, usize);
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next_col) = self.col.next() {
            Some((next_col, self.row))
        } else {
            None
        }
    }
}

impl RowIterator {
    pub fn forward_col(mut self, new_val: usize) -> Self {
        if self.col.start > new_val {
            self
        } else if self.col.end <= new_val {
            self.col.start = self.col.end;
            self
        } else {
            self.col.start = new_val;
            self
        }
    }
}
