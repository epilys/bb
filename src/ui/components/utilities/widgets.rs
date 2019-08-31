use super::*;

#[derive(Default)]
pub struct ScrollBar {
    show_arrows: bool,
    block_character: Option<char>,
}

impl ScrollBar {
    pub fn set_show_arrows(&mut self, flag: bool) {
        self.show_arrows = flag;
    }
    pub fn set_block_character(&mut self, val: Option<char>) {
        self.block_character = val;
    }
    pub fn draw(
        self,
        grid: &mut CellBuffer,
        area: Area,
        pos: usize,
        visible_rows: usize,
        length: usize,
    ) {
        if length == 0 {
            return;
        }
        let mut height = height!(area);
        if height < 3 {
            return;
        }
        if self.show_arrows {
            height -= height;
        }
        clear_area(grid, area);

        let visible_ratio: f32 = (std::cmp::min(visible_rows, length) as f32) / (length as f32);
        let scrollbar_height = std::cmp::max((visible_ratio * (height as f32)) as usize, 1);
        let scrollbar_offset = {
            let temp = (((pos as f32) / (length as f32)) * (height as f32)) as usize;
            if temp + scrollbar_height >= height {
                height - scrollbar_height
            } else {
                temp
            }
        };
        let (mut upper_left, bottom_right) = area;

        if self.show_arrows {
            grid[upper_left].set_ch('▴');
            upper_left = (upper_left.0, upper_left.1 + 1);
        }

        for y in get_y(upper_left)..(get_y(upper_left) + scrollbar_offset) {
            grid[set_y(upper_left, y)].set_ch(' ');
        }
        for y in (get_y(upper_left) + scrollbar_offset)
            ..=(get_y(upper_left) + scrollbar_offset + scrollbar_height)
        {
            grid[set_y(upper_left, y)].set_ch(self.block_character.unwrap_or('█'));
        }
        for y in (get_y(upper_left) + scrollbar_offset + scrollbar_height + 1)..get_y(bottom_right)
        {
            grid[set_y(upper_left, y)].set_ch(' ');
        }
        if self.show_arrows {
            grid[set_x(bottom_right, get_x(upper_left))].set_ch('▾');
        }
    }
}
