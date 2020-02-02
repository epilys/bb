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
use std::fs::File;
use std::io::prelude::*;
use std::str::FromStr;

/* if cpu_no > MAX_CPU_ROWS, the cpu bars wrap in columns */
static MAX_CPU_ROWS: usize = 5;
/* Kernel metrics components */
#[derive(Debug)]
pub struct KernelMetrics {
    hostname: String,
    kernel: String,
    os_type: String,
    uptime: String,
    cpu_stat: Vec<Stat>,
    boot_time: usize,
    dirty: bool,
}

impl fmt::Display for KernelMetrics {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "kernel")
    }
}

impl KernelMetrics {
    pub fn new() -> Self {
        let mut file = File::open("/proc/sys/kernel/hostname").unwrap();
        let mut hostname = String::new();
        file.read_to_string(&mut hostname).unwrap();
        let mut kernel = String::new();
        file = File::open("/proc/sys/kernel/version").unwrap();
        file.read_to_string(&mut kernel).unwrap();
        let mut os_type = String::new();
        file = File::open("/proc/sys/kernel/ostype").unwrap();
        file.read_to_string(&mut os_type).unwrap();
        let mut boot_time = 0;
        let cpu_stat = get_stat(&mut boot_time);
        KernelMetrics {
            hostname,
            kernel,
            os_type,
            uptime: String::with_capacity(60),
            cpu_stat,
            boot_time,
            dirty: true,
        }
    }

    /* Returns width of entire widget */
    fn draw_cpu_bars(&mut self, grid: &mut CellBuffer, area: Area) -> usize {
        let upper_left = upper_left!(area);
        let total_cols = width!(area);
        /* no of bars is no of CPUs along with the total CPU usage  */
        let cpu_no = self.cpu_stat.len();

        /* Calculate how much horizontal space the labels (ie CPU0, CPU1, CPU2) take in order to
         * distribute the remainder for each column, specifically the bars */
        let mut cpu_label_space = 0;
        let mut cpu_bar_columns = 0;
        let mut i = 0;
        while i < cpu_no {
            /* Reserve space for CPU labels */
            if i < 10 {
                /* label will be " CPU0  " */
                cpu_label_space += 7;
            } else if i < 100 {
                /* label will be " CPU32  " */
                cpu_label_space += 8;
            } else {
                /* label will be " CPU128  " */
                cpu_label_space += 9;
            }
            /* each column holds MAX_CPU_ROWS */
            i += MAX_CPU_ROWS;
            cpu_bar_columns += 1;
        }
        /* max width of each cpu bar */
        let bar_width = if cpu_no < MAX_CPU_ROWS {
            total_cols
        } else {
            (total_cols - cpu_label_space) / (cpu_bar_columns)
        };

        let mut boot_time: usize = 0;

        let mut x_offset = 0;
        for (i, cpu_stat) in get_stat(&mut boot_time).into_iter().enumerate() {
            let label_len = if i < 10 {
                8
            } else if i < 100 {
                9
            } else {
                10
            };
            let bottom_right = pos_inc(
                upper_left,
                (x_offset + bar_width + label_len, i % MAX_CPU_ROWS + 2),
            );
            let (mut x, y) = if i > 0 {
                write_string_to_grid(
                    &format!("CPU{}", i),
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Bold,
                    (
                        pos_inc(upper_left, (x_offset, 2 + (i % MAX_CPU_ROWS))),
                        bottom_right,
                    ),
                    None,
                )
            } else {
                /* add padding */
                let (x, y) = write_string_to_grid(
                    "Σ",
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    (
                        pos_inc(upper_left, (x_offset, 2 + i % MAX_CPU_ROWS)),
                        bottom_right,
                    ),
                    None,
                );
                write_string_to_grid(
                    "CPU",
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Bold,
                    ((x, y), bottom_right),
                    None,
                )
            };
            x += 2;

            /* Calculate percentages for the cpu usage bar */
            let bar_length: usize = ((cpu_stat
                .busy_time()
                .saturating_sub(self.cpu_stat[i].busy_time())
                as f64
                / (cpu_stat
                    .total_time()
                    .saturating_sub(self.cpu_stat[i].total_time())) as f64)
                * bar_width as f64) as usize;
            if bar_length >= width!(area) {
                return x_offset;
            }

            /* Sometimes you draw the bar */
            let mut _x_offset = 0;
            while _x_offset < bar_length {
                write_string_to_grid(
                    "▁",
                    grid,
                    Color::Byte(235),
                    Color::Byte(240),
                    Attr::Default,
                    ((x + _x_offset, y), bottom_right),
                    None,
                );
                _x_offset += 1;
            }
            /* and sometimes the bar draws you */
            while _x_offset <= bar_width {
                write_string_to_grid(
                    "▁",
                    grid,
                    Color::Byte(236),
                    Color::Byte(235),
                    Attr::Default,
                    ((x + _x_offset, y), bottom_right),
                    None,
                );

                _x_offset += 1;
            }
            self.cpu_stat[i] = cpu_stat;
            if (i + 1) % MAX_CPU_ROWS == 0 {
                x_offset += bar_width + label_len;
            }
        }

        self.boot_time = boot_time;
        if (self.cpu_stat.len()) % MAX_CPU_ROWS == 0 {
            x_offset
        } else {
            x_offset
                + bar_width
                + if self.cpu_stat.len() < 10 {
                    8
                } else if self.cpu_stat.len() < 100 {
                    9
                } else {
                    10
                }
        }
    }

    fn draw_ram_bar(&mut self, grid: &mut CellBuffer, area: Area, bars_max: usize) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        if bars_max == 0 {
            /* In first draw, we have no cpu data since there is no previous measurement to use
             * in the calculation so bars_max will be 0 */
            return;
        }
        let (available, total) = get_mem_info();

        /*  available_length == the length the spaces takes up in this case:
         *  |********       | 50%
         */
        let available_length = ((available as f64 / total as f64) * bars_max as f64) as usize;
        /*  mem_bar length == the length the asterisks takes up in this case:
         *  |********       | 50%
         */
        let mem_bar_length = bars_max - available_length;
        let mem_display = format!(
            "RAM {}/{}",
            Bytes((total - available) * 1024).as_convenient_string(),
            Bytes(total * 1024).as_convenient_string()
        );
        /* Put the "RAM XGB/YGB" in the middle of the RAM bar */
        let mem_display_padding = bars_max.saturating_sub(mem_display.len()) / 2;

        let y_offset = 2 + MAX_CPU_ROWS;
        let mut x = 0;
        /* Calculate spillover of mem_display string to available part of the bar in order to
         * paint it differently
         *
         * If "RAM 1GB/2GB" is printed over this bar:
         *
         *          |**********          |
         *
         * Some part of the string will have different colors than the rest of it, because the
         * available part of the bar has different colors.
         *
         *                   cutoff
         *                     ⇩
         *                RAM 1GB/2GB
         *                ↓ ↓ ↓ ↓ ↓ ↓ (string overlays asterisks and spaces)
         *          |**********          |
         * */
        let cutoff = std::cmp::min(
            mem_display.len(),
            if mem_display_padding + mem_display.len() > mem_bar_length {
                mem_bar_length - mem_display_padding
            } else {
                mem_display.len()
            },
        );

        while x <= available_length + mem_bar_length {
            if x == mem_display_padding {
                let (_x, _) = write_string_to_grid(
                    &mem_display[0..cutoff],
                    grid,
                    Color::White,
                    Color::Byte(240),
                    Attr::Default,
                    (pos_inc(upper_left, (x + 2, y_offset)), bottom_right),
                    None,
                );
                x += cutoff;
            } else {
                write_string_to_grid(
                    "█",
                    grid,
                    Color::Byte(240),
                    Color::Byte(235),
                    Attr::Default,
                    (pos_inc(upper_left, (x + 2, y_offset)), bottom_right),
                    None,
                );
                x += 1;
            }
        }
        if cutoff != mem_display.len() {
            let (_x, _) = write_string_to_grid(
                &mem_display[cutoff..],
                grid,
                Color::White,
                Color::Byte(235),
                Attr::Default,
                (pos_inc(upper_left, (x + 2, y_offset)), bottom_right),
                None,
            );
        }
    }
}

impl Component for KernelMetrics {
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
        let total_cols = width!(area);

        dirty_areas.push_back(area);
        if self.dirty {
            clear_area(grid, area);
            let (x, y) = write_string_to_grid(
                &self.hostname,
                grid,
                Color::Default,
                Color::Default,
                Attr::Bold,
                area,
                None,
            );
            let (x, y) = write_string_to_grid(
                &self.os_type,
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                ((x + 2, y), bottom_right),
                None,
            );
            write_string_to_grid(
                &self.kernel,
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                ((x + 2, y), bottom_right),
                None,
            );
            self.dirty = false;
        }

        /* Draw uptime */
        let mut file = File::open("/proc/uptime").unwrap();
        self.uptime.clear();
        file.read_to_string(&mut self.uptime).unwrap();
        let seconds: usize =
            f64::from_str(self.uptime.split(" ").next().unwrap()).unwrap() as usize;
        let days = seconds / (60 * 60 * 24);
        let hours = seconds / 3600 - days * 24;
        let mins = seconds / 60 - hours * 60 - days * 24 * 60;
        let seconds = seconds % 60;
        let uptime = if days > 0 {
            format!(
                "uptime: {} days, {:02}:{:02}:{:02}",
                days, hours, mins, seconds
            )
        } else {
            format!("uptime: {:02}:{:02}:{:02}", hours, mins, seconds)
        };

        write_string_to_grid(
            &uptime,
            grid,
            Color::Default,
            Color::Default,
            Attr::Default,
            (
                (get_x(bottom_right) - uptime.len(), get_y(upper_left)),
                bottom_right,
            ),
            None,
        );

        if !tick {
            return;
        }
        let old_cpu_stat = self.cpu_stat[0];

        /* Draw CPU usage bars */

        /* max width of cpu bar area */
        let bars_max = (0.6 * total_cols as f32) as usize;
        let cpu_widget_width = self.draw_cpu_bars(
            grid,
            (
                pos_inc(upper_left, (2, 0)),
                pos_inc(upper_left, (bars_max + 1, MAX_CPU_ROWS + 1)),
            ),
        );
        /* Draw RAM usage bar */

        self.draw_ram_bar(grid, area, cpu_widget_width.saturating_sub(2));
        /* Various values table */
        /* max width of cpu bar area */
        let bars_max = (0.6 * total_cols as f32) as usize;

        /* CPU Times */
        let mut cpu_column_width = "CPU".len();
        let upper_left = pos_inc(upper_left, (bars_max + 5, 2));
        clear_area(grid, (upper_left, bottom_right));
        if get_x(upper_left) >= get_x(bottom_right) {
            return;
        }
        write_string_to_grid(
            "CPU%",
            grid,
            Color::Default,
            Color::Default,
            Attr::Bold,
            (upper_left, bottom_right),
            None,
        );

        for (i, (tag, s, fg_color, bg_color)) in get_cpu_times(&old_cpu_stat, &self.cpu_stat[0])
            .into_iter()
            .enumerate()
        {
            let (x, y) = write_string_to_grid(
                tag,
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                (pos_inc(upper_left, (0, i + 1)), bottom_right),
                None,
            );

            let padding = 6 - s.len();
            clear_area(grid, ((x, y), (x + padding + 1, y)));

            write_string_to_grid(
                &s,
                grid,
                fg_color,
                bg_color,
                Attr::Default,
                ((x + 2 + padding, y), bottom_right),
                None,
            );
            cpu_column_width = std::cmp::max(tag.len() + s.len() + 4, cpu_column_width);
        }

        /* Load average */
        let upper_left = pos_inc(upper_left, (cpu_column_width + 3, 0));
        if get_x(upper_left) >= get_x(bottom_right) {
            return;
        }
        write_string_to_grid(
            "LOAD_AVG",
            grid,
            Color::Default,
            Color::Default,
            Attr::Bold,
            (upper_left, bottom_right),
            None,
        );
        let loadavgs = get_loadavg();
        for (i, avg) in loadavgs.into_iter().enumerate() {
            write_string_to_grid(
                &format!(
                    "{}  {}",
                    match i {
                        0 => " 1",
                        1 => " 5",
                        _ => "15",
                    },
                    avg
                ),
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                (pos_inc(upper_left, (0, i + 1)), bottom_right),
                None,
            );
            grid[pos_inc(upper_left, (0, i + 1))].set_attrs(Attr::Bold);
            grid[pos_inc(upper_left, (1, i + 1))].set_attrs(Attr::Bold);
            grid[pos_inc(upper_left, (0, i + 1))].set_fg(Color::Byte(8));
            grid[pos_inc(upper_left, (1, i + 1))].set_fg(Color::Byte(8));
        }
    }

    fn process_event(&mut self, event: &mut UIEvent, _ui_mode: &mut UIMode) {
        match event {
            UIEvent::Resize => {
                self.dirty = true;
            }
            _ => {}
        }
    }

    fn is_dirty(&self) -> bool {
        true
    }

    fn set_dirty(&mut self) {
        self.dirty = true;
    }
}

fn get_mem_info() -> (usize, usize) {
    let mut file = File::open("/proc/meminfo").unwrap();
    let mut res = String::with_capacity(2048);
    file.read_to_string(&mut res).unwrap();
    let mut lines_iter = res.lines();
    let mem_total = usize::from_str(
        lines_iter
            .next()
            .unwrap()
            .split_whitespace()
            .skip(1)
            .next()
            .unwrap(),
    )
    .unwrap();
    let mem_available = usize::from_str(
        lines_iter
            .next()
            .unwrap()
            .split_whitespace()
            .skip(1)
            .next()
            .unwrap(),
    )
    .unwrap();
    (mem_available, mem_total)
}

fn get_loadavg() -> [String; 3] {
    let mut file = File::open("/proc/loadavg").unwrap();
    let mut res = String::with_capacity(2048);
    file.read_to_string(&mut res).unwrap();
    let mut mut_value_iter = res.split_whitespace();
    let avg_1 = mut_value_iter.next().unwrap().to_string();
    let avg_5 = mut_value_iter.next().unwrap().to_string();
    let avg_15 = mut_value_iter.next().unwrap().to_string();
    [avg_1, avg_5, avg_15]
}

fn get_cpu_times(
    old_cpu_stat: &Stat,
    cpu_stat: &Stat,
) -> Vec<(&'static str, String, Color, Color)> {
    let mut ret = Vec::new();

    macro_rules! val {
        ($tag:literal, $field:tt) => {
            let percent = (cpu_stat.$field.saturating_sub(old_cpu_stat.$field)) as f64
                / (cpu_stat
                    .total_time()
                    .saturating_sub(old_cpu_stat.total_time())) as f64;
            let s = format!("{:.1}%", percent * 100.0);
            ret.push((
                $tag,
                s,
                if percent < 0.15 {
                    Color::Default
                } else if percent < 0.50 {
                    Color::Default
                } else {
                    Color::White
                },
                if percent < 0.15 {
                    Color::Default
                } else if percent < 0.50 {
                    Color::Byte(70)
                } else if $tag != "idle   " {
                    Color::Red
                } else {
                    Color::Default
                },
            ));
        };
    };

    /* user % */
    val!("user   ", user_time);
    /* system % */
    val!("system ", system_time);
    /* nice % */
    val!("nice   ", nice_time);
    /* idle % */
    val!("idle   ", idle_time);
    /* iowait % */
    val!("iowait ", iowait_time);

    ret
}
