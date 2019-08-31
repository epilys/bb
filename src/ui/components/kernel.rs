use super::*;
use std::fs::File;
use std::io::prelude::*;
use std::str::FromStr;

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
        let total_rows = height!(area);
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
                false,
            );
            let (x, y) = write_string_to_grid(
                &self.os_type,
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                ((x + 2, y), bottom_right),
                false,
            );
            let (x, y) = write_string_to_grid(
                &self.kernel,
                grid,
                Color::Default,
                Color::Default,
                Attr::Default,
                ((x + 2, y), bottom_right),
                false,
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
            false,
        );

        let mut y_offset = 2;

        if !tick {
            return;
        }
        /* Draw CPU usage bars */

        let bar_max = std::dbg!((0.6 * total_cols as f32) as usize);

        let mut boot_time: usize = 0;
        for (i, cpu_stat) in get_stat(&mut boot_time).into_iter().enumerate() {
            let (mut x, y) = write_string_to_grid(
                "CPU",
                grid,
                Color::Byte(250),
                Color::Default,
                Attr::Default,
                (pos_inc(upper_left, (2, 2 + i)), bottom_right),
                false,
            );
            if i > 0 {
                write_string_to_grid(
                    &i.to_string(),
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((x, y), bottom_right),
                    false,
                );
            } else {
                /* add padding */
                write_string_to_grid(
                    " ",
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((x, y), bottom_right),
                    false,
                );
            }
            x += 2;

            /* Calculate percentages for the cpu usage bar */
            let busy_length = (cpu_stat.user_time + cpu_stat.system_time)
                - (self.cpu_stat[i].user_time + self.cpu_stat[i].system_time);
            let iowait_length = cpu_stat.iowait_time - self.cpu_stat[i].iowait_time;
            let bar_length: usize = std::dbg!(
                (((busy_length + iowait_length) as f64 / 100.0) * bar_max as f64) as usize
            );

            let mut x_offset = 0;
            while x_offset < bar_length {
                write_string_to_grid(
                    "▁",
                    grid,
                    Color::Byte(235),
                    Color::Byte(240),
                    Attr::Default,
                    ((x + x_offset, y), bottom_right),
                    false,
                );
                x_offset += 1;
            }
            while x_offset < bar_max {
                write_string_to_grid(
                    "▁",
                    grid,
                    Color::Byte(236),
                    Color::Byte(235),
                    Attr::Default,
                    ((x + x_offset, y), bottom_right),
                    false,
                );

                x_offset += 1;
            }
            self.cpu_stat[i] = cpu_stat;
            y_offset += 1;
        }
        self.boot_time = boot_time;

        /* Draw RAM usage bar */

        y_offset += 1;

        let bar_max = bar_max + 5;
        let (available, total) = get_mem_info();
        let available_length = ((available as f64 / total as f64) * bar_max as f64) as usize;
        let mem_bar_length = bar_max - available_length;
        let mem_display = format!(
            "RAM {}/{}",
            Bytes((total - available) * 1024).as_convenient_string(),
            Bytes(total * 1024).as_convenient_string()
        );
        let mem_display_padding = bar_max.saturating_sub(mem_display.len()) / 2;

        let mut x = 0;
        /* Calculate spillover of mem_display string to available part of the bar in order to
         * paint it differently */
        let cutoff = if mem_display_padding + mem_display.len() > mem_bar_length {
            mem_bar_length - mem_display_padding
        } else {
            mem_display.len()
        };

        while x < mem_bar_length {
            if x == mem_display_padding {
                let (_x, _) = write_string_to_grid(
                    &mem_display[0..cutoff],
                    grid,
                    Color::White,
                    Color::Byte(240),
                    Attr::Default,
                    (pos_inc(upper_left, (x + 2, y_offset)), bottom_right),
                    false,
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
                    false,
                );
                x += 1;
            }
        }
        let x = if cutoff != mem_display.len() {
            let (_x, _) = write_string_to_grid(
                &mem_display[cutoff..],
                grid,
                Color::White,
                Color::Byte(235),
                Attr::Default,
                (pos_inc(upper_left, (x + 2, y_offset)), bottom_right),
                false,
            );
            _x
        } else {
            x
        };
        for x in x..bar_max {
            write_string_to_grid(
                " ",
                grid,
                Color::Default,
                Color::Byte(235),
                Attr::Default,
                (pos_inc(upper_left, (x + 2, y_offset)), bottom_right),
                false,
            );
        }
    }

    fn process_event(&mut self, event: &mut UIEvent) {
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

#[derive(Debug)]
struct Stat {
    user_time: usize,
    system_time: usize,
    idle_time: usize,
    iowait_time: usize,
}

fn get_stat(boot_time: &mut usize) -> Vec<Stat> {
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
        mut_value_iter.next();
        let system_time = usize::from_str(&mut_value_iter.next().unwrap()).unwrap();
        let idle_time = usize::from_str(&mut_value_iter.next().unwrap()).unwrap();
        let iowait_time = usize::from_str(&mut_value_iter.next().unwrap()).unwrap();
        ret.push(Stat {
            user_time,
            system_time,
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
