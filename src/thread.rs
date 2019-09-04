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

use crate::ui::components::processes::State;
use crate::ui::components::*;
use crossbeam::channel::bounded;
use crossbeam::{Receiver, Sender};
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;
use std::str::FromStr;

struct ProcessData {
    cpu_stat: Stat,
    processes_times: HashMap<Pid, usize>,
}

pub fn start_thread() -> (Sender<Vec<ProcessDisplay>>, Receiver<Vec<ProcessDisplay>>) {
    let (s, r): (Sender<Vec<ProcessDisplay>>, Receiver<Vec<ProcessDisplay>>) = bounded(1); /* ours */
    let (ts, tr): (Sender<Vec<ProcessDisplay>>, Receiver<Vec<ProcessDisplay>>) = bounded(1); /* theirs */
    ts.send(Vec::with_capacity(1024)).unwrap();
    std::thread::spawn(move || {
        let mut data = ProcessData {
            cpu_stat: get_stat(&mut 0).remove(0),
            processes_times: Default::default(),
        };
        loop {
            let mut processes = match tr.recv() {
                Ok(n) => n,
                Err(e) => panic!(e),
            };
            processes.clear();

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
                    pid: PidString(process.pid.to_string()),
                    ppid: PpidString(process.ppid.to_string()),
                    vm_rss: VmRssString(Bytes(process.vm_rss * 1024).as_convenient_string()),
                    cpu_percent: (100.0
                        * ((process.utime
                            - data
                                .processes_times
                                .get(&process.pid)
                                .map(|v| *v)
                                .unwrap_or(process.utime)) as f64
                            / ((cpu_stat.total_time() - data.cpu_stat.total_time()) as f64)))
                        as usize,
                    utime: process.utime,
                    state: process.state,
                    cmd_line: CmdLineString(process.cmd_line),
                    username: UserString(crate::ui::username(process.uid)),
                };
                if process_display.cpu_percent > 100 {
                    process_display.cpu_percent = 0;
                }

                data.processes_times
                    .insert(process.pid, process_display.utime);

                processes.push(process_display);
            }
            data.cpu_stat = cpu_stat;

            s.send(processes).unwrap();
        }
    });

    (ts, r)
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
