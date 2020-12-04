/*
 * bb - bin.rs
 *
 * Copyright 2019 Manos Pitsidianakis
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

extern crate crossbeam;
extern crate nix;
extern crate signal_hook;
extern crate termion;

use crossbeam::channel::{bounded, tick};
use crossbeam::select;
use libc::c_int;
use std::io::Error;
use std::sync::{
    atomic::{AtomicBool, AtomicPtr},
    Arc,
};
use std::time::Duration;

pub const PROC_FS_ERROR_STR: &str = "/proc/ filesystem not found, are you running this on linux?";

//#[allow(dead_code)]
mod text_processing;
pub use crate::text_processing::*;
#[macro_use]
mod types;
pub use crate::types::*;

#[macro_use]
mod terminal;
pub use crate::terminal::*;

pub mod state;
pub use crate::state::*;

pub mod components;
pub use crate::components::*;
pub use crate::username::*;
pub mod username {
    use libc;
    use std::ptr::null_mut;
    /* taken from whoami-0.1.1 */
    fn getpwuid(
        pw_uid: u32,
        #[cfg(not(any(
            all(target_arch = "arm", target_pointer_width = "32"),
            target_arch = "aarch64"
        )))]
        buffer: &mut [i8; 16384],
        #[cfg(any(
            all(target_arch = "arm", target_pointer_width = "32"),
            target_arch = "aarch64"
        ))]
        buffer: &mut [u8; 16384],
    ) -> Option<libc::passwd> {
        let mut pwentp = null_mut();
        #[cfg(any(
            target_os = "macos",
            target_os = "ios",
            target_os = "freebsd",
            target_os = "dragonfly",
            target_os = "openbsd",
            target_os = "netbsd"
        ))]
        {
            let mut pwent = libc::passwd {
                pw_name: null_mut(),
                pw_passwd: null_mut(),
                pw_uid,
                pw_gid: 0,
                pw_change: 0,
                pw_class: null_mut(),
                pw_gecos: null_mut(),
                pw_dir: null_mut(),
                pw_shell: null_mut(),
                pw_expire: 0,
            };
            unsafe {
                libc::getpwuid_r(pw_uid, &mut pwent, buffer, 16384, &mut pwentp);
            }

            if pwentp.is_null() {
                None
            } else {
                Some(pwent)
            }
        }
        #[cfg(target_os = "linux")]
        {
            let mut pwent = libc::passwd {
                pw_name: null_mut(),
                pw_passwd: null_mut(),
                pw_uid,
                pw_gid: 0,
                pw_gecos: null_mut(),
                pw_dir: null_mut(),
                pw_shell: null_mut(),
            };

            unsafe {
                libc::getpwuid_r(pw_uid, &mut pwent, buffer.as_mut_ptr(), 16384, &mut pwentp);
            }
            if pwentp.is_null() {
                None
            } else {
                Some(pwent)
            }
        }
    }

    pub fn username(uid: u32) -> String {
        #[cfg(not(any(
            all(target_arch = "arm", target_pointer_width = "32"),
            target_arch = "aarch64"
        )))]
        let mut buffer = [0i8; 16384]; // from the man page
        #[cfg(any(
            all(target_arch = "arm", target_pointer_width = "32"),
            target_arch = "aarch64"
        ))]
        let mut buffer = [0u8; 16384]; // from the man page
        let pwent = getpwuid(uid, &mut buffer);

        let string;
        unsafe {
            string = match pwent {
                None => uid.to_string(),
                Some(p) => ::std::ffi::CStr::from_ptr(p.pw_name)
                    .to_str()
                    .unwrap_or_else(|_| "")
                    .to_string(),
            }
        }

        string
    }
}

fn notify(
    signals: &[c_int],
    exit_flag: Arc<AtomicBool>,
    state: Arc<AtomicPtr<StateStdout>>,
) -> Result<crossbeam::channel::Receiver<c_int>, Error> {
    let (s, r) = bounded(100);
    let sigint_handler = {
        let s = s.clone();
        let state = state.clone();
        move |_info: &nix::libc::siginfo_t| {
            if exit_flag.load(std::sync::atomic::Ordering::SeqCst) {
                crate::state::restore_to_main_screen(state.clone());
                std::process::exit(130);
            }
            exit_flag.store(true, std::sync::atomic::Ordering::SeqCst);
            let _ = s.send(signal_hook::SIGINT);
        }
    };
    let sigquit_handler = {
        let state = state.clone();
        move |_info: &nix::libc::siginfo_t| {
            crate::state::restore_to_main_screen(state.clone());
            std::process::exit(131);
        }
    };
    let sigterm_handler = move |_info: &nix::libc::siginfo_t| {
        crate::state::restore_to_main_screen(state.clone());
        std::process::exit(143);
    };
    unsafe {
        signal_hook_registry::register_sigaction(signal_hook::SIGINT, sigint_handler)?;
        signal_hook_registry::register_sigaction(signal_hook::SIGQUIT, sigquit_handler)?;
        signal_hook_registry::register_sigaction(signal_hook::SIGTERM, sigterm_handler)?;
    }
    let signals = signal_hook::iterator::Signals::new(signals)?;
    std::thread::spawn(move || {
        for signal in signals.forever() {
            s.send(signal).unwrap();
        }
    });
    Ok(r)
}

fn main() -> Result<(), Error> {
    /* Create the application State */
    let mut state = UIState::new();

    let signals = &[
        /*
        signal_hook::SIGALRM,
        signal_hook::SIGTERM,
        signal_hook::SIGINT,
        signal_hook::SIGQUIT,
        */
        /* Catch SIGWINCH to handle terminal resizing */
        signal_hook::SIGWINCH,
    ];

    let ticker = tick(Duration::from_millis(1600));

    let exit_flag: Arc<AtomicBool> = Arc::new(AtomicBool::new(false));
    let signal_recvr = notify(
        signals,
        exit_flag.clone(),
        state.stdout.as_ref().unwrap().clone(),
    )?;

    let receiver = state.receiver();
    let window = Box::new(Window::new(
        Box::new(components::KernelMetrics::new()),
        Box::new(components::ProcessList::new()),
    ));

    state.register_component(window);
    state.render();
    state.redraw(true);

    /* Keep track of the input mode. See UIMode for details */
    'main: loop {
        /* Poll on all channels. Currently we have the input channel for stdin, watching events and the signal watcher. */
        select! {
            recv(ticker) -> _ => {
                state.redraw(true);
            },
            recv(signal_recvr) -> sig => {
                match sig.unwrap() {
                    signal_hook::SIGWINCH => {
                        state.update_size();
                        state.render();
                        state.redraw(true);
                    },
                    signal_hook::SIGINT => {

                                drop(state);
                                break 'main;
                    }
                    _ => {}
                }
            },
            recv(receiver) -> msg => {
                match msg.unwrap() {
                    ThreadEvent::Input(Key::Ctrl('z')) => {
                        state.switch_to_main_screen();
                        //_thread_handler.join().expect("Couldn't join on the associated thread");
                        let self_pid = nix::unistd::Pid::this();
                        nix::sys::signal::kill(self_pid, nix::sys::signal::Signal::SIGSTOP).unwrap();
                        state.switch_to_alternate_screen();
                        state.restore_input();
                        // BUG: thread sends input event after one received key
                        state.update_size();
                        state.render();
                        state.redraw(true);
                    },
                    ThreadEvent::Input(Key::Ctrl('s')) => {
                        state.rcv_event(UIEvent::Freeze);
                        state.redraw(false);
                    }
                    ThreadEvent::Input(Key::Ctrl('q')) => {
                        state.rcv_event(UIEvent::Unfreeze);
                        state.redraw(false);
                    }
                    ThreadEvent::Input(k) => {
                        match k {
                            Key::Char('q') | Key::Char('Q') if state.mode == UIMode::Normal => {
                                drop(state);
                                break 'main;
                            },
                            key  => {
                                state.rcv_event(UIEvent::Input(key));
                                state.redraw(false);
                            },
                        }
                    },
                }
            },
        }
    }
    Ok(())
}
