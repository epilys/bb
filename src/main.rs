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

//!
//!  This crate contains the frontend stuff of the application. The application entry way on
//!  `src/bin.rs` creates an event loop and passes input to the `ui` module.
//!
//! The mail handling stuff is done in the `bb` crate which includes all backend needs. The
//! split is done to theoretically be able to create different frontends with the same innards.
//!

extern crate crossbeam;
extern crate nix;
extern crate signal_hook;
extern crate termion;

use crossbeam::channel::{bounded, tick};
use crossbeam::select;
use libc::c_int;
use std::io::Error;
use std::time::Duration;

mod ui;
use ui::*;

fn notify(signals: &[c_int]) -> Result<crossbeam::channel::Receiver<c_int>, Error> {
    let (s, r) = bounded(100);
    let signals = signal_hook::iterator::Signals::new(signals)?;
    std::thread::spawn(move || {
        for signal in signals.forever() {
            s.send(signal).unwrap();
        }
    });
    Ok(r)
}

fn main() -> Result<(), Error> {
    let signals = &[
        signal_hook::SIGALRM,
        signal_hook::SIGTERM,
        signal_hook::SIGINT,
        signal_hook::SIGQUIT,
        /* Catch SIGWINCH to handle terminal resizing */
        signal_hook::SIGWINCH,
    ];

    let ticker = tick(Duration::from_millis(1600));

    let signal_recvr = notify(signals)?;

    /* Create the application State */
    let mut state = State::new();

    let receiver = state.receiver();
    let window = Box::new(Window::new(
        Box::new(ui::components::KernelMetrics::new()),
        Box::new(ui::components::ProcessList::new()),
    ));

    state.register_component(window);
    state.render();
    state.redraw(true);

    /* Keep track of the input mode. See ui::UIMode for details */
    'main: loop {
        /* Poll on all channels. Currently we have the input channel for stdin, watching events and the signal watcher. */
        select! {
            recv(ticker) -> _ => {
                state.redraw(true);
            },
            recv(signal_recvr) -> sig => {
                eprintln!("got signal {:?}", sig);
                match sig.unwrap() {
                    signal_hook::SIGWINCH => {
                        state.update_size();
                        state.render();
                        state.redraw(true);
                    },
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
                    ThreadEvent::Input(k) => {
                        match k {
                            Key::Char('q') | Key::Char('Q') => {
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
