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
 * This library exports the public types and methods of its modules
 */

#[allow(dead_code)]
mod text_processing;
pub use crate::ui::text_processing::*;
#[macro_use]
#[allow(dead_code)]
mod types;
pub use crate::ui::types::*;

#[macro_use]
#[allow(dead_code)]
mod terminal;
pub use crate::ui::terminal::*;

pub mod state;
pub use crate::ui::state::*;

#[allow(dead_code)]
pub mod components;
pub use crate::ui::components::*;
pub use crate::ui::username::*;
pub mod username {
    use libc;
    use std::ptr::null_mut;
    /* taken from whoami-0.1.1 */
    fn getpwuid(pw_uid: u32) -> libc::passwd {
        let mut pwentp = null_mut();
        let mut buffer = [0i8; 16384]; // from the man page
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
                libc::getpwuid_r(pw_uid, &mut pwent, &mut buffer[0], 16384, &mut pwentp);
            }

            pwent
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
                libc::getpwuid_r(pw_uid, &mut pwent, &mut buffer[0], 16384, &mut pwentp);
            }

            pwent
        }
    }
    fn ptr_to_string(name: *mut i8) -> String {
        let uname = name as *mut _ as *mut u8;

        let s;
        let string;

        unsafe {
            s = ::std::slice::from_raw_parts(uname, libc::strlen(name));
            string = String::from_utf8_lossy(s).to_string();
        }

        string
    }

    pub fn username(uid: u32) -> String {
        let pwent = getpwuid(uid);

        ptr_to_string(pwent.pw_name)
    }
}
