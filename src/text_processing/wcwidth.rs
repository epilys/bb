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

// This is an implementation of wcwidth() and wcswidth() as defined in
// "The Single UNIX Specification, Version 2, The Open Group, 1997"
// <http://www.UNIX-systems.org/online.html>
//
// Markus Kuhn -- 2001-09-08 -- public domain

// TODO: Spacing widths
// Update to Unicode 12

#[macro_export]
macro_rules! big_if_true {
    ($a:expr) => {
        if $a {
            1
        } else {
            0
        }
    };
}

type WChar = u32;
type Interval = (WChar, WChar);

pub struct CodePointsIterator<'a> {
    rest: &'a [u8],
}

/// UTF-8 uses a system of binary prefixes, in which the high bits of each
/// byte mark whether it’s a single byte, the beginning of a multi-byte
/// sequence, or a continuation byte; the remaining bits, concatenated, give
/// the code point index. This table shows how it works:
///
/// | UTF-8 (binary)                        | Code point (binary)     | Range              |
/// |---------------------------------------|-------------------------|--------------------|
/// | `0xxxxxxx`                            | `xxxxxxx`               | `U+0000–U+007F`    |
/// | `110xxxxx 10yyyyyy`                   | `xxxxxyyyyyy`           | `U+0080–U+07FF`    |
/// | `1110xxxx 10yyyyyy 10zzzzzz`          | `xxxxyyyyyyzzzzzz`      | `U+0800–U+FFFF`    |
/// | `11110xxx 10yyyyyy 10zzzzzz 10wwwwww` | `xxxyyyyyyzzzzzzwwwwww` | `U+10000–U+10FFFF` |
impl<'a> Iterator for CodePointsIterator<'a> {
    type Item = WChar;

    fn next(&mut self) -> Option<WChar> {
        if self.rest.is_empty() {
            return None;
        }
        // Input is UTF-8 valid strings, guaranteed by Rust's std
        if self.rest[0] & 0b1000_0000 == 0x0 {
            let ret: WChar = WChar::from(self.rest[0]);
            self.rest = &self.rest[1..];
            return Some(ret);
        }
        if self.rest[0] & 0b1110_0000 == 0b1100_0000 {
            let ret: WChar = (WChar::from(self.rest[0]) & 0b0001_1111).rotate_left(6)
                + (WChar::from(self.rest[1]) & 0b0111_1111);
            self.rest = &self.rest[2..];
            return Some(ret);
        }

        if self.rest[0] & 0b1111_0000 == 0b1110_0000 {
            let ret: WChar = (WChar::from(self.rest[0]) & 0b0000_0111).rotate_left(12)
                + (WChar::from(self.rest[1]) & 0b0011_1111).rotate_left(6)
                + (WChar::from(self.rest[2]) & 0b0011_1111);
            self.rest = &self.rest[3..];
            return Some(ret);
        }

        let ret: WChar = (WChar::from(self.rest[0]) & 0b0000_0111).rotate_left(18)
            + (WChar::from(self.rest[1]) & 0b0011_1111).rotate_left(12)
            + (WChar::from(self.rest[2]) & 0b0011_1111).rotate_left(6)
            + (WChar::from(self.rest[3]) & 0b0011_1111);
        self.rest = &self.rest[4..];
        Some(ret)
    }
}
pub trait CodePointsIter {
    fn code_points(&self) -> CodePointsIterator;
}

impl CodePointsIter for str {
    fn code_points(&self) -> CodePointsIterator {
        CodePointsIterator {
            rest: self.as_bytes(),
        }
    }
}
impl CodePointsIter for &str {
    fn code_points(&self) -> CodePointsIterator {
        CodePointsIterator {
            rest: self.as_bytes(),
        }
    }
}

// auxiliary function for binary search in Interval table
fn bisearch(ucs: WChar, table: &'static [Interval]) -> bool {
    let mut min = 0;
    let mut mid;

    let mut max = table.len() - 1;

    if ucs < table[0].0 || ucs > table[max].1 {
        return false;
    }
    while max >= min {
        mid = (min + max) / 2;
        if ucs > table[mid].1 {
            min = mid + 1;
        } else if ucs < table[mid].0 {
            max = mid - 1;
        } else {
            return true;
        }
    }

    false
}

// The following functions define the column width of an ISO 10646
// character as follows:
//
//    - The null character (U+0000) has a column width of 0.
//
//    - Other C0/C1 control characters and DEL will lead to a return value of
//      -1.
//
//    - Non-spacing and enclosing combining characters (general category code Mn
//      or Me in the Unicode database) have a column width of 0.
//
//    - Other format characters (general category code Cf in the Unicode
//      database) and ZERO WIDTH SPACE (U+200B) have a column width of 0.
//
//    - Hangul Jamo medial vowels and final consonants (U+1160-U+11FF) have a
//      column width of 0.
//
//    - Spacing characters in the East Asian Wide (W) or East Asian FullWidth
//      (F) category as defined in Unicode Technical Report #11 have a column
//      width of 2.
//
//    - All remaining characters (including all printable ISO 8859-1 and WGL4
//      characters, Unicode control characters, etc.) have a column width of 1.
//
// This implementation assumes that wchar_t characters are encoded
// in ISO 10646.

pub fn wcwidth(ucs: WChar) -> Option<usize> {
    // sorted list of non-overlapping intervals of non-spacing characters
    let combining: &'static [Interval] = &[
        (0x0300, 0x034E),
        (0x0360, 0x0362),
        (0x0483, 0x0486),
        (0x0488, 0x0489),
        (0x0591, 0x05A1),
        (0x05A3, 0x05B9),
        (0x05BB, 0x05BD),
        (0x05BF, 0x05BF),
        (0x05C1, 0x05C2),
        (0x05C4, 0x05C4),
        (0x064B, 0x0655),
        (0x0670, 0x0670),
        (0x06D6, 0x06E4),
        (0x06E7, 0x06E8),
        (0x06EA, 0x06ED),
        (0x070F, 0x070F),
        (0x0711, 0x0711),
        (0x0730, 0x074A),
        (0x07A6, 0x07B0),
        (0x0901, 0x0902),
        (0x093C, 0x093C),
        (0x0941, 0x0948),
        (0x094D, 0x094D),
        (0x0951, 0x0954),
        (0x0962, 0x0963),
        (0x0981, 0x0981),
        (0x09BC, 0x09BC),
        (0x09C1, 0x09C4),
        (0x09CD, 0x09CD),
        (0x09E2, 0x09E3),
        (0x0A02, 0x0A02),
        (0x0A3C, 0x0A3C),
        (0x0A41, 0x0A42),
        (0x0A47, 0x0A48),
        (0x0A4B, 0x0A4D),
        (0x0A70, 0x0A71),
        (0x0A81, 0x0A82),
        (0x0ABC, 0x0ABC),
        (0x0AC1, 0x0AC5),
        (0x0AC7, 0x0AC8),
        (0x0ACD, 0x0ACD),
        (0x0B01, 0x0B01),
        (0x0B3C, 0x0B3C),
        (0x0B3F, 0x0B3F),
        (0x0B41, 0x0B43),
        (0x0B4D, 0x0B4D),
        (0x0B56, 0x0B56),
        (0x0B82, 0x0B82),
        (0x0BC0, 0x0BC0),
        (0x0BCD, 0x0BCD),
        (0x0C3E, 0x0C40),
        (0x0C46, 0x0C48),
        (0x0C4A, 0x0C4D),
        (0x0C55, 0x0C56),
        (0x0CBF, 0x0CBF),
        (0x0CC6, 0x0CC6),
        (0x0CCC, 0x0CCD),
        (0x0D41, 0x0D43),
        (0x0D4D, 0x0D4D),
        (0x0DCA, 0x0DCA),
        (0x0DD2, 0x0DD4),
        (0x0DD6, 0x0DD6),
        (0x0E31, 0x0E31),
        (0x0E34, 0x0E3A),
        (0x0E47, 0x0E4E),
        (0x0EB1, 0x0EB1),
        (0x0EB4, 0x0EB9),
        (0x0EBB, 0x0EBC),
        (0x0EC8, 0x0ECD),
        (0x0F18, 0x0F19),
        (0x0F35, 0x0F35),
        (0x0F37, 0x0F37),
        (0x0F39, 0x0F39),
        (0x0F71, 0x0F7E),
        (0x0F80, 0x0F84),
        (0x0F86, 0x0F87),
        (0x0F90, 0x0F97),
        (0x0F99, 0x0FBC),
        (0x0FC6, 0x0FC6),
        (0x102D, 0x1030),
        (0x1032, 0x1032),
        (0x1036, 0x1037),
        (0x1039, 0x1039),
        (0x1058, 0x1059),
        (0x1160, 0x11FF),
        (0x17B7, 0x17BD),
        (0x17C6, 0x17C6),
        (0x17C9, 0x17D3),
        (0x180B, 0x180E),
        (0x18A9, 0x18A9),
        (0x200B, 0x200F),
        (0x202A, 0x202E),
        (0x206A, 0x206F),
        (0x20D0, 0x20E3),
        (0x302A, 0x302F),
        (0x3099, 0x309A),
        (0xFB1E, 0xFB1E),
        (0xFE20, 0xFE23),
        (0xFEFF, 0xFEFF),
        (0xFFF9, 0xFFFB),
    ];

    // test for 8-bit control characters
    if ucs == 0 {
        return Some(0);
    }
    if ucs < 32 || (0x7f..0xa0).contains(&ucs) {
        return None;
    }

    // binary search in table of emojis
    if bisearch(ucs, EMOJI_RANGES) {
        return Some(2);
    }
    // binary search in table of non-spacing characters
    if bisearch(ucs, combining) {
        return Some(1);
    }

    // if we arrive here, ucs is not a combining or C0/C1 control character

    Some(
        1 + big_if_true!(
            ucs >= 0x1100
                && (ucs <= 0x115f ||                    /* Hangul Jamo init. consonants */
      ((0x2e80..=0xa4cf).contains(&ucs) && (ucs & !0x0011) != 0x300a &&
       ucs != 0x303f) ||                  /* CJK ... Yi */
      (0xac00..=0xd7a3).contains(&ucs) || /* Hangul Syllables */
      (0xf900..=0xfaff).contains(&ucs) || /* CJK Compatibility Ideographs */
      (0xfe30..=0xfe6f).contains(&ucs) || /* CJK Compatibility Forms */
      (0xff00..=0xff5f).contains(&ucs) || /* Fullwidth Forms */
      (0xffe0..=0xffe6).contains(&ucs) ||
      (0x20000..=0x2ffff).contains(&ucs))
        ),
    )
}

pub fn wcswidth(mut pwcs: WChar, mut n: usize) -> Option<usize> {
    let mut width = 0;

    while pwcs > 0 && n > 0 {
        if let Some(w) = wcwidth(pwcs) {
            width += w;
        } else {
            return None;
        }

        pwcs += 1;
        n -= 1;
    }

    Some(width)
}

const EMOJI_RANGES: &[Interval] = &[
    (0x231A, 0x231B), /*    ; Basic_Emoji              ; watch
                       * #  1.1  [2] (⌚..⌛) */
    (0x23E9, 0x23EC), /*    ; Basic_Emoji              ; fast-forward button
                       * #  6.0  [4] (⏩..⏬) */
    (0x23F0, 0x23F0), /*          ; Basic_Emoji              ; alarm clock
                       * #  6.0  [1] (⏰) */
    (0x23F3, 0x23F3), /*          ; Basic_Emoji              ; hourglass not done
                       * #  6.0  [1] (⏳) */
    (0x25FD, 0x25FE), /*    ; Basic_Emoji              ; white medium-small square
                       * #  3.2  [2] (◽..◾) */
    (0x2614, 0x2615), /*    ; Basic_Emoji              ; umbrella with rain drops
                       * #  4.0  [2] (☔..☕) */
    (0x2648, 0x2653), /*    ; Basic_Emoji              ; Aries
                       * #  1.1 [12] (♈..♓) */
    (0x267F, 0x267F), /*          ; Basic_Emoji              ; wheelchair symbol
                       * #  4.1  [1] (♿) */
    (0x2693, 0x2693), /*          ; Basic_Emoji              ; anchor
                       * #  4.1  [1] (⚓) */
    (0x26A1, 0x26A1), /*          ; Basic_Emoji              ; high voltage
                       * #  4.0  [1] (⚡) */
    (0x26AA, 0x26AB), /*    ; Basic_Emoji              ; white circle
                       * #  4.1  [2] (⚪..⚫) */
    (0x26BD, 0x26BE), /*    ; Basic_Emoji              ; soccer ball
                       * #  5.2  [2] (⚽..⚾) */
    (0x26C4, 0x26C5), /*    ; Basic_Emoji              ; snowman without snow
                       * #  5.2  [2] (⛄..⛅) */
    (0x26CE, 0x26CE), /*          ; Basic_Emoji              ; Ophiuchus
                       * #  6.0  [1] (⛎) */
    (0x26D4, 0x26D4), /*          ; Basic_Emoji              ; no entry
                       * #  5.2  [1] (⛔) */
    (0x26EA, 0x26EA), /*          ; Basic_Emoji              ; church
                       * #  5.2  [1] (⛪) */
    (0x26F2, 0x26F3), /*    ; Basic_Emoji              ; fountain
                       * #  5.2  [2] (⛲..⛳) */
    (0x26F5, 0x26F5), /*          ; Basic_Emoji              ; sailboat
                       * #  5.2  [1] (⛵) */
    (0x26FA, 0x26FA), /*          ; Basic_Emoji              ; tent
                       * #  5.2  [1] (⛺) */
    (0x26FD, 0x26FD), /*          ; Basic_Emoji              ; fuel pump
                       * #  5.2  [1] (⛽) */
    (0x2705, 0x2705), /*          ; Basic_Emoji              ; check mark button
                       * #  6.0  [1] (✅) */
    (0x270A, 0x270B), /*    ; Basic_Emoji              ; raised fist
                       * #  6.0  [2] (✊..✋) */
    (0x2728, 0x2728), /*          ; Basic_Emoji              ; sparkles
                       * #  6.0  [1] (✨) */
    (0x274C, 0x274C), /*          ; Basic_Emoji              ; cross mark
                       * #  6.0  [1] (❌) */
    (0x274E, 0x274E), /*          ; Basic_Emoji              ; cross mark button
                       * #  6.0  [1] (❎) */
    (0x2753, 0x2755), /*    ; Basic_Emoji              ; question mark
                       * #  6.0  [3] (❓..❕) */
    (0x2757, 0x2757), /*          ; Basic_Emoji              ; exclamation mark
                       * #  5.2  [1] (❗) */
    (0x2795, 0x2797), /*    ; Basic_Emoji              ; plus sign
                       * #  6.0  [3] (➕..➗) */
    (0x27B0, 0x27B0), /*          ; Basic_Emoji              ; curly loop
                       * #  6.0  [1] (➰) */
    (0x27BF, 0x27BF), /*          ; Basic_Emoji              ; double curly loop
                       * #  6.0  [1] (➿) */
    (0x2B1B, 0x2B1C), /*    ; Basic_Emoji              ; black large square
                       * #  5.1  [2] (⬛..⬜) */
    (0x2B50, 0x2B50), /*          ; Basic_Emoji              ; star
                       * #  5.1  [1] (⭐) */
    (0x2B55, 0x2B55), /*          ; Basic_Emoji              ; hollow red circle
                       * #  5.2  [1] (⭕) */
    (0x1F004, 0x1F004), /*         ; Basic_Emoji              ; mahjong red dragon
                         * #  5.1  [1] (🀄) */
    (0x1F0CF, 0x1F0CF), /*         ; Basic_Emoji              ; joker
                         * #  6.0  [1] (🃏) */
    (0x1F18E, 0x1F18E), /*         ; Basic_Emoji              ; AB button (blood type)
                         * #  6.0  [1] (🆎) */
    (0x1F191, 0x1F19A), /*  ; Basic_Emoji              ; CL button
                         * #  6.0 [10] (🆑..🆚) */
    (0x1F201, 0x1F201), /*         ; Basic_Emoji              ; Japanese “here” button
                         * #  6.0  [1] (🈁) */
    (0x1F21A, 0x1F21A), /*         ; Basic_Emoji              ; Japanese “free of charge” button
                         * #  5.2  [1] (🈚) */
    (0x1F22F, 0x1F22F), /*         ; Basic_Emoji              ; Japanese “reserved” button
                         * #  5.2  [1] (🈯) */
    (0x1F232, 0x1F236), /*  ; Basic_Emoji              ; Japanese “prohibited” button
                         * #  6.0  [5] (🈲..🈶) */
    (0x1F238, 0x1F23A), /*  ; Basic_Emoji              ; Japanese “application” button
                         * #  6.0  [3] (🈸..🈺) */
    (0x1F250, 0x1F251), /*  ; Basic_Emoji              ; Japanese “bargain” button
                         * #  6.0  [2] (🉐..🉑) */
    (0x1F300, 0x1F320), /*  ; Basic_Emoji              ; cyclone
                         * #  6.0 [33] (🌀..🌠) */
    (0x1F32D, 0x1F32F), /*  ; Basic_Emoji              ; hot dog
                         * #  8.0  [3] (🌭..🌯) */
    (0x1F330, 0x1F335), /*  ; Basic_Emoji              ; chestnut
                         * #  6.0  [6] (🌰..🌵) */
    (0x1F337, 0x1F37C), /*  ; Basic_Emoji              ; tulip
                         * #  6.0 [70] (🌷..🍼) */
    (0x1F37E, 0x1F37F), /*  ; Basic_Emoji              ; bottle with popping cork
                         * #  8.0  [2] (🍾..🍿) */
    (0x1F380, 0x1F393), /*  ; Basic_Emoji              ; ribbon
                         * #  6.0 [20] (🎀..🎓) */
    (0x1F3A0, 0x1F3C4), /*  ; Basic_Emoji              ; carousel horse
                         * #  6.0 [37] (🎠..🏄) */
    (0x1F3C5, 0x1F3C5), /*         ; Basic_Emoji              ; sports medal
                         * #  7.0  [1] (🏅) */
    (0x1F3C6, 0x1F3CA), /*  ; Basic_Emoji              ; trophy
                         * #  6.0  [5] (🏆..🏊) */
    (0x1F3CF, 0x1F3D3), /*  ; Basic_Emoji              ; cricket game
                         * #  8.0  [5] (🏏..🏓) */
    (0x1F3E0, 0x1F3F0), /*  ; Basic_Emoji              ; house
                         * #  6.0 [17] (🏠..🏰) */
    (0x1F3F4, 0x1F3F4), /*         ; Basic_Emoji              ; black flag
                         * #  7.0  [1] (🏴) */
    (0x1F3F8, 0x1F3FF), /*  ; Basic_Emoji              ; badminton
                         * #  8.0  [8] (🏸..🏿) */
    (0x1F400, 0x1F43E), /*  ; Basic_Emoji              ; rat
                         * #  6.0 [63] (🐀..🐾) */
    (0x1F440, 0x1F440), /*         ; Basic_Emoji              ; eyes
                         * #  6.0  [1] (👀) */
    (0x1F442, 0x1F4F7), /*  ; Basic_Emoji              ; ear
                         * #  6.0[182] (👂..📷) */
    (0x1F4F8, 0x1F4F8), /*         ; Basic_Emoji              ; camera with flash
                         * #  7.0  [1] (📸) */
    (0x1F4F9, 0x1F4FC), /*  ; Basic_Emoji              ; video camera
                         * #  6.0  [4] (📹..📼) */
    (0x1F4FF, 0x1F4FF), /*         ; Basic_Emoji              ; prayer beads
                         * #  8.0  [1] (📿) */
    (0x1F500, 0x1F53D), /*  ; Basic_Emoji              ; shuffle tracks button
                         * #  6.0 [62] (🔀..🔽) */
    (0x1F54B, 0x1F54E), /*  ; Basic_Emoji              ; kaaba
                         * #  8.0  [4] (🕋..🕎) */
    (0x1F550, 0x1F567), /*  ; Basic_Emoji              ; one o’clock
                         * #  6.0 [24] (🕐..🕧) */
    (0x1F57A, 0x1F57A), /*         ; Basic_Emoji              ; man dancing
                         * #  9.0  [1] (🕺) */
    (0x1F595, 0x1F596), /*  ; Basic_Emoji              ; middle finger
                         * #  7.0  [2] (🖕..🖖) */
    (0x1F5A4, 0x1F5A4), /*         ; Basic_Emoji              ; black heart
                         * #  9.0  [1] (🖤) */
    (0x1F5FB, 0x1F5FF), /*  ; Basic_Emoji              ; mount fuji
                         * #  6.0  [5] (🗻..🗿) */
    (0x1F600, 0x1F600), /*         ; Basic_Emoji              ; grinning face
                         * #  6.1  [1] (😀) */
    (0x1F601, 0x1F610), /*  ; Basic_Emoji              ; beaming face with smiling eyes
                         * #  6.0 [16] (😁..😐) */
    (0x1F611, 0x1F611), /*         ; Basic_Emoji              ; expressionless face
                         * #  6.1  [1] (😑) */
    (0x1F612, 0x1F614), /*  ; Basic_Emoji              ; unamused face
                         * #  6.0  [3] (😒..😔) */
    (0x1F615, 0x1F615), /*         ; Basic_Emoji              ; confused face
                         * #  6.1  [1] (😕) */
    (0x1F616, 0x1F616), /*         ; Basic_Emoji              ; confounded face
                         * #  6.0  [1] (😖) */
    (0x1F617, 0x1F617), /*         ; Basic_Emoji              ; kissing face
                         * #  6.1  [1] (😗) */
    (0x1F618, 0x1F618), /*         ; Basic_Emoji              ; face blowing a kiss
                         * #  6.0  [1] (😘) */
    (0x1F619, 0x1F619), /*         ; Basic_Emoji              ; kissing face with smiling eyes
                         * #  6.1  [1] (😙) */
    (0x1F61A, 0x1F61A), /*         ; Basic_Emoji              ; kissing face with closed eyes
                         * #  6.0  [1] (😚) */
    (0x1F61B, 0x1F61B), /*         ; Basic_Emoji              ; face with tongue
                         * #  6.1  [1] (😛) */
    (0x1F61C, 0x1F61E), /*  ; Basic_Emoji              ; winking face with tongue
                         * #  6.0  [3] (😜..😞) */
    (0x1F61F, 0x1F61F), /*         ; Basic_Emoji              ; worried face
                         * #  6.1  [1] (😟) */
    (0x1F620, 0x1F625), /*  ; Basic_Emoji              ; angry face
                         * #  6.0  [6] (😠..😥) */
    (0x1F626, 0x1F627), /*  ; Basic_Emoji              ; frowning face with open mouth
                         * #  6.1  [2] (😦..😧) */
    (0x1F628, 0x1F62B), /*  ; Basic_Emoji              ; fearful face
                         * #  6.0  [4] (😨..😫) */
    (0x1F62C, 0x1F62C), /*         ; Basic_Emoji              ; grimacing face
                         * #  6.1  [1] (😬) */
    (0x1F62D, 0x1F62D), /*         ; Basic_Emoji              ; loudly crying face
                         * #  6.0  [1] (😭) */
    (0x1F62E, 0x1F62F), /*  ; Basic_Emoji              ; face with open mouth
                         * #  6.1  [2] (😮..😯) */
    (0x1F630, 0x1F633), /*  ; Basic_Emoji              ; anxious face with sweat
                         * #  6.0  [4] (😰..😳) */
    (0x1F634, 0x1F634), /*         ; Basic_Emoji              ; sleeping face
                         * #  6.1  [1] (😴) */
    (0x1F635, 0x1F640), /*  ; Basic_Emoji              ; dizzy face
                         * #  6.0 [12] (😵..🙀) */
    (0x1F641, 0x1F642), /*  ; Basic_Emoji              ; slightly frowning face
                         * #  7.0  [2] (🙁..🙂) */
    (0x1F643, 0x1F644), /*  ; Basic_Emoji              ; upside-down face
                         * #  8.0  [2] (🙃..🙄) */
    (0x1F645, 0x1F64F), /*  ; Basic_Emoji              ; person gesturing NO
                         * #  6.0 [11] (🙅..🙏) */
    (0x1F680, 0x1F6C5), /*  ; Basic_Emoji              ; rocket
                         * #  6.0 [70] (🚀..🛅) */
    (0x1F6CC, 0x1F6CC), /*         ; Basic_Emoji              ; person in bed
                         * #  7.0  [1] (🛌) */
    (0x1F6D0, 0x1F6D0), /*         ; Basic_Emoji              ; place of worship
                         * #  8.0  [1] (🛐) */
    (0x1F6D1, 0x1F6D2), /*  ; Basic_Emoji              ; stop sign
                         * #  9.0  [2] (🛑..🛒) */
    (0x1F6D5, 0x1F6D5), /*         ; Basic_Emoji              ; hindu temple
                         * # 12.0  [1] (🛕) */
    (0x1F6EB, 0x1F6EC), /*  ; Basic_Emoji              ; airplane departure
                         * #  7.0  [2] (🛫..🛬) */
    (0x1F6F4, 0x1F6F6), /*  ; Basic_Emoji              ; kick scooter
                         * #  9.0  [3] (🛴..🛶) */
    (0x1F6F7, 0x1F6F8), /*  ; Basic_Emoji              ; sled
                         * # 10.0  [2] (🛷..🛸) */
    (0x1F6F9, 0x1F6F9), /*         ; Basic_Emoji              ; skateboard
                         * # 11.0  [1] (🛹) */
    (0x1F6FA, 0x1F6FA), /*         ; Basic_Emoji              ; auto rickshaw
                         * # 12.0  [1] (🛺) */
    (0x1F7E0, 0x1F7EB), /*  ; Basic_Emoji              ; orange circle
                         * # 12.0 [12] (🟠..🟫) */
    (0x1F90D, 0x1F90F), /*  ; Basic_Emoji              ; white heart
                         * # 12.0  [3] (🤍..🤏) */
    (0x1F910, 0x1F918), /*  ; Basic_Emoji              ; zipper-mouth face
                         * #  8.0  [9] (🤐..🤘) */
    (0x1F919, 0x1F91E), /*  ; Basic_Emoji              ; call me hand
                         * #  9.0  [6] (🤙..🤞) */
    (0x1F91F, 0x1F91F), /*         ; Basic_Emoji              ; love-you gesture
                         * # 10.0  [1] (🤟) */
    (0x1F920, 0x1F927), /*  ; Basic_Emoji              ; cowboy hat face
                         * #  9.0  [8] (🤠..🤧) */
    (0x1F928, 0x1F92F), /*  ; Basic_Emoji              ; face with raised eyebrow
                         * # 10.0  [8] (🤨..🤯) */
    (0x1F930, 0x1F930), /*         ; Basic_Emoji              ; pregnant woman
                         * #  9.0  [1] (🤰) */
    (0x1F931, 0x1F932), /*  ; Basic_Emoji              ; breast-feeding
                         * # 10.0  [2] (🤱..🤲) */
    (0x1F933, 0x1F93A), /*  ; Basic_Emoji              ; selfie
                         * #  9.0  [8] (🤳..🤺) */
    (0x1F93C, 0x1F93E), /*  ; Basic_Emoji              ; people wrestling
                         * #  9.0  [3] (🤼..🤾) */
    (0x1F93F, 0x1F93F), /*         ; Basic_Emoji              ; diving mask
                         * # 12.0  [1] (🤿) */
    (0x1F940, 0x1F945), /*  ; Basic_Emoji              ; wilted flower
                         * #  9.0  [6] (🥀..🥅) */
    (0x1F947, 0x1F94B), /*  ; Basic_Emoji              ; 1st place medal
                         * #  9.0  [5] (🥇..🥋) */
    (0x1F94C, 0x1F94C), /*         ; Basic_Emoji              ; curling stone
                         * # 10.0  [1] (🥌) */
    (0x1F94D, 0x1F94F), /*  ; Basic_Emoji              ; lacrosse
                         * # 11.0  [3] (🥍..🥏) */
    (0x1F950, 0x1F95E), /*  ; Basic_Emoji              ; croissant
                         * #  9.0 [15] (🥐..🥞) */
    (0x1F95F, 0x1F96B), /*  ; Basic_Emoji              ; dumpling
                         * # 10.0 [13] (🥟..🥫) */
    (0x1F96C, 0x1F970), /*  ; Basic_Emoji              ; leafy green
                         * # 11.0  [5] (🥬..🥰) */
    (0x1F971, 0x1F971), /*         ; Basic_Emoji              ; yawning face
                         * # 12.0  [1] (🥱) */
    (0x1F973, 0x1F976), /*  ; Basic_Emoji              ; partying face
                         * # 11.0  [4] (🥳..🥶) */
    (0x1F97A, 0x1F97A), /*         ; Basic_Emoji              ; pleading face
                         * # 11.0  [1] (🥺) */
    (0x1F97B, 0x1F97B), /*         ; Basic_Emoji              ; sari
                         * # 12.0  [1] (🥻) */
    (0x1F97C, 0x1F97F), /*  ; Basic_Emoji              ; lab coat
                         * # 11.0  [4] (🥼..🥿) */
    (0x1F980, 0x1F984), /*  ; Basic_Emoji              ; crab
                         * #  8.0  [5] (🦀..🦄) */
    (0x1F985, 0x1F991), /*  ; Basic_Emoji              ; eagle
                         * #  9.0 [13] (🦅..🦑) */
    (0x1F992, 0x1F997), /*  ; Basic_Emoji              ; giraffe
                         * # 10.0  [6] (🦒..🦗) */
    (0x1F998, 0x1F9A2), /*  ; Basic_Emoji              ; kangaroo
                         * # 11.0 [11] (🦘..🦢) */
    (0x1F9A5, 0x1F9AA), /*  ; Basic_Emoji              ; sloth
                         * # 12.0  [6] (🦥..🦪) */
    (0x1F9AE, 0x1F9AF), /*  ; Basic_Emoji              ; guide dog
                         * # 12.0  [2] (🦮..🦯) */
    (0x1F9B0, 0x1F9B9), /*  ; Basic_Emoji              ; red hair
                         * # 11.0 [10] (🦰..🦹) */
    (0x1F9BA, 0x1F9BF), /*  ; Basic_Emoji              ; safety vest
                         * # 12.0  [6] (🦺..🦿) */
    (0x1F9C0, 0x1F9C0), /*         ; Basic_Emoji              ; cheese wedge
                         * #  8.0  [1] (🧀) */
    (0x1F9C1, 0x1F9C2), /*  ; Basic_Emoji              ; cupcake
                         * # 11.0  [2] (🧁..🧂) */
    (0x1F9C3, 0x1F9CA), /*  ; Basic_Emoji              ; beverage box
                         * # 12.0  [8] (🧃..🧊) */
    (0x1F9CD, 0x1F9CF), /*  ; Basic_Emoji              ; person standing
                         * # 12.0  [3] (🧍..🧏) */
    (0x1F9D0, 0x1F9E6), /*  ; Basic_Emoji              ; face with monocle
                         * # 10.0 [23] (🧐..🧦) */
    (0x1F9E7, 0x1F9FF), /*  ; Basic_Emoji              ; red envelope
                         * # 11.0 [25] (🧧..🧿) */
    (0x1FA70, 0x1FA73), /*  ; Basic_Emoji              ; ballet shoes
                         * # 12.0  [4] (🩰..🩳) */
    (0x1FA78, 0x1FA7A), /*  ; Basic_Emoji              ; drop of blood
                         * # 12.0  [3] (🩸..🩺) */
    (0x1FA80, 0x1FA82), /*  ; Basic_Emoji              ; yo-yo
                         * # 12.0  [3] (🪀..🪂) */
    (0x1FA90, 0x1FA95), /*  ; Basic_Emoji              ; ringed planet
                         * # 12.0  [6] (🪐..🪕) */
];
// 00A9 FE0F     ; Basic_Emoji              ; copyright
// #  3.2  [1] (©️) 00AE FE0F     ; Basic_Emoji              ; registered
// #  3.2  [1] (®️) 203C FE0F     ; Basic_Emoji              ; double
// exclamation mark                                        #  3.2  [1] (‼️) 2049
// FE0F     ; Basic_Emoji              ; exclamation question mark
// #  3.2  [1] (⁉️) 2122 FE0F     ; Basic_Emoji              ; trade mark
// #  3.2  [1] (™️) 2139 FE0F     ; Basic_Emoji              ; information
// #  3.2  [1] (ℹ️) 2194 FE0F     ; Basic_Emoji              ; left-right arrow
// #  3.2  [1] (↔️) 2195 FE0F     ; Basic_Emoji              ; up-down arrow
// #  3.2  [1] (↕️) 2196 FE0F     ; Basic_Emoji              ; up-left arrow
// #  3.2  [1] (↖️) 2197 FE0F     ; Basic_Emoji              ; up-right arrow
// #  3.2  [1] (↗️) 2198 FE0F     ; Basic_Emoji              ; down-right arrow
// #  3.2  [1] (↘️) 2199 FE0F     ; Basic_Emoji              ; down-left arrow
// #  3.2  [1] (↙️) 21A9 FE0F     ; Basic_Emoji              ; right arrow
// curving left                                       #  3.2  [1] (↩️) 21AA FE0F
// ; Basic_Emoji              ; left arrow curving right
// #  3.2  [1] (↪️) 2328 FE0F     ; Basic_Emoji              ; keyboard
// #  3.2  [1] (⌨️) 23CF FE0F     ; Basic_Emoji              ; eject button
// #  4.0  [1] (⏏️) 23ED FE0F     ; Basic_Emoji              ; next track button
// #  6.0  [1] (⏭️) 23EE FE0F     ; Basic_Emoji              ; last track button
// #  6.0  [1] (⏮️) 23EF FE0F     ; Basic_Emoji              ; play or pause
// button                                           #  6.0  [1] (⏯️) 23F1 FE0F
// ; Basic_Emoji              ; stopwatch
// #  6.0  [1] (⏱️) 23F2 FE0F     ; Basic_Emoji              ; timer clock
// #  6.0  [1] (⏲️) 23F8 FE0F     ; Basic_Emoji              ; pause button
// #  7.0  [1] (⏸️) 23F9 FE0F     ; Basic_Emoji              ; stop button
// #  7.0  [1] (⏹️) 23FA FE0F     ; Basic_Emoji              ; record button
// #  7.0  [1] (⏺️) 24C2 FE0F     ; Basic_Emoji              ; circled M
// #  3.2  [1] (Ⓜ️) 25AA FE0F     ; Basic_Emoji              ; black small
// square                                             #  3.2  [1] (▪️) 25AB FE0F
// ; Basic_Emoji              ; white small square
// #  3.2  [1] (▫️) 25B6 FE0F     ; Basic_Emoji              ; play button
// #  3.2  [1] (▶️) 25C0 FE0F     ; Basic_Emoji              ; reverse button
// #  3.2  [1] (◀️) 25FB FE0F     ; Basic_Emoji              ; white medium
// square                                            #  3.2  [1] (◻️) 25FC FE0F
// ; Basic_Emoji              ; black medium square
// #  3.2  [1] (◼️) 2600 FE0F     ; Basic_Emoji              ; sun
// #  3.2  [1] (☀️) 2601 FE0F     ; Basic_Emoji              ; cloud
// #  3.2  [1] (☁️) 2602 FE0F     ; Basic_Emoji              ; umbrella
// #  3.2  [1] (☂️) 2603 FE0F     ; Basic_Emoji              ; snowman
// #  3.2  [1] (☃️) 2604 FE0F     ; Basic_Emoji              ; comet
// #  3.2  [1] (☄️) 260E FE0F     ; Basic_Emoji              ; telephone
// #  3.2  [1] (☎️) 2611 FE0F     ; Basic_Emoji              ; check box with
// check                                           #  3.2  [1] (☑️) 2618 FE0F
// ; Basic_Emoji              ; shamrock
// #  4.1  [1] (☘️) 261D FE0F     ; Basic_Emoji              ; index pointing up
// #  3.2  [1] (☝️) 2620 FE0F     ; Basic_Emoji              ; skull and
// crossbones                                           #  3.2  [1] (☠️)
// 2622 FE0F     ; Basic_Emoji              ; radioactive
// #  3.2  [1] (☢️) 2623 FE0F     ; Basic_Emoji              ; biohazard
// #  3.2  [1] (☣️) 2626 FE0F     ; Basic_Emoji              ; orthodox cross
// #  3.2  [1] (☦️) 262A FE0F     ; Basic_Emoji              ; star and crescent
// #  3.2  [1] (☪️) 262E FE0F     ; Basic_Emoji              ; peace symbol
// #  3.2  [1] (☮️) 262F FE0F     ; Basic_Emoji              ; yin yang
// #  3.2  [1] (☯️) 2638 FE0F     ; Basic_Emoji              ; wheel of dharma
// #  3.2  [1] (☸️) 2639 FE0F     ; Basic_Emoji              ; frowning face
// #  3.2  [1] (☹️) 263A FE0F     ; Basic_Emoji              ; smiling face
// #  3.2  [1] (☺️) 2640 FE0F     ; Basic_Emoji              ; female sign
// #  3.2  [1] (♀️) 2642 FE0F     ; Basic_Emoji              ; male sign
// #  3.2  [1] (♂️) 265F FE0F     ; Basic_Emoji              ; chess pawn
// #  3.2  [1] (♟️) 2660 FE0F     ; Basic_Emoji              ; spade suit
// #  3.2  [1] (♠️) 2663 FE0F     ; Basic_Emoji              ; club suit
// #  3.2  [1] (♣️) 2665 FE0F     ; Basic_Emoji              ; heart suit
// #  3.2  [1] (♥️) 2666 FE0F     ; Basic_Emoji              ; diamond suit
// #  3.2  [1] (♦️) 2668 FE0F     ; Basic_Emoji              ; hot springs
// #  3.2  [1] (♨️) 267B FE0F     ; Basic_Emoji              ; recycling symbol
// #  3.2  [1] (♻️) 267E FE0F     ; Basic_Emoji              ; infinity
// #  4.1  [1] (♾️) 2692 FE0F     ; Basic_Emoji              ; hammer and pick
// #  4.1  [1] (⚒️) 2694 FE0F     ; Basic_Emoji              ; crossed swords
// #  4.1  [1] (⚔️) 2695 FE0F     ; Basic_Emoji              ; medical symbol
// #  4.1  [1] (⚕️) 2696 FE0F     ; Basic_Emoji              ; balance scale
// #  4.1  [1] (⚖️) 2697 FE0F     ; Basic_Emoji              ; alembic
// #  4.1  [1] (⚗️) 2699 FE0F     ; Basic_Emoji              ; gear
// #  4.1  [1] (⚙️) 269B FE0F     ; Basic_Emoji              ; atom symbol
// #  4.1  [1] (⚛️) 269C FE0F     ; Basic_Emoji              ; fleur-de-lis
// #  4.1  [1] (⚜️) 26A0 FE0F     ; Basic_Emoji              ; warning
// #  4.0  [1] (⚠️) 26B0 FE0F     ; Basic_Emoji              ; coffin
// #  4.1  [1] (⚰️) 26B1 FE0F     ; Basic_Emoji              ; funeral urn
// #  4.1  [1] (⚱️) 26C8 FE0F     ; Basic_Emoji              ; cloud with
// lightning and rain                                  #  5.2  [1] (⛈️)
// 26CF FE0F     ; Basic_Emoji              ; pick
// #  5.2  [1] (⛏️) 26D1 FE0F     ; Basic_Emoji              ; rescue worker’s
// helmet                                         #  5.2  [1] (⛑️) 26D3 FE0F
// ; Basic_Emoji              ; chains
// #  5.2  [1] (⛓️) 26E9 FE0F     ; Basic_Emoji              ; shinto shrine
// #  5.2  [1] (⛩️) 26F0 FE0F     ; Basic_Emoji              ; mountain
// #  5.2  [1] (⛰️) 26F1 FE0F     ; Basic_Emoji              ; umbrella on
// ground                                             #  5.2  [1] (⛱️) 26F4 FE0F
// ; Basic_Emoji              ; ferry
// #  5.2  [1] (⛴️) 26F7 FE0F     ; Basic_Emoji              ; skier
// #  5.2  [1] (⛷️) 26F8 FE0F     ; Basic_Emoji              ; ice skate
// #  5.2  [1] (⛸️) 26F9 FE0F     ; Basic_Emoji              ; person bouncing
// ball                                           #  5.2  [1] (⛹️) 2702 FE0F
// ; Basic_Emoji              ; scissors
// #  3.2  [1] (✂️) 2708 FE0F     ; Basic_Emoji              ; airplane
// #  3.2  [1] (✈️) 2709 FE0F     ; Basic_Emoji              ; envelope
// #  3.2  [1] (✉️) 270C FE0F     ; Basic_Emoji              ; victory hand
// #  3.2  [1] (✌️) 270D FE0F     ; Basic_Emoji              ; writing hand
// #  3.2  [1] (✍️) 270F FE0F     ; Basic_Emoji              ; pencil
// #  3.2  [1] (✏️) 2712 FE0F     ; Basic_Emoji              ; black nib
// #  3.2  [1] (✒️) 2714 FE0F     ; Basic_Emoji              ; check mark
// #  3.2  [1] (✔️) 2716 FE0F     ; Basic_Emoji              ; multiplication
// sign                                            #  3.2  [1] (✖️) 271D FE0F
// ; Basic_Emoji              ; latin cross
// #  3.2  [1] (✝️) 2721 FE0F     ; Basic_Emoji              ; star of David
// #  3.2  [1] (✡️) 2733 FE0F     ; Basic_Emoji              ; eight-spoked
// asterisk                                          #  3.2  [1] (✳️) 2734 FE0F
// ; Basic_Emoji              ; eight-pointed star
// #  3.2  [1] (✴️) 2744 FE0F     ; Basic_Emoji              ; snowflake
// #  3.2  [1] (❄️) 2747 FE0F     ; Basic_Emoji              ; sparkle
// #  3.2  [1] (❇️) 2763 FE0F     ; Basic_Emoji              ; heart exclamation
// #  3.2  [1] (❣️) 2764 FE0F     ; Basic_Emoji              ; red heart
// #  3.2  [1] (❤️) 27A1 FE0F     ; Basic_Emoji              ; right arrow
// #  3.2  [1] (➡️) 2934 FE0F     ; Basic_Emoji              ; right arrow
// curving up                                         #  3.2  [1] (⤴️) 2935 FE0F
// ; Basic_Emoji              ; right arrow curving down
// #  3.2  [1] (⤵️) 2B05 FE0F     ; Basic_Emoji              ; left arrow
// #  4.0  [1] (⬅️) 2B06 FE0F     ; Basic_Emoji              ; up arrow
// #  4.0  [1] (⬆️) 2B07 FE0F     ; Basic_Emoji              ; down arrow
// #  4.0  [1] (⬇️) 3030 FE0F     ; Basic_Emoji              ; wavy dash
// #  3.2  [1] (〰️) 303D FE0F     ; Basic_Emoji              ; part alternation
// mark                                          #  3.2  [1] (〽️) 3297 FE0F
// ; Basic_Emoji              ; Japanese “congratulations” button
// #  3.2  [1] (㊗️) 3299 FE0F     ; Basic_Emoji              ; Japanese “secret”
// button                                       #  3.2  [1] (㊙️) 1F170 FE0F    ;
// Basic_Emoji              ; A button (blood type)
// #  6.0  [1] (🅰️) 1F171 FE0F    ; Basic_Emoji              ; B button (blood
// type)                                          #  6.0  [1] (🅱️) 1F17E FE0F
// ; Basic_Emoji              ; O button (blood type)
// #  6.0  [1] (🅾️) 1F17F FE0F    ; Basic_Emoji              ; P button
// #  5.2  [1] (🅿️) 1F202 FE0F    ; Basic_Emoji              ; Japanese “service
// charge” button                               #  6.0  [1] (🈂️) 1F237 FE0F    ;
// Basic_Emoji              ; Japanese “monthly amount” button
// #  6.0  [1] (🈷️) 1F321 FE0F    ; Basic_Emoji              ; thermometer
// #  7.0  [1] (🌡️) 1F324 FE0F    ; Basic_Emoji              ; sun behind small
// cloud                                         #  7.0  [1] (🌤️) 1F325 FE0F
// ; Basic_Emoji              ; sun behind large cloud
// #  7.0  [1] (🌥️) 1F326 FE0F    ; Basic_Emoji              ; sun behind rain
// cloud                                          #  7.0  [1] (🌦️) 1F327 FE0F
// ; Basic_Emoji              ; cloud with rain
// #  7.0  [1] (🌧️) 1F328 FE0F    ; Basic_Emoji              ; cloud with snow
// #  7.0  [1] (🌨️) 1F329 FE0F    ; Basic_Emoji              ; cloud with
// lightning                                           #  7.0  [1] (🌩️)
// 1F32A FE0F    ; Basic_Emoji              ; tornado
// #  7.0  [1] (🌪️) 1F32B FE0F    ; Basic_Emoji              ; fog
// #  7.0  [1] (🌫️) 1F32C FE0F    ; Basic_Emoji              ; wind face
// #  7.0  [1] (🌬️) 1F336 FE0F    ; Basic_Emoji              ; hot pepper
// #  7.0  [1] (🌶️) 1F37D FE0F    ; Basic_Emoji              ; fork and knife
// with plate                                      #  7.0  [1] (🍽️) 1F396 FE0F
// ; Basic_Emoji              ; military medal
// #  7.0  [1] (🎖️) 1F397 FE0F    ; Basic_Emoji              ; reminder ribbon
// #  7.0  [1] (🎗️) 1F399 FE0F    ; Basic_Emoji              ; studio microphone
// #  7.0  [1] (🎙️) 1F39A FE0F    ; Basic_Emoji              ; level slider
// #  7.0  [1] (🎚️) 1F39B FE0F    ; Basic_Emoji              ; control knobs
// #  7.0  [1] (🎛️) 1F39E FE0F    ; Basic_Emoji              ; film frames
// #  7.0  [1] (🎞️) 1F39F FE0F    ; Basic_Emoji              ; admission tickets
// #  7.0  [1] (🎟️) 1F3CB FE0F    ; Basic_Emoji              ; person lifting
// weights                                         #  7.0  [1] (🏋️) 1F3CC FE0F
// ; Basic_Emoji              ; person golfing
// #  7.0  [1] (🏌️) 1F3CD FE0F    ; Basic_Emoji              ; motorcycle
// #  7.0  [1] (🏍️) 1F3CE FE0F    ; Basic_Emoji              ; racing car
// #  7.0  [1] (🏎️) 1F3D4 FE0F    ; Basic_Emoji              ; snow-capped
// mountain                                           #  7.0  [1] (🏔️)
// 1F3D5 FE0F    ; Basic_Emoji              ; camping
// #  7.0  [1] (🏕️) 1F3D6 FE0F    ; Basic_Emoji              ; beach with
// umbrella                                            #  7.0  [1] (🏖️)
// 1F3D7 FE0F    ; Basic_Emoji              ; building construction
// #  7.0  [1] (🏗️) 1F3D8 FE0F    ; Basic_Emoji              ; houses
// #  7.0  [1] (🏘️) 1F3D9 FE0F    ; Basic_Emoji              ; cityscape
// #  7.0  [1] (🏙️) 1F3DA FE0F    ; Basic_Emoji              ; derelict house
// #  7.0  [1] (🏚️) 1F3DB FE0F    ; Basic_Emoji              ; classical
// building                                             #  7.0  [1] (🏛️)
// 1F3DC FE0F    ; Basic_Emoji              ; desert
// #  7.0  [1] (🏜️) 1F3DD FE0F    ; Basic_Emoji              ; desert island
// #  7.0  [1] (🏝️) 1F3DE FE0F    ; Basic_Emoji              ; national park
// #  7.0  [1] (🏞️) 1F3DF FE0F    ; Basic_Emoji              ; stadium
// #  7.0  [1] (🏟️) 1F3F3 FE0F    ; Basic_Emoji              ; white flag
// #  7.0  [1] (🏳️) 1F3F5 FE0F    ; Basic_Emoji              ; rosette
// #  7.0  [1] (🏵️) 1F3F7 FE0F    ; Basic_Emoji              ; label
// #  7.0  [1] (🏷️) 1F43F FE0F    ; Basic_Emoji              ; chipmunk
// #  7.0  [1] (🐿️) 1F441 FE0F    ; Basic_Emoji              ; eye
// #  7.0  [1] (👁️) 1F4FD FE0F    ; Basic_Emoji              ; film projector
// #  7.0  [1] (📽️) 1F549 FE0F    ; Basic_Emoji              ; om
// #  7.0  [1] (🕉️) 1F54A FE0F    ; Basic_Emoji              ; dove
// #  7.0  [1] (🕊️) 1F56F FE0F    ; Basic_Emoji              ; candle
// #  7.0  [1] (🕯️) 1F570 FE0F    ; Basic_Emoji              ; mantelpiece clock
// #  7.0  [1] (🕰️) 1F573 FE0F    ; Basic_Emoji              ; hole
// #  7.0  [1] (🕳️) 1F574 FE0F    ; Basic_Emoji              ; man in suit
// levitating                                         #  7.0  [1] (🕴️)
// 1F575 FE0F    ; Basic_Emoji              ; detective
// #  7.0  [1] (🕵️) 1F576 FE0F    ; Basic_Emoji              ; sunglasses
// #  7.0  [1] (🕶️) 1F577 FE0F    ; Basic_Emoji              ; spider
// #  7.0  [1] (🕷️) 1F578 FE0F    ; Basic_Emoji              ; spider web
// #  7.0  [1] (🕸️) 1F579 FE0F    ; Basic_Emoji              ; joystick
// #  7.0  [1] (🕹️) 1F587 FE0F    ; Basic_Emoji              ; linked paperclips
// #  7.0  [1] (🖇️) 1F58A FE0F    ; Basic_Emoji              ; pen
// #  7.0  [1] (🖊️) 1F58B FE0F    ; Basic_Emoji              ; fountain pen
// #  7.0  [1] (🖋️) 1F58C FE0F    ; Basic_Emoji              ; paintbrush
// #  7.0  [1] (🖌️) 1F58D FE0F    ; Basic_Emoji              ; crayon
// #  7.0  [1] (🖍️) 1F590 FE0F    ; Basic_Emoji              ; hand with fingers
// splayed                                      #  7.0  [1] (🖐️) 1F5A5 FE0F    ;
// Basic_Emoji              ; desktop computer
// #  7.0  [1] (🖥️) 1F5A8 FE0F    ; Basic_Emoji              ; printer
// #  7.0  [1] (🖨️) 1F5B1 FE0F    ; Basic_Emoji              ; computer mouse
// #  7.0  [1] (🖱️) 1F5B2 FE0F    ; Basic_Emoji              ; trackball
// #  7.0  [1] (🖲️) 1F5BC FE0F    ; Basic_Emoji              ; framed picture
// #  7.0  [1] (🖼️) 1F5C2 FE0F    ; Basic_Emoji              ; card index
// dividers                                            #  7.0  [1] (🗂️)
// 1F5C3 FE0F    ; Basic_Emoji              ; card file box
// #  7.0  [1] (🗃️) 1F5C4 FE0F    ; Basic_Emoji              ; file cabinet
// #  7.0  [1] (🗄️) 1F5D1 FE0F    ; Basic_Emoji              ; wastebasket
// #  7.0  [1] (🗑️) 1F5D2 FE0F    ; Basic_Emoji              ; spiral notepad
// #  7.0  [1] (🗒️) 1F5D3 FE0F    ; Basic_Emoji              ; spiral calendar
// #  7.0  [1] (🗓️) 1F5DC FE0F    ; Basic_Emoji              ; clamp
// #  7.0  [1] (🗜️) 1F5DD FE0F    ; Basic_Emoji              ; old key
// #  7.0  [1] (🗝️) 1F5DE FE0F    ; Basic_Emoji              ; rolled-up
// newspaper                                            #  7.0  [1] (🗞️)
// 1F5E1 FE0F    ; Basic_Emoji              ; dagger
// #  7.0  [1] (🗡️) 1F5E3 FE0F    ; Basic_Emoji              ; speaking head
// #  7.0  [1] (🗣️) 1F5E8 FE0F    ; Basic_Emoji              ; left speech
// bubble                                             #  7.0  [1] (🗨️)
// 1F5EF FE0F    ; Basic_Emoji              ; right anger bubble
// #  7.0  [1] (🗯️) 1F5F3 FE0F    ; Basic_Emoji              ; ballot box with
// ballot                                         #  7.0  [1] (🗳️) 1F5FA FE0F
// ; Basic_Emoji              ; world map
// #  7.0  [1] (🗺️) 1F6CB FE0F    ; Basic_Emoji              ; couch and lamp
// #  7.0  [1] (🛋️) 1F6CD FE0F    ; Basic_Emoji              ; shopping bags
// #  7.0  [1] (🛍️) 1F6CE FE0F    ; Basic_Emoji              ; bellhop bell
// #  7.0  [1] (🛎️) 1F6CF FE0F    ; Basic_Emoji              ; bed
// #  7.0  [1] (🛏️) 1F6E0 FE0F    ; Basic_Emoji              ; hammer and wrench
// #  7.0  [1] (🛠️) 1F6E1 FE0F    ; Basic_Emoji              ; shield
// #  7.0  [1] (🛡️) 1F6E2 FE0F    ; Basic_Emoji              ; oil drum
// #  7.0  [1] (🛢️) 1F6E3 FE0F    ; Basic_Emoji              ; motorway
// #  7.0  [1] (🛣️) 1F6E4 FE0F    ; Basic_Emoji              ; railway track
// #  7.0  [1] (🛤️) 1F6E5 FE0F    ; Basic_Emoji              ; motor boat
// #  7.0  [1] (🛥️) 1F6E9 FE0F    ; Basic_Emoji              ; small airplane
// #  7.0  [1] (🛩️) 1F6F0 FE0F    ; Basic_Emoji              ; satellite
// #  7.0  [1] (🛰️) 1F6F3 FE0F    ; Basic_Emoji              ; passenger ship
// #  7.0  [1] (🛳️)
