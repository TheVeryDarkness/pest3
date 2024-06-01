//! This is a replacement for [pest2::Position],
//! which provides a more general interface that stands for a cursor.

use std::ops::RangeBounds;

pub trait Cursor<'i> {
    /// Get byte offset since the begin.
    ///
    /// Please maintain a counter if you are implementing this.
    fn get_offset(&self) -> usize;
    /// Get line number and column number.
    ///
    /// This is helpful in error diagnostics,
    /// so please maintain a counter if you are implementing this.
    fn get_pos(&self) -> (usize, usize);
    /// Check if the cursor is at the start of the input.
    fn at_start(&self) -> bool;
    /// Check if the cursor is at the end of the input.
    fn at_end(&self) -> bool;
    /// Match a character.
    fn match_char(&mut self, c: char) -> bool {
        self.match_char_by(|c_| c == c_)
    }
    /// Match a string.
    fn match_string(&mut self, s: &str) -> bool;
    /// Match a string in case-insensitive mode.
    fn match_insensitive(&mut self, s: &str) -> bool;
    /// Match a character in a range.
    fn match_range(&mut self, range: impl RangeBounds<char>) -> bool {
        self.match_char_by(|c| range.contains(&c))
    }
    /// Match a character by a predicate,
    /// which returns whether the character should be accepted.
    fn match_char_by(&mut self, f: impl FnOnce(char) -> bool) -> bool;

    // Maintain a buffer for lines that may be referenced in error diagnostics.
    // type Lines;
    // /// Take.
    // fn take_related_lines(self) ->
}

/// We may support async stream in the future.
pub trait AsyncCursor {}
