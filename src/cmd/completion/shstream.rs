// SPDX-License-Identifier: GPL-2.0-only

//! Support for generating shell scripts.

/// Build-up a shell script piece-by-piece.
///
/// The script is composed by inserting lines, space-separated words, and/or raw strings
/// in the order they should appear in the script. This light-weight interface mostly
/// just helps with maintaining whitespace: newlines, spaces between words, and
/// indentation.
pub(crate) struct ShStream {
    buffer: String,
    indent_level: usize,
    word_sep: char,
}

impl ShStream {
    /// Create a new [`ShStream`] instance.
    pub fn new() -> Self {
        Self {
            buffer: String::new(),
            indent_level: 0,
            word_sep: ' ',
        }
    }

    /// Insert a raw string into the stream.
    pub fn raw(&mut self, s: &str) {
        if !self.is_bol() {
            self.buffer.push('\n');
        }
        self.buffer.push_str(s);
    }

    /// Increase the indentation level used for subsequent lines.
    pub fn indent(&mut self) {
        self.indent_level += 1;
    }

    /// Decrease the indentation level used for subsequent lines.
    pub fn dedent(&mut self) {
        assert!(self.indent_level > 0);
        self.indent_level -= 1;
    }

    /// Insert a line into the stream.
    ///
    /// The line is prefixed with indentation and suffixed with a newline character.
    pub fn line(&mut self, line: &str) {
        if !self.is_bol() {
            self.buffer.push('\n');
        }
        if !line.is_empty() {
            self.push_indent();
            self.buffer.push_str(line);
        }
        self.buffer.push('\n');
    }

    /// Ensure a blank line precedes subsequent lines.
    pub fn ensure_blank_line(&mut self) {
        if !self.buffer.is_empty() && !self.buffer.ends_with("\n\n") {
            if self.buffer.ends_with('\n') {
                self.buffer.push('\n');
            } else {
                self.buffer.push_str("\n\n");
            }
        }
    }

    /// Insert slice of lines into the stream.
    ///
    /// Each line is prefixed with the current indentation level.
    pub fn lines(&mut self, lines: &[&str]) {
        for line in lines {
            self.line(line);
        }
    }

    /// Insert a word into the stream.
    ///
    /// If the word starts a line, it will be indented to the current indentation level.
    /// Otherwise, a single space will be used to separate this word from any preceding
    /// word.
    pub fn word(&mut self, word: &str) {
        if self.is_bol() {
            self.push_indent();
        } else if !self.buffer.ends_with(self.word_sep) {
            self.buffer.push(self.word_sep);
        }
        self.buffer.push_str(word);
    }

    /// Set the inter-word separator character.
    pub fn word_sep(&mut self, separator: char) {
        self.word_sep = separator;
    }

    /// Return byte slice of the stream's contents.
    pub fn as_bytes(&self) -> &[u8] {
        self.buffer.as_bytes()
    }

    /// Return whether the stream is at the beginning of a line.
    fn is_bol(&self) -> bool {
        self.buffer.is_empty() || self.buffer.ends_with('\n')
    }

    /// Insert indentation into the stream.
    fn push_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.buffer.push_str("    ");
        }
    }
}

impl AsRef<str> for ShStream {
    fn as_ref(&self) -> &str {
        &self.buffer
    }
}

impl std::fmt::Display for ShStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.buffer.fmt(f)
    }
}
