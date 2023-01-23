pub(crate) mod comment;
pub(crate) mod file;
pub(crate) mod nlsplice;
pub(crate) mod trigraph;

// 1. Physical source file multibyte characters are mapped, in an implementation
// defined manner, to the source character set (introducing new-line characters for
// end-of-line indicators) if necessary. Trigraph sequences are replaced by
// corresponding single-character internal representations.

// 2. Each instance of a backslash character (\) immediately followed by a new-line
// character is deleted, splicing physical source lines to form logical source lines.
// Only the last backslash on any physical source line shall be eligible for being part
// of such a splice. A source file that is not empty shall end in a new-line character,
// which shall not be immediately preceded by a backslash character before any such
// splicing takes place

// 3. The source file is decomposed into preprocessing tokens(6) and sequences of
// white-space characters (including comments). A source file shall not end in a
// partial preprocessing token or in a partial comment. Each comment is replaced by
// one space character. New-line characters are retained. Whether each nonempty
// sequence of white-space characters other than new-line is retained or replaced by
// one space character is implementation-defined.
