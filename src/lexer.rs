/*
 * This file is part of rsdb.
 *
 * rsdb is free software: you can redistribute it and/or modify it under the terms 
 * of the GNU General Public License as published by the Free Software Foundation, 
 * either version 3 of the License, or (at your option) any later version.
 *
 * rsdb is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with rsdb. 
 * If not, see <https://www.gnu.org/licenses/>. 
 *
*/

use crate::regex::{Automaton, Error as RegexError, Matcher};
use crate::settings::Settings;

use std::fs::File;

///
/// A token
///
#[derive(Debug, PartialEq)]
pub struct Token {
    ///
    /// The token kind
    ///
    kind: usize,

    ///
    /// The position of the token in the buffer
    ///
    pos: usize,

    ///
    /// The token length
    ///
    len: usize,
}

impl Token {
    ///
    /// Returns the ID of the kind of the token
    ///
    pub fn kind(&self) -> usize {
	self.kind
    }

    ///
    /// Returns the position of the token in the stream
    ///
    pub fn pos(&self) -> usize {
	self.pos
    }

    ///
    /// Returns the length of the token in characters
    ///
    pub fn len(&self) -> usize {
	self.len
    }
}


///
/// The (thread safe) lexer factory
///
pub struct LexerFactory {
    ///
    /// The regex automaton
    ///
    automaton: Automaton,
}

impl LexerFactory {
    ///
    /// Constructs a new lexer factory from settings
    ///
    pub fn new(settings: &Settings) -> Result<LexerFactory, Error> {
	let mut path = settings.create_config_path();
	path.push("sql.regex");
	let file = File::open(path.clone()).map_err(|_| Error::IO)?;
	LexerFactory::from_file(&file)
    }

    ///
    /// Constructs a new lecer factory from a file
    ///
    pub fn from_file(file: &File) -> Result<LexerFactory, Error> {
	LexerFactory::from_str(&std::io::read_to_string(file).map_err(|_| Error::IO)?)
    }
    
    ///
    /// Constructs a new lexer factory from a definition string
    ///
    pub fn from_str(input: &str) -> Result<LexerFactory, Error> {
	let automaton = Automaton::from_str(input)?;
	Ok(LexerFactory {
	    automaton,
	})
    }

    ///
    /// Returns the token names
    ///
    pub fn names(&self) -> &Vec<String> {
	self.automaton.names()
    }

    ///
    /// Creates a lexer for the given input
    ///
    pub fn create_lexer<'a, 'b>(&'a self, chars: &'b Characters) -> Lexer<'a, 'b> {
	Lexer {
	    factory: self,
	    matcher: Matcher::new(&self.automaton),
	    chars,
	    pos: 0,
	}
    }
}

///
/// A simple character buffer
/// Can later be replaced by a trait to make input generic
///
pub struct Characters {
    ///
    /// The underlying buffer
    ///
    buffer: Vec<char>,
}

impl Characters {
    ///
    /// Creates a new buffer from a string slice by copying the characters in it
    ///
    pub fn from_str(input: &str) -> Characters {
	Characters {
	    buffer: String::from(input).chars().collect(),
	}
    }

    ///
    /// Creates a string containing the token value
    ///
    pub fn token_value(&self, token: &Token) -> String {
	let mut value = String::new();
	for i in token.pos..(token.pos + token.len) {
	    value.push(self.buffer[i]);
	}
	value
    }
}

///
/// A lexer
///
pub struct Lexer<'a, 'b> {
    ///
    /// The factory that created this lexer
    ///
    factory: &'a LexerFactory,
    
    ///
    /// The matcher
    ///
    matcher: Matcher<'a>,

    ///
    /// The character buffer
    ///
    chars: &'b Characters,

    ///
    /// The current position
    ///
    pos: usize,
}

impl<'a, 'b> Lexer<'a, 'b> {
    ///
    /// Attempts to read a token from the character buffer at the specified position
    ///
    pub fn next_token(&mut self) -> Option<Token> {
	if self.matcher.match_vec(&self.chars.buffer, self.pos, 1000) {
	    let pos = self.pos;
	    let len = self.matcher.len();
	    self.pos += len;
	    Some(Token {
		kind: self.matcher.result_id(),
		pos: pos,
		len: len,
	    })
	} else {
	    None
	}
    }

    ///
    /// Returns the token kinds
    ///
    pub fn token_kinds(&self) -> &'a Vec<String> {
	self.factory.automaton.names()
    }

    ///
    /// Retusn the position of the current lexer
    ///
    pub fn pos(&self) -> usize {
	self.pos
    }
}

///
/// Errors that can occur with the lexer
///
#[derive(Debug, PartialEq)]
pub enum Error {
    ///
    /// An IO error occurred
    ///
    IO,
    ///
    /// Could not construct the regex automaton from the definition file
    ///
    InvalidDefinition(RegexError),

    ///
    /// Invalid token count
    ///
    InvalidTokenCount,
    
    ///
    /// Mismatched or invalid tokens
    ///
    InvalidToken(String),
}

impl From<RegexError> for Error {
    fn from(e: RegexError) -> Error {
	Error::InvalidDefinition(e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::settings::Settings;

    #[test]
    pub fn lex_tokens() {
	let settings = Settings::new().unwrap();
	let lexer_factory = LexerFactory::new(&settings).unwrap();

	let input = Characters::from_str("select ");
	let mut lexer = lexer_factory.create_lexer(&input);
	
	let expected = Token {
	    kind: 1,
	    pos: 0,
	    len: 7,
	};
	assert_eq!(Some(expected), lexer.next_token());

	let input = Characters::from_str("*");
	let mut lexer = lexer_factory.create_lexer(&input);
	
	let expected = Token {
	    kind: 0,
	    pos: 0,
	    len: 1,
	};
	assert_eq!(Some(expected), lexer.next_token());
	
	let input = Characters::from_str("from ");
	let mut lexer = lexer_factory.create_lexer(&input);
	
	let expected = Token {
	    kind: 2,
	    pos: 0,
	    len: 5,
	};
	assert_eq!(Some(expected), lexer.next_token());
	
	let input = Characters::from_str(";");
	let mut lexer = lexer_factory.create_lexer(&input);
	
	let expected = Token {
	    kind: 3,
	    pos: 0,
	    len: 1,
	};
	assert_eq!(Some(expected), lexer.next_token());

	let input = Characters::from_str("selectid ");
	let mut lexer = lexer_factory.create_lexer(&input);
	
	let expected = Token {
	    kind: 4,
	    pos: 0,
	    len: 8,
	};
	assert_eq!(Some(expected), lexer.next_token());
    }
}
