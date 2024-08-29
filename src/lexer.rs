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

use log::debug;
use std::fs::File;

///
/// Lexer token kind
///
#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    ///
    /// The asterisk wildcard
    ///
    Asterisk,
    
    ///
    /// The from keyword
    ///
    From,
    
    ///
    /// An identifier
    ///
    Identifier,
    
    ///
    /// The select keyword
    ///
    Select,

    ///
    /// The delimiter between two statements
    ///
    StatementDelimiter,

    ///
    /// Whitespace
    ///
    Whitespace
}

impl TokenKind {

    ///
    /// Parses the token from their name
    ///
    fn parse(input: &str) -> Option<TokenKind> {
	match input {
	    "asterisk" => Some(TokenKind::Asterisk),
	    "from" => Some(TokenKind::From),
	    "identifier" => Some(TokenKind::Identifier),
	    "select" => Some(TokenKind::Select),
	    "statement_delimiter" => Some(TokenKind::StatementDelimiter),
	    "whitespace" => Some(TokenKind::Whitespace),
	    _ => None,
	}
    }
    
}

///
/// A token
///
#[derive(Debug, PartialEq)]
pub struct Token {
    ///
    /// The token kind
    ///
    kind: TokenKind,

    ///
    /// The position of the token in the buffer
    ///
    pos: usize,

    ///
    /// The token length
    ///
    len: usize,
}


///
/// The (thread safe) lexer factory
///
pub struct LexerFactory {
    ///
    /// The regex automaton
    ///
    automaton: Automaton,

    ///
    /// Mapping from ID to token kind
    ///
    kinds: Vec<TokenKind>,
}

impl LexerFactory {
    ///
    /// Constructs a new lexer factory from settings
    ///
    pub fn new(settings: &Settings) -> Result<LexerFactory, Error> {
	let mut path = settings.create_config_path();
	path.push("sql.regex");
	let file = File::open(path.clone()).map_err(|_| Error::IO)?;
	LexerFactory::from_str(&std::io::read_to_string(file).map_err(|_| Error::IO)?)
    }
    
    ///
    /// Constructs a new lexer factory from a definition string
    ///
    pub fn from_str(input: &str) -> Result<LexerFactory, Error> {
	let automaton = Automaton::from_str(input)?;
	let count = automaton.names().len();
	debug!("token definitions found: {:?}", automaton.names());
	if count != 6 {
	    Err(Error::InvalidTokenCount)
	} else {
	    let mut kinds = vec![TokenKind::Asterisk; count];
	    for i in 0..count {
		let name = &automaton.names()[i];
		kinds[i] = TokenKind::parse(name).ok_or_else(|| Error::InvalidToken(String::from(name)))?;
	    }
	    Ok(LexerFactory {
		automaton,
		kinds,
	    })
	}
    }

    pub fn create_lexer<'a>(&'a self) -> Lexer<'a> {
	Lexer {
	    factory: self,
	    matcher: Matcher::new(&self.automaton),
	}
    }
}

///
/// A lexer
///
pub struct Lexer<'a> {
    ///
    /// The factory that created this lexer
    ///
    factory: &'a LexerFactory,
    
    ///
    /// The matcher
    ///
    matcher: Matcher<'a>,
}

impl<'a> Lexer<'a> {
    ///
    /// Attempts to read a token from the character buffer at the specified position
    ///
    pub fn next_token(&mut self, input: &Vec<char>, pos: usize) -> Option<Token> {
	if self.matcher.match_vec(input, pos, 1000) {
	    Some(Token {
		kind: self.factory.kinds[self.matcher.result_id()].clone(),
		pos: pos,
		len: self.matcher.len(),
	    })
	} else {
	    None
	}
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
	let mut lexer = lexer_factory.create_lexer();

	let input = String::from("select ").chars().collect();
	let expected = Token {
	    kind: TokenKind::Select,
	    pos: 0,
	    len: 7,
	};
	assert_eq!(Some(expected), lexer.next_token(&input, 0));

	let input = String::from("*").chars().collect();
	let expected = Token {
	    kind: TokenKind::Asterisk,
	    pos: 0,
	    len: 1,
	};
	assert_eq!(Some(expected), lexer.next_token(&input, 0));
	
	let input = String::from("from ").chars().collect();
	let expected = Token {
	    kind: TokenKind::From,
	    pos: 0,
	    len: 5,
	};
	assert_eq!(Some(expected), lexer.next_token(&input, 0));
	
	let input = String::from(";").chars().collect();
	let expected = Token {
	    kind: TokenKind::StatementDelimiter,
	    pos: 0,
	    len: 1,
	};
	assert_eq!(Some(expected), lexer.next_token(&input, 0));

	let input = String::from("selectid ").chars().collect();
	let expected = Token {
	    kind: TokenKind::Identifier,
	    pos: 0,
	    len: 8,
	};
	assert_eq!(Some(expected), lexer.next_token(&input, 0));
    }
}
