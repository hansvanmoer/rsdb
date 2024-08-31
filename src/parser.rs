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

use crate::lexer::{Characters, Error as LexerError, LexerFactory, Lexer, Token};
use crate::settings::Settings;

use std::collections::BTreeMap;
use std::fs::File;
use std::iter::Peekable;

///
/// A factory for grammar parsers
///
pub struct GrammarParserFactory {
    ///
    /// The lexer factory
    ///
    lexer_factory: LexerFactory,

    ///
    /// The grammar token kinds
    ///
    token_kinds: GrammarTokenKinds,
}

impl GrammarParserFactory {

    ///
    /// Constructs a new grammar parser factory
    ///
    fn new(settings: &Settings) -> Result<GrammarParserFactory, Error> {
	let mut config_path = settings.create_config_path();
	config_path.push("grammar.regex");
	let file = File::open(&config_path)?;
	let lexer_factory = LexerFactory::from_file(&file)?;
	let token_kinds = GrammarTokenKinds::new(&lexer_factory)?;
	
	Ok(GrammarParserFactory {
	    lexer_factory,
	    token_kinds,
	})
    }

    ///
    /// Creates a grammar parser for the specified input
    ///
    fn create_parser<'a, 'b>(&'a self, chars: &'b Characters) -> Result<GrammarParser<'a, 'b>, Error>{
	let lexer = self.lexer_factory.create_lexer(chars);
	
	Ok(GrammarParser {
	    chars,
	    tokens: TokenIterator::new(lexer).peekable(),
	    token_kinds: self.token_kinds.clone(),
	    states: Vec::new(),
	    trees: Vec::new(),
	    trees_by_name: BTreeMap::new(),
	})
    }
}

///
/// The tokens for the grammar definition
///
#[derive(Clone, Debug, PartialEq)]
struct GrammarTokenKinds {
    ///
    /// The comment lexeme
    ///
    comment: usize,

    ///
    /// The identifier lexeme
    ///
    identifier: usize,

    ///
    /// The whitespace lexeme
    ///
    whitespace: usize,

    ///
    /// The assignment lexeme
    ///
    assignment: usize,

    ///
    /// The closure lexeme
    ///
    closure: usize,
    
    ///
    /// The or lexeme
    ///
    or: usize,

    ///
    /// The reference start lexeme
    ///
    reference_start: usize,

    ///
    /// The reference end lexeme
    ///
    reference_end: usize,

    ///
    /// The group start lexeme
    ///
    group_start: usize,

    ///
    /// The group end lexeme
    ///
    group_end: usize,

    ///
    /// The statement end lexeme
    /// 
    statement_end: usize,

    ///
    /// The capture lexeme
    ///
    capture: usize,
}

impl GrammarTokenKinds {
    ///
    /// Resolves the tokens necessary for the parser
    ///
    fn new<'a>(factory: &LexerFactory) -> Result<GrammarTokenKinds, Error> {
	let mut whitespace = None;
	let mut comment = None;
	let mut identifier = None;
	let mut assignment = None;
	let mut closure = None;
	let mut or = None;
	let mut reference_start = None;
	let mut reference_end = None;
	let mut group_start = None;
	let mut group_end = None;
	let mut statement_end = None;
	let mut capture = None;
	let names = factory.names();
	for i in 0..names.len() {
	    match names[i].as_str() {
		"comment" => {
		    comment = Some(i);
		    Ok(())
		},
		"identifier" => {
		    identifier = Some(i);
		    Ok(())
		},
		"whitespace" => {
		    whitespace = Some(i);
		    Ok(())
		},
		"assignment" => {
		    assignment = Some(i);
		    Ok(())
		},
		"or" => {
		    or = Some(i);
		    Ok(())
		},
		"closure" => {
		    closure = Some(i);
		    Ok(())
		},
		"reference_start" => {
		    reference_start = Some(i);
		    Ok(())
		},
		"reference_end" => {
		    reference_end = Some(i);
		    Ok(())
		},
		"group_start" => {
		    group_start = Some(i);
		    Ok(())
		},
		"group_end" => {
		    group_end = Some(i);
		    Ok(())
		},
		"statement_end" => {
		    statement_end = Some(i);
		    Ok(())
		},
		"capture" => {
		    capture = Some(i);
		    Ok(())
		},
		_ => {
		    Err(Error::UnknownGrammarToken(names[i].clone()))
		}
	    }?;
	}
	Ok(GrammarTokenKinds {
	    comment: comment.ok_or_else(|| Error::MissingGrammarToken("comment"))?,
	    identifier: identifier.ok_or_else(|| Error::MissingGrammarToken("identifier"))?,
	    whitespace: whitespace.ok_or_else(|| Error::MissingGrammarToken("whitespace"))?,
	    assignment: assignment.ok_or_else(|| Error::MissingGrammarToken("assignment"))?,
	    closure: closure.ok_or_else(|| Error::MissingGrammarToken("closure"))?,
	    or: or.ok_or_else(|| Error::MissingGrammarToken("or"))?,
	    reference_start: reference_start.ok_or_else(|| Error::MissingGrammarToken("reference_start"))?,
	    reference_end: reference_end.ok_or_else(|| Error::MissingGrammarToken("reference_end"))?,
	    group_start: group_start.ok_or_else(|| Error::MissingGrammarToken("group_start"))?,
	    group_end: group_end.ok_or_else(|| Error::MissingGrammarToken("group_end"))?,
	    statement_end: statement_end.ok_or_else(|| Error::MissingGrammarToken("statement_end"))?,
	    capture: capture.ok_or_else(|| Error::MissingGrammarToken("capture"))?,
	})
    }
}

///
/// An iterator
///
struct TokenIterator<'a, 'b> {
    ///
    /// A lexer
    ///
    lexer: Lexer<'a, 'b>,
    
    ///
    /// The current position
    ///
    pos: usize,
}

impl<'a, 'b> TokenIterator<'a, 'b> {
    ///
    /// Constructs a new token iterator
    ///
    fn new(lexer: Lexer<'a, 'b>) -> TokenIterator<'a, 'b> {
	TokenIterator {
	    lexer,
	    pos: 0,
	}
    }

    ///
    /// Returns the underlying lexer
    ///
    fn lexer(&self) -> &Lexer<'a, 'b> {
	&self.lexer
    }
}

impl<'a, 'b> Iterator for TokenIterator<'a, 'b> {
    type Item = Token;

    ///
    /// Gets the next token
    ///
    fn next(&mut self) -> Option<Token> {
	match self.lexer.next_token() {
	    None => None,
	    Some(token) => {
		self.pos += token.len();
		Some(token)
	    },
	}
    }
}

struct GrammarParser<'a, 'b> {
    ///
    /// The character buffers
    ///
    chars: &'b Characters,

    ///
    /// The tokens
    ///
    tokens: Peekable<TokenIterator<'a, 'b>>,

    ///
    /// Token kinds
    ///
    token_kinds: GrammarTokenKinds,
    
    ///
    /// State buffer
    ///
    states: Vec<UnresolvedState>,

    ///
    /// Syntax tree buffer
    ///
    trees: Vec<Tree>,

    ///
    /// Syntax trees by name
    ///
    trees_by_name: BTreeMap<String, usize>,
}

impl<'a, 'b> GrammarParser<'a, 'b> {
    ///
    /// Parses the input into a grammar 
    ///
    fn parse(&mut self) -> Result<Grammar, Error> {
	loop {
	    self.skip_whitespace();
	    match self.tokens.peek() {
		Some(token) => {
		    if token.kind() == self.token_kinds.comment {
			self.tokens.next();
		    } else if token.kind() == self.token_kinds.identifier {
			self.parse_tree();
		    } else {
			break Err(Error::UnexpectedGrammarToken);
		    }
		},
		None => {
		    break Ok(());
		}
	    }
	}?;
	Ok(Grammar {})
    }

    ///
    /// Parses a syntax tree
    ///
    fn parse_tree(&mut self) -> Result<(), Error> {
	let token = self.tokens.next().expect("expected an identifier");
	let name = self.chars.token_value(&token);
	if self.trees_by_name.contains_key(&name) {
	    Err(Error::DuplicateGrammarTree(name))
	} else {
	    Ok(())
	}
    }
    
    ///
    /// Skip whitespace
    ///
    fn skip_whitespace(&mut self) {
	loop {
	    match self.tokens.peek() {
		Some(token) => {
		    if token.kind() != self.token_kinds.whitespace {
			break;
		    }
		},
		None => {
		    break;
		}
	    }
	    self.tokens.next();
	}
    }
}

///
/// A state that ay be an unresolved symbol
///
struct UnresolvedState {}

///
/// A syntax tree
///
struct Tree {}

///
/// A grammar
///
#[derive(Debug, PartialEq)]
pub struct Grammar {
    
}

///
/// All errors related to parsers
///
#[derive(Debug, PartialEq)]
pub enum Error {
    ///
    /// An IO error occurred
    ///
    IO(String),

    ///
    /// A lexer error occurred
    ///
    Lexer(LexerError),

    ///
    /// An unknown token occurred in the grammar definition lexer
    ///
    UnknownGrammarToken(String),

    ///
    /// A token was missing in the grammar definition lexer 
    ///
    MissingGrammarToken(&'static str),

    ///
    /// An unexpected token was encountered parsing the grammar
    ///
    UnexpectedGrammarToken,

    ///
    /// The grammar contains a duplicate tree
    ///
    DuplicateGrammarTree(String),
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Error {
	Error::IO(format!("{:?}", e))
    }
}

impl From<LexerError> for Error {
    fn from(e: LexerError) -> Error {
	Error::Lexer(e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn create_grammar_parser() {
	let settings = Settings::new().unwrap();
	let factory = GrammarParserFactory::new(&settings).unwrap();
    }

    #[test]
    pub fn parse_grammar_comment() {
	let settings = Settings::new().unwrap();
	let factory = GrammarParserFactory::new(&settings).unwrap();
	let input = Characters::from_str(" #this is a comment\n");
	    
	let mut parser = factory.create_parser(&input).unwrap();
	assert_eq!(Ok(Grammar {}), parser.parse());
    }
}
