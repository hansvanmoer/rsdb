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

use std::collections::BTreeMap;
use std::iter::Peekable;
use std::str::Chars;

use log::debug;

///
/// A parser to construct a regex
///
#[derive(Debug)]
pub struct Parser<'a> {
    ///
    /// The input stream
    ///
    input: Peekable<Chars<'a>>,

    ///
    /// The current line
    ///
    line: i32,

    ///
    /// The current column
    ///
    column: i32,

    ///
    /// The current map of regex indices by name
    ///
    names: BTreeMap<String, usize>,

    ///
    /// The regex buffer
    ///
    regexes: Vec<Regex>,
    
    ///
    /// The state buffer
    ///
    states: Vec<UnresolvedState>,
}

#[derive(Debug)]
struct Regex {
    ///
    /// The ID of this regex
    ///
    id: i32,

    ///
    /// The name of the regex
    ///
    name: String,

    ///
    /// Whether this regex is a terminal symbol
    ///
    term: bool,

    ///
    /// The start state
    ///
    start: usize,
    
    ///
    /// The end state
    ///
    end: usize,

    ///
    /// The lower bound state ID
    ///
    lower: usize,

    ///
    /// The upper bound state ID
    ///
    upper: usize,
}

///
/// Whether this is a whitespace character
///
fn is_whitespace(c: &char) -> bool {
    *c == ' ' || *c == '\n' || *c == '\t'
}

///
/// Whether this is a comment
///
fn is_comment(c: &char) -> bool {
    *c != '\n'
}

///
/// Whether this is a name character
///
fn is_name(c: &char) -> bool {
    !is_whitespace(c) && *c != '='
}

///
/// Whether this is a symbol character
///
fn is_symbol(c: &char) -> bool {
    *c != '}'
}

///
/// Whether this is an escape character
///
fn is_escape(c: &char) -> bool {
    c.is_ascii_digit()
}

impl<'a> Parser<'a> {
    ///
    /// Constructs a new parser
    ///
    pub fn new(input: &'a str) -> Parser<'a> {
	Parser {
	    input: input.chars().peekable(),
	    line: 1,
	    column: 1,
	    names: BTreeMap::new(),
	    regexes: Vec::new(),
	    states: Vec::new(),
	}
    }

    ///
    /// Parses the input and returns an automaton
    ///
    pub fn parse(&mut self) -> Result<Automaton, Error> {
	loop {
	    self.skip_whitespace();
	    if !self.has_next() {
		break;
	    }
	    if !self.skip_comment() {
		self.parse_regex()?;
	    }
	}
	Ok(Automaton::new(&self.regexes, &self.states))
    }


    ///
    /// Parses a regex
    ///
    fn parse_regex(&mut self) -> Result<(), Error> {
	println!("parsing regex");
	let mut name = String::new();
	self.take_while(is_name, &mut name);
	if name.len() == 0 {
	    Err(Error::NoName)
	} else {
	    let term = name.get(0..1) != Some("$");
	    println!("regex name: {}", name);
	    self.skip_whitespace();
	    self.parse_character('=')?;
	    self.skip_whitespace();
	    let lower = self.states.len();
	    let (start, end) = self.parse_sequence(';')?;
	    let upper = self.states.len();
	    let id = self.regexes.len();
	    self.set_end(end, id as i32);
	    self.regexes.push(Regex {
		id: id as i32,
		name: name.clone(),
		term,
		start,
		end,
		lower,
		upper
	    });
	    self.names.insert(name, id);
	    Ok(())
	}
    }

    ///
    /// Parses an expression
    ///
    fn parse_sequence(&mut self, delim: char) -> Result<(usize, usize), Error> {
	let mut state = None;
	loop {
	    let next = match self.input.peek() {
		Some(&';') => {
		    if delim == ';' {
			match state {
			    None => {
				Err(Error::EmptyExpression)
			    },
			    Some((start, end)) => {
				self.skip();
				self.set_then_otherwise(end, end);
				break Ok((start, end));
			    },
			}
		    } else {
			Err(Error::UnexpectedChar)
		    }
		},
		Some(&')') => {
		    if delim == ')' {
			match state {
			    None => {
				Err(Error::EmptyGroup)
			    },
			    Some((start, end)) => {
				self.skip();
				self.set_then_otherwise(end, end);
				break Ok((start, end));
			    },
			}
		    } else {
			Err(Error::UnexpectedChar)
		    }
		},
		Some(_) => {
		    let (start, end) = self.parse_node()?;
		    match state {
			None => {
			    Ok((start, end))
			},
			Some((prev_start, prev_end)) => {
			    self.set_then_otherwise(prev_end, start);
			    Ok((prev_start, end))
			}
		    }
		}
		None => {
		     Err(Error::UnexpectedEnd)
		},
	    }?;
	    state = Some(next);
	    self.skip_whitespace();
	}
    }

    ///
    /// Parses a node
    ///
    fn parse_node(&mut self) -> Result<(usize, usize), Error>{
	let (start, end) = match self.input.peek() {
	    Some(&';') => {
		Err(Error::EmptyExpression)
	    },
	    Some(&'*') => {
		Err(Error::EmptyClosure)
	    },
	    Some(&'|') => {
		Err(Error::EmptyBranch)
	    },
	    Some(&'(') => {
		self.parse_group()
	    },
	    Some(&')') => {
		Err(Error::UnexpectedChar)
	    },
	    Some(&'{') => {
		self.parse_symbol()
	    },
	    Some(&'[') => {
		self.parse_range()
	    },
	    Some(_) => {
		self.parse_literal()
	    },
	    None => {
		Err(Error::UnexpectedEnd)
	    },
	}?;
	match self.input.peek() {
	    Some(&'*') => {
		Ok(self.parse_closure(start, end))
	    },
	    Some(&'|') => {
		self.parse_branch(start, end)
	    },
	    _ => Ok((start, end))
	}
    }

    ///
    /// Unescapes a character literal
    ///
    fn unescape(&mut self, c: char) -> Result<u32, Error> {
	if c == '&' {
	    let mut buffer = String::new();
	    self.take_while(is_escape, &mut buffer);
	    if buffer.len() == 0 {
		Ok(c as u32)
	    } else {
		buffer.parse::<u32>().map_err(|_| Error::InvalidEscape)
	    }
	} else {
	    Ok(c as u32)
	}
    }
    
    ///
    /// Parse a range
    ///
    fn parse_range(&mut self) -> Result<(usize, usize), Error> {
	self.skip();
	self.skip_whitespace();
	let left = match self.input.peek() {
	    None => Err(Error::UnexpectedEnd),
	    Some('-') => Err(Error::UnexpectedChar),
	    Some(c) => Ok(*c)
	}?;
	self.skip();
	let left = self.unescape(left)?;
	
	self.skip_whitespace();
	match self.input.peek() {
	    None => Err(Error::UnexpectedEnd),
	    Some('-') => Ok(()),
	    Some(_) => Err(Error::UnexpectedChar),
	}?;
	self.skip();
	self.skip_whitespace();
	let right = match self.input.peek() {
	    None => Err(Error::UnexpectedEnd),
	    Some(']') => Err(Error::UnexpectedChar),
	    Some(c) => {
		Ok(*c)
	    },
	}?;
	self.skip();
	let right = self.unescape(right)?;
	
	self.skip_whitespace();
	match self.input.peek() {
	    None => Err(Error::UnexpectedEnd),
	    Some(']') => Ok(()),
	    Some(_) => Err(Error::UnexpectedChar),
	}?;
	self.skip();
	let id = self.add_state();
	if left > right {
	    self.set_range(id, right, left + 1);
	} else {
	    self.set_range(id, left, right + 1);
	}
	Ok((id, id))
    }

    ///
    /// Parses a literal
    ///
    fn parse_literal(&mut self) -> Result<(usize, usize), Error> {
	let c = match self.input.peek() {
	    Some(c) => Ok(*c),
	    None => Err(Error::UnexpectedEnd),
	}?;
	self.skip();
	let code_point = self.unescape(c)?;
	
	let id = self.add_state();
	self.set_range(id, code_point, code_point + 1);
	Ok((id, id))
    }

    ///
    /// Parses a symbol and checks whether it exists
    ///
    fn parse_symbol(&mut self) -> Result<(usize, usize), Error> {
	let mut name = String::new();
	name.push('$');
	self.skip();
	self.take_while(is_symbol, &mut name);
	match self.input.peek() {
	    Some(&'}') => Ok(()),
	    Some(_) => Err(Error::UnexpectedChar),
	    None => Err(Error::UnexpectedEnd),
	}?;
	self.skip();
	if name.len() == 1 {
	    Err(Error::NoSymbolName)
	} else {
	    let symbol_id = match self.names.get(&name) {
		Some(regex_id) => Ok(*regex_id),
		None => Err(Error::NoSymbol),
	    }?;
	    let id = self.add_symbol(symbol_id as i32);
	    Ok((id, id))
	}
    }

    ///
    /// Parses a kleene closure
    ///
    fn parse_closure(&mut self, start: usize, end: usize) -> (usize, usize) {
	self.skip();
	let id = self.add_state();
	self.set_then(end, start);
	self.set_otherwise(end, id);
	(start, id)
    }

    ///
    /// Parses a branch
    ///
    fn parse_branch(&mut self, left_start: usize, left_end: usize) -> Result<(usize, usize), Error> {
	self.skip();
	let (right_start, right_end) = self.parse_node()?;
	let branch_start = self.add_state();
	self.set_then(branch_start, left_start);
	self.set_otherwise(branch_start, right_start);
	let branch_end = self.add_state();
	self.set_then_otherwise(left_end, branch_end);
	self.set_then_otherwise(right_end, branch_end);
	Ok((branch_start, branch_end))
    }

    ///
    /// Parses a group
    ///
    fn parse_group(&mut self) -> Result<(usize, usize), Error> {
	self.skip();
	self.parse_sequence(')')
    }

    ///
    /// Parses a single character
    ///
    fn parse_character(&mut self, c: char) -> Result<(), Error> {
	match self.input.peek() {
	    Some(c) => {
		self.skip();
		Ok(())
	    },
	    Some(_) => {
		Err(Error::UnexpectedChar)
	    },
	    None => {
		Err(Error::UnexpectedEnd)
	    }
	}
    }

    ///
    /// Skip a comment
    ///
    fn skip_comment(&mut self) -> bool {
	match self.input.peek() {
	    Some('#') => {
		self.skip();
		self.skip_while(is_comment);
		true
	    },
	    _ => {
		false
	    }
	}
    }
    
    ///
    /// Skip whitespace
    ///
    fn skip_whitespace(&mut self) {
	self.skip_while(is_whitespace);
    }
    
    ///
    /// Skips while the predicate holds
    ///
    fn skip_while<P: Fn(&char) -> bool>(&mut self, pred: P) {
	loop {
	    match self.input.peek() {
		Some(c) => {
		    if !pred(c) {
			break;
		    }
		},
		None => {
		    break;
		}
	    }
	    self.skip();
	}
    }

    ///
    /// Takes characters from the input while the predicate holds
    ///
    fn take_while<P: Fn(&char) -> bool>(&mut self, pred: P, buffer: &mut String) {
	loop {
	    match self.input.peek() {
		Some(c) => {
		    if pred(c) {
			buffer.push(*c);
		    } else {
			break;
		    }
		},
		None => {
		    break;
		}
	    }
	    self.skip();
	}
    }

    ///
    /// Skips one character
    ///
    fn skip(&mut self) {
	match self.input.next() {
	    Some('\n') => {
		self.line += 1;
		self.column = 1;
	    },
	    Some(_) => {
		self.column += 1;
	    },
	    None => {}
	}
    }

    ///
    /// Whether there are more characters in the input
    ///
    fn has_next(&mut self) -> bool {
	self.input.peek().is_some()
    }

    ///
    /// Sets the state to move to
    ///
    fn set_then(&mut self, id: usize, next: usize) {
	self.states[id].then = next;
    }


    ///
    /// Sets the otherwise to the state itself
    ///
    fn set_otherwise(&mut self, id: usize, otherwise: usize) {
	self.states[id].otherwise = otherwise;
    }

    ///
    /// Sets both then and otherwise
    ///
    fn set_then_otherwise(&mut self, id: usize, next: usize) {
	self.states[id].then = next;
	self.states[id].otherwise = next;
    }
    
    ///
    /// Sets the otherwise to the state itself
    ///
    fn set_no_otherwise(&mut self, id: usize) {
	self.states[id].otherwise = id;
    }

    ///
    /// Sets the end state
    ///
    fn set_end(&mut self, id: usize, result: i32) {
	self.states[id].then = id;
	self.states[id].otherwise = id;
	self.states[id].result = result;
    }

    ///
    /// Sets a range
    ///
    fn set_range(&mut self, id: usize, min: u32, max: u32) {
	self.states[id].min = min;
	self.states[id].max = max;
    }

    ///
    /// Adds a symbol state
    ///
    fn add_symbol(&mut self, symbol: i32) -> usize {
	let id = self.add_state();
	self.states[id].symbol = symbol;
	id
    }

    ///
    /// Adds a state
    ///
    fn add_state(&mut self) -> usize {
	let id = self.states.len();
	self.states.push(UnresolvedState::new(id));
	id
    }

    ///
    /// Creates a state and sets otherwise
    ///
    fn add_then(&mut self, id: usize) -> usize {
	let then = self.add_state();
	self.states[id].then = then;
	then
    }
    
    ///
    /// Creates a state and sets otherwise
    ///
    fn add_otherwise(&mut self, id: usize) -> usize {
	let otherwise = self.add_state();
	self.states[id].otherwise = id;
	otherwise
    }

    ///
    /// Returns the position of the parser
    ///
    fn get_pos(&self) -> (i32, i32) {
	(self.line, self.column)
    }
}

///
/// A state in the automaton, possibly with unresolved symbols
///
#[derive(Clone, Debug, PartialEq)]
struct UnresolvedState {
    ///
    /// The minimum of the input range
    ///
    min: u32,

    ///
    /// The maximum of the input range
    ///
    max: u32,

    ///
    /// If within range, go here
    ///
    then: usize,

    ///
    /// If not, go here
    ///
    otherwise: usize,

    ///
    /// Whether this is a symbol
    ///
    symbol: i32,
    
    ///
    /// If this is not -1, this is an end state
    ///
    result: i32,
}

impl UnresolvedState {
    ///
    /// Constructs a new state
    ///
    fn new(id: usize) -> UnresolvedState {
	UnresolvedState {
	    min: 0,
	    max: 0,
	    then: id,
	    otherwise: id,
	    symbol: -1,
	    result: -1,
	}
    }
}


///
/// A state in the automaton
///
#[derive(Clone, Debug, PartialEq)]
struct State {
    ///
    /// The minimum of the input range
    ///
    min: u32,

    ///
    /// The maximum of the input range
    ///
    max: u32,

    ///
    /// If within range, go here
    ///
    then: usize,

    ///
    /// If not, go here
    ///
    otherwise: usize,
    
    ///
    /// If this is not -1, this is an end state
    ///
    result: i32,
}

///
/// A non-deterministic automaton for a set of regex expression
///
#[derive(Debug, PartialEq)]
pub struct Automaton {
    ///
    /// The names of the regular expressions
    ///
    names: Vec<String>,

    ///
    /// The state buffer
    ///
    states: Vec<State>,

    ///
    /// The start state
    ///
    start: Option<usize>,
}

impl Automaton {
    ///
    /// Creates a new automaton
    ///
    fn new(regexes: &Vec<Regex>, states: &Vec<UnresolvedState>) -> Automaton {
	let mut autom = Automaton {
	    names: Vec::with_capacity(regexes.len()),
	    states: Vec::new(),
	    start: None,
	};
	autom.add_all(regexes, states);
	autom
    }

    ///
    /// Adds all regexes
    ///
    fn add_all(&mut self, regexes: &Vec<Regex>, states: &Vec<UnresolvedState>) {
	let mut start = None;
	for regex_id in 0..regexes.len() {
	    let regex = &regexes[regex_id];
	    if regex.term {
		let (next, _) = self.add(regex_id, regexes, states);
		start = match start {
		    None => {
			Some(next)
		    },
		    Some(prev) => {
			let id = self.states.len();
			self.states.push(State {
			    min: 0,
			    max: 0,
			    then: prev,
			    otherwise: next,
			    result: -1
			});
			Some(id)
		    }
		};
	    }
	}
	self.start = start;
    }

    ///
    /// Adds a single regex
    ///
    fn add(&mut self, regex_id: usize, regexes: &Vec<Regex>, input: &Vec<UnresolvedState>) -> (usize, usize) {
	let regex = &regexes[regex_id];
	let result = if regex.term {
	    let result = self.names.len() as i32;
	    self.names.push(regex.name.clone());
	    result
	} else {
	    -1
	};
	let offset = self.states.len();
	println!("offset: {}, lower: {}, upper: {}", offset, regex.lower, regex.upper);
	for i in regex.lower..regex.upper {
	    let state = &input[i];
	    self.states.push(State {
		min: state.min,
		max: state.max,
		then: state.then + offset - regex.lower,
		otherwise: state.otherwise + offset - regex.lower,
		result: if state.result == -1 {
		    -1
		} else {
		    result
		},
	    });
	}
	let result = (regex.start + offset - regex.lower, regex.end + offset - regex.lower);
	for i in regex.lower..regex.upper {
	    let state = &input[i];
	    if state.symbol != -1 {
		let (start, end) = self.add(state.symbol as usize, regexes, input);
		let id = i + offset - regex.lower;
		let then = self.states[id].then;
		let otherwise = self.states[id].otherwise;
		self.states[id].then = start;
		self.states[id].otherwise = start;
		self.states[end].then = then;
		self.states[end].otherwise = otherwise;
	    }
	}
	result
    }
}

struct Matcher<'a> {
    autom: &'a Automaton,
    stack: Vec<usize>,
}

///
/// Errors that can occur while parsing
///
#[derive(Debug, PartialEq)]
pub enum Error {
    ///
    /// No name found for a regex
    ///
    NoName,

    ///
    /// Symbol does not exist (yet)
    ///
    NoSymbol,
    
    ///
    /// No symbol name found
    ///
    NoSymbolName,

    ///
    /// Invalid escape character
    ///
    InvalidEscape,
    
    ///
    /// Unexpected end of input
    ///
    UnexpectedEnd,

    ///
    /// Unexpected character
    ///
    UnexpectedChar,

    ///
    /// Kleene closure is empty
    ///
    EmptyClosure,
    
    ///
    /// An expression can't be empty
    ///
    EmptyExpression,

    ///
    /// A branch can't be empty
    ///
    EmptyBranch,

    ///
    /// A group can't be empty
    ///
    EmptyGroup,
}

#[cfg(test)]
mod tests {

    use super::*;
    
    #[test]
    pub fn no_name() {
	let mut parser = Parser::new(" =a");
	let result = parser.parse();

	assert_eq!((1, 2), parser.get_pos());
	assert_eq!(Err(Error::NoName), result);
    }

    #[test]
    pub fn no_expression_delim() {
	let mut parser = Parser::new("literal");
	let result = parser.parse();

	assert_eq!((1, 8), parser.get_pos());
	assert_eq!(Err(Error::UnexpectedEnd), result);
    }
    
    #[test]
    pub fn no_expression() {
	let mut parser = Parser::new("literal =");
	let result = parser.parse();

	assert_eq!((1, 10), parser.get_pos());
	assert_eq!(Err(Error::UnexpectedEnd), result);
    }
    
    #[test]
    pub fn literal() {
	let mut parser = Parser::new("literal = a ;");
	let result = parser.parse();

	assert_eq!((1, 14), parser.get_pos());
	
	assert_eq!(Ok(Automaton {
	    names: vec![String::from("literal")],
	    states: vec![
		State {
		    min: 97,
		    max: 98,
		    then: 0,
		    otherwise: 0,
		    result: 0,
		},
	    ],
	    start: Some(0),
	}), result);
    }

    #[test]
    pub fn empty_closure() {
	let mut parser = Parser::new("literal = *;");
	let result = parser.parse();

	assert_eq!((1, 11), parser.get_pos());
	assert_eq!(Err(Error::EmptyClosure), result);
    }

    #[test]
    pub fn closure() {
	let mut parser = Parser::new("literal=a*;");
	let result = parser.parse();

	assert_eq!((1, 12), parser.get_pos());
	
	assert_eq!(Ok(Automaton {
	    names: vec![String::from("literal")],
	    states: vec![
		State {
		    min: 97,
		    max: 98,
		    then: 0,
		    otherwise: 1,
		    result: -1,
		},
		State {
		    min: 0,
		    max: 0,
		    then: 1,
		    otherwise: 1,
		    result: 0
		},
	    ],
	    start: Some(0),
	}), result);
    }

    #[test]
    pub fn branch() {
	let mut parser = Parser::new("literal=a|b;");
	let result = parser.parse();

	assert_eq!((1, 13), parser.get_pos());
	
	assert_eq!(Ok(Automaton {
	    names: vec![String::from("literal")],
	    states: vec![
		State {
		    min: 97,
		    max: 98,
		    then: 3,
		    otherwise: 3,
		    result: -1,
		},
		State {
		    min: 98,
		    max: 99,
		    then: 3,
		    otherwise: 3,
		    result: -1,
		},
		State {
		    min: 0,
		    max: 0,
		    then: 0,
		    otherwise: 1,
		    result: -1,
		},
		State {
		    min: 0,
		    max: 0,
		    then: 3,
		    otherwise: 3,
		    result: 0,
		},
	    ],
	    start: Some(2),
	}), result);
    }
    
    #[test]
    pub fn empty_group() {
	let mut parser = Parser::new("literal = ();");
	let result = parser.parse();

	assert_eq!((1, 12), parser.get_pos());
	assert_eq!(Err(Error::EmptyGroup), result);
    }

    #[test]
    pub fn unclosed_group() {
	let mut parser = Parser::new("literal = (;");
	let result = parser.parse();

	assert_eq!((1, 12), parser.get_pos());
	assert_eq!(Err(Error::UnexpectedChar), result);
    }

    #[test]
    pub fn unexpected_group_end() {
	let mut parser = Parser::new("literal = );");
	let result = parser.parse();

	assert_eq!((1, 11), parser.get_pos());
	assert_eq!(Err(Error::UnexpectedChar), result);
    }
    
    #[test]
    pub fn group() {
	let mut parser = Parser::new("literal=(a);");
	let result = parser.parse();

	assert_eq!((1, 13), parser.get_pos());
	
	assert_eq!(Ok(Automaton {
	    names: vec![String::from("literal")],
	    states: vec![
		State {
		    min: 97,
		    max: 98,
		    then: 0,
		    otherwise: 0,
		    result: 0,
		},
	    ],
	    start: Some(0),
	}), result);
    }
    
    #[test]
    pub fn sequence() {
	let mut parser = Parser::new("literal=ab;");
	let result = parser.parse();

	assert_eq!((1, 12), parser.get_pos());
	
	assert_eq!(Ok(Automaton {
	    names: vec![String::from("literal")],
	    states: vec![
		State {
		    min: 97,
		    max: 98,
		    then: 1,
		    otherwise: 1,
		    result: -1,
		},
		State {
		    min: 98,
		    max: 99,
		    then: 1,
		    otherwise: 1,
		    result: 0,
		},
	    ],
	    start: Some(0),
	}), result);
    }

    #[test]
    pub fn combined() {
	let mut parser = Parser::new("literal=((ab)|c)*;");
	let result = parser.parse();

	assert_eq!((1, 19), parser.get_pos());
	
	assert_eq!(Ok(Automaton {
	    names: vec![String::from("literal")],
	    states: vec![
		State {
		    min: 97,
		    max: 98,
		    then: 1,
		    otherwise: 1,
		    result: -1,
		},
		State {
		    min: 98,
		    max: 99,
		    then: 4,
		    otherwise: 4,
		    result: -1,
		},
		State {
		    min: 99,
		    max: 100,
		    then: 4,
		    otherwise: 4,
		    result: -1,
		},
		State {
		    min: 0,
		    max: 0,
		    then: 0,
		    otherwise: 2,
		    result: -1,
		},
		State {
		    min: 0,
		    max: 0,
		    then: 3,
		    otherwise: 5,
		    result: -1
		},
		State {
		    min: 0,
		    max: 0,
		    then: 5,
		    otherwise: 5,
		    result: 0
		}
	    ],
	    start: Some(3),
	}), result);
    }
    
    #[test]
    pub fn no_symbol_name() {
	let mut parser = Parser::new("literal = {};");
	let result = parser.parse();

	assert_eq!((1, 13), parser.get_pos());
	assert_eq!(Err(Error::NoSymbolName), result);
    }

    #[test]
    pub fn open_symbol_name() {
	let mut parser = Parser::new("literal = {abcdek");
	let result = parser.parse();

	assert_eq!((1, 18), parser.get_pos());
	assert_eq!(Err(Error::UnexpectedEnd), result);
    }

    #[test]
    pub fn no_symbol() {
	let mut parser = Parser::new("literal = {abc};");
	let result = parser.parse();

	assert_eq!((1, 16), parser.get_pos());
	assert_eq!(Err(Error::NoSymbol), result);
    }

    #[test]
    pub fn symbol() {
	let mut parser = Parser::new("$symbol=a;literal={symbol}b;");
	let result = parser.parse();

	assert_eq!((1, 29), parser.get_pos());
	
	assert_eq!(Ok(Automaton {
	    names: vec![String::from("literal")],
	    states: vec![
		State {
		    min: 0,
		    max: 0,
		    then: 2,
		    otherwise: 2,
		    result: -1,
		},
		State {
		    min: 98,
		    max: 99,
		    then: 1,
		    otherwise: 1,
		    result: 0,
		},
		State {
		    min: 97,
		    max: 98,
		    then: 1,
		    otherwise: 1,
		    result: -1,
		}
	    ],
	    start: Some(0),
	}), result);
    }

    #[test]
    pub fn open_range_left() {
	let mut parser = Parser::new("literal = [");
	let result = parser.parse();

	assert_eq!((1, 12), parser.get_pos());
	assert_eq!(Err(Error::UnexpectedEnd), result);
    }

    #[test]
    pub fn no_range_left() {
	let mut parser = Parser::new("literal = [-");
	let result = parser.parse();

	assert_eq!((1, 12), parser.get_pos());
	assert_eq!(Err(Error::UnexpectedChar), result);
    }

    #[test]
    pub fn open_range_separator() {
	let mut parser = Parser::new("literal = [a-");
	let result = parser.parse();

	assert_eq!((1, 14), parser.get_pos());
	assert_eq!(Err(Error::UnexpectedEnd), result);
    }

    #[test]
    pub fn no_range_separator() {
	let mut parser = Parser::new("literal = [ab");
	let result = parser.parse();

	assert_eq!((1, 13), parser.get_pos());
	assert_eq!(Err(Error::UnexpectedChar), result);
    }
    
    #[test]
    pub fn open_range_right() {
	let mut parser = Parser::new("literal = [a-b");
	let result = parser.parse();

	assert_eq!((1, 15), parser.get_pos());
	assert_eq!(Err(Error::UnexpectedEnd), result);
    }

    #[test]
    pub fn no_range_right() {
	let mut parser = Parser::new("literal = [a-bb");
	let result = parser.parse();

	assert_eq!((1, 15), parser.get_pos());
	assert_eq!(Err(Error::UnexpectedChar), result);
    }

    #[test]
    pub fn range() {
	let mut parser = Parser::new("literal = [ a - b ];");
	let result = parser.parse();

	assert_eq!((1, 21), parser.get_pos());
	
	assert_eq!(Ok(Automaton {
	    names: vec![String::from("literal")],
	    states: vec![
		State {
		    min: 97,
		    max: 99,
		    then: 0,
		    otherwise: 0,
		    result: 0,
		},
	    ],
	    start: Some(0),
	}), result);
    }
    
    #[test]
    pub fn reverse_range() {
	let mut parser = Parser::new("literal = [b-a];");
	let result = parser.parse();

	assert_eq!((1, 17), parser.get_pos());
	
	assert_eq!(Ok(Automaton {
	    names: vec![String::from("literal")],
	    states: vec![
		State {
		    min: 97,
		    max: 99,
		    then: 0,
		    otherwise: 0,
		    result: 0,
		},
	    ],
	    start: Some(0),
	}), result);
    }
    
    #[test]
    pub fn escaped_range() {
	let mut parser = Parser::new("literal = [&97-&98];");
	let result = parser.parse();

	assert_eq!((1, 21), parser.get_pos());
	
	assert_eq!(Ok(Automaton {
	    names: vec![String::from("literal")],
	    states: vec![
		State {
		    min: 97,
		    max: 99,
		    then: 0,
		    otherwise: 0,
		    result: 0,
		},
	    ],
	    start: Some(0),
	}), result);
    }

    #[test]
    pub fn invalid_escaped_range() {
	let mut parser = Parser::new("literal = [&97-&1000000000000000];");
	let result = parser.parse();

	assert_eq!((1, 33), parser.get_pos());
	
	assert_eq!(Err(Error::InvalidEscape), result);
    }

    #[test]
    pub fn escaped_literal() {
	let mut parser = Parser::new("literal = &97;");
	let result = parser.parse();

	assert_eq!((1, 15), parser.get_pos());
	
	assert_eq!(Ok(Automaton {
	    names: vec![String::from("literal")],
	    states: vec![
		State {
		    min: 97,
		    max: 98,
		    then: 0,
		    otherwise: 0,
		    result: 0,
		},
	    ],
	    start: Some(0),
	}), result);
    }

    #[test]
    pub fn invalid_literal() {
	let mut parser = Parser::new("literal = &1000000000000000;");
	let result = parser.parse();

	assert_eq!((1, 28), parser.get_pos());
	
	assert_eq!(Err(Error::InvalidEscape), result);
    }
}
