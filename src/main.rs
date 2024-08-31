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

mod lexer;
mod parser;
mod regex;
mod settings;

use crate::lexer::{Error as LexerError, LexerFactory};
use crate::settings::Settings;

use log::info;

fn main() {
    env_logger::init();
    info!("starting server...");

    let settings = Settings::new().expect("could not load settings");
    
    info!("loading lexer...");
    let lexers = LexerFactory::new(&settings);
    
    info!("server started.");
}

#[derive(Debug, PartialEq)]
enum Error {
    IO,
    Lexer(LexerError),
}
