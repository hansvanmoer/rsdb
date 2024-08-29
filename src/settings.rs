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

use std::path::PathBuf;

use log::debug;

///
/// The settings
///
#[derive(Debug)]
pub struct Settings {
    ///
    /// The configuration folder
    ///
    config_dir: PathBuf,
}

impl Settings {
    ///
    /// Constructs a new settings instance
    ///
    pub fn new() -> Result<Settings, Error> {
	Ok(Settings {
	    config_dir: Settings::find_config_dir().ok_or_else(|| Error::NoConfiguration)?,
	})
    }

    ///
    /// Constructs a new buffer containing the config path
    ///
    pub fn create_config_path(&self) -> PathBuf {
	self.config_dir.clone()
    }

    
    ///
    /// Find the configuration directory
    ///
    fn find_config_dir() -> Option<PathBuf> {
	match std::env::current_dir() {
	    Ok(mut path) => {
		loop {
		    debug!("testing path for configuration: {:?}", path);
		    path.push("config");
		    path.push("sql.regex");
		    if path.is_file() {
			path.pop();
			debug!("found configuration path: {:?}", path);
			return Some(path);
		    }
		    path.pop();
		    path.pop();
		    if !path.pop() {
			break;
		    }
		}
		None
	    },
	    _ => None,
	}
    }
}

///
/// Errors that can occur loading the settings
///
#[derive(Debug, PartialEq)]
pub enum Error {
    ///
    /// No configuration folder found
    ///
    NoConfiguration,
}
