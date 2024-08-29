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

///
/// The configuration
///
pub struct Configuration {
    config
}

///
/// Find the configuration directory
///
fn find_config_dir() -> Option<PathBuf> {
    match std::env::current_dir() {
	Ok(mut path) => {
	    loop {
		path.push("config");
		path.push("sql.regex");
		if path.is_file() {
		    path.pop();
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
