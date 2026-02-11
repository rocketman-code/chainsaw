use std::fs;
use std::path::Path;

use crate::lang::RawImport;

pub fn parse_file(path: &Path) -> Result<Vec<RawImport>, String> {
    let _source =
        fs::read_to_string(path).map_err(|e| format!("{}: {e}", path.display()))?;
    Ok(Vec::new())
}
