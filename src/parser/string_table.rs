use std::collections::HashMap;
use std::hash::Hash;
use std::sync::Mutex;

use lazy_static::lazy_static;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct StringID(pub u64);

struct StringTable {
	curr_id: u64,
	strings: HashMap<String, StringID>,
	ids: HashMap<StringID, String>,
}

impl StringTable {
	pub fn new() -> StringTable {
		StringTable {
			curr_id: 0,
			strings: HashMap::new(),
			ids: HashMap::new(),
		}
	}

	pub fn insert(&mut self, s: &str) -> StringID {
		if let Some(id) = self.strings.get(s) {
			return *id;
		}

		self.curr_id = self.curr_id + 1;
		let id = StringID(self.curr_id);

		self.ids.insert(id, String::from(s));
		self.strings.insert(String::from(s), id);
		id
	}

	pub fn lookup(&self, id: StringID) -> Option<&String> {
		self.ids.get(&id)
	}
}

lazy_static! {
	static ref STRING_TABLE: Mutex<StringTable> = Mutex::new(StringTable::new());
}

pub fn insert(s: &str) -> StringID {
	let mut lock = STRING_TABLE.lock().unwrap();
	lock.insert(s)
}

pub fn lookup(id: StringID) -> Option<String> {
	let lock = STRING_TABLE.lock().unwrap();
	lock.lookup(id).map(|x| x.clone())
}

pub fn dump() {
	let lock = STRING_TABLE.lock().unwrap();
	println!("{:?}", lock.ids);
}