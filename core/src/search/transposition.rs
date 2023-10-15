use std::sync::{
	atomic::{AtomicU64, Ordering},
	Arc, RwLock,
};

use crate::position::{moves::Move, Position};

use super::Score;

/// A single row in the transposition table.
#[derive(Copy, Clone)]
pub struct TranspositionEntry {
	/// The table is keyed using the LSBs of this value
	/// to keep table size reasonable, but we want to preserve the whole key
	/// to double check that the hashes are the same. We're still prone to
	/// key collisions, but we avoid index collisions.
	pub key: u64,

	/// What we determined the best move was the last time we hit this position.
	pub best_move: Move,

	/// How deeply we've searched this position. This helps us make more intelligent
	/// eviction decisions, since deeply searched nodes are more valuable than
	/// shallowly searched ones.
	pub depth: u8,

	/// The resulting score after playing the best move. This can help with move
	/// ordering to increase the number of pruned nodes from the search tree.
	pub score: Score,

	/// The half-move age of this row. This is used to help kick out
	/// older positions that are probably not as relevant when we have
	/// an index collision.
	pub age: u16,
}

impl TranspositionEntry {
	pub fn from(position: &Position, best_move: Move, score: Score, depth: u8) -> Self {
		Self {
			key: position.hash(),
			age: position.full_move_clock(),
			best_move,
			score,
			depth,
		}
	}
}

/// A thread-safe lookup table to store the results of previous searches.
#[derive(Clone)]
pub struct TranspositionTable {
	table: Arc<RwLock<Vec<Option<TranspositionEntry>>>>,
	key_mask: Arc<AtomicU64>,
}

impl TranspositionTable {
	/// Create a new [`TranspositionTable`] with a fixed size.  
	/// The size limit is an upper bound, and the actual table might be smaller than
	/// the specified size if (desired_size / size_of(Option<TranspositionEntry>)) is
	/// far from a power of two.  
	/// The desired size is immediately reserved, so the table will not grow once created.
	/// If more space is required, it must be manually resized with [`TranspositionTable::resize()`].
	pub fn new(max_size_megabytes: usize) -> Self {
		// To make accessing items easier, the table size is set to the nearest power of 2 that is
		// less than or equal to the max size determined based on the size of an entry. This lets
		// us use a simple bit mask on each key to bucketize the entry into a vector index.
		let max_entries_for_size =
			max_size_megabytes * 1024 * 1024 / std::mem::size_of::<Option<TranspositionEntry>>();
		let msb_index = 64 - max_entries_for_size.leading_zeros();
		let table_size = 2usize.pow(msb_index);
		let table = Arc::new(RwLock::new(vec![None; table_size]));

		// Easiest way to get all ones up to the msb_index is to create 2 ^ msb_index,
		// then subtract one. Ex: (2 ^ 3) - 1 = 7 = 0b0111
		let key_mask = Arc::new(AtomicU64::from((1 << (msb_index)) - 1));

		Self { table, key_mask }
	}

	/// Resize the table, purging all entries in the process. The memory required for the new table
	/// is immediately reserved, and the table won't grow after being resized.
	pub fn resize(&mut self, max_size_megabytes: usize) {
		let max_entries_for_size =
			max_size_megabytes * 1024 * 1024 / std::mem::size_of::<Option<TranspositionEntry>>();
		let msb_index = 64 - max_entries_for_size.leading_zeros();
		let table_size = 2usize.pow(msb_index);

		// Get an exclusive lock on the table first to ensure that all reads are suspended until
		// the update is complete.
		let mut table = self.table.write().unwrap();
		*table = vec![None; table_size];
		self.key_mask
			.store((1 << (msb_index)) - 1, Ordering::Relaxed);
	}

	/// Retrieve a value from the table. If the value was missing from the table,
	/// returns `None`.
	pub fn get(&self, key: u64) -> Option<TranspositionEntry> {
		let table = self.table.read().unwrap();
		let index = self.key_mask.load(Ordering::Relaxed) & key;
		table[index as usize].filter(|stored_position| stored_position.key == key)
	}

	/// Insert a value into the table.
	pub fn insert(&mut self, entry: TranspositionEntry) {
		let mut table = self.table.write().unwrap();
		let index = (self.key_mask.load(Ordering::Relaxed) & entry.key) as usize;

		if let Some(existing_entry) = table[index] {
			if entry.depth > existing_entry.depth
				|| (entry.age > existing_entry.age && entry.age - existing_entry.age > 10)
			{
				table[index] = Some(entry);
			}
		} else {
			table[index] = Some(entry);
		}
	}

	/// Clear all values from the table. Retains the same underlying memory
	/// for the table to avoir a reallocation.
	pub fn clear(&mut self) {
		self.table.write().unwrap().fill(None);
	}
}
