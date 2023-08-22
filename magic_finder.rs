/* Adapted from the C++ example given on the chess programming wiki by
 * Tord Romstad: https://www.chessprogramming.org/Looking_for_Magics 
 */

use std::io::prelude::*;

const ROOK_BITS: [u64; 64] = [
	12, 11, 11, 11, 11, 11, 11, 12,
	11, 10, 10, 10, 10, 10, 10, 11,
	11, 10, 10, 10, 10, 10, 10, 11,
	11, 10, 10, 10, 10, 10, 10, 11,
	11, 10, 10, 10, 10, 10, 10, 11,
	11, 10, 10, 10, 10, 10, 10, 11,
	11, 10, 10, 10, 10, 10, 10, 11,
	12, 11, 11, 11, 11, 11, 11, 12
];

const BISHOP_BITS: [u64; 64] = [
	6, 5, 5, 5, 5, 5, 5, 6,
	5, 5, 5, 5, 5, 5, 5, 5,
	5, 5, 7, 7, 7, 7, 5, 5,
	5, 5, 7, 9, 9, 7, 5, 5,
	5, 5, 7, 9, 9, 7, 5, 5,
	5, 5, 7, 7, 7, 7, 5, 5,
	5, 5, 5, 5, 5, 5, 5, 5,
	6, 5, 5, 5, 5, 5, 5, 6
];

/// A really simple random u64 generator using /dev/urandom to avoid
/// needing to depend on the `rand` crate.
fn random() -> u64 {
	let mut f = std::fs::File::open("/dev/urandom").unwrap();
	let mut buff: [u8; 8] = [0; 8];

	f.read_exact(&mut buff).unwrap();
	u64::from_be_bytes(buff)
}

enum BoardType {
	Bishop,
	Rook
}

impl BoardType {

	pub fn bits_for_square(&self, square: u64) -> u64 {
		match self {
			Self::Bishop => BISHOP_BITS[square as usize],
			Self::Rook => ROOK_BITS[square as usize],
		}
	}

	pub fn mask_for_square(&self, square: u64) -> u64 {
		match self {
			Self::Bishop => self.bishop_mask(square),
			Self::Rook => self.rook_mask(square),
		}
	}

	pub fn blockers(&self, square: u64, blockers: u64) -> u64 {
		match self {
			Self::Bishop => self.bishop_blockers(square, blockers),
			Self::Rook => self.rook_blockers(square, blockers),
		}
	}

	fn bishop_mask(&self, square: u64) -> u64 {
		let mut mask = 0u64;
		let rank = square / 8;
		let file = square % 8;

		for (r, f) in (rank+1..7).zip(file+1..7) {
			mask |= 1 << f + r * 8;
		}

		for (r, f) in (rank+1..7).zip((0..file).rev()) {
			mask |= 1 << f + r * 8;
		}

		for (r, f) in (0..rank).rev().zip(file+1..7) {
			mask |= 1 << f + r * 8;
		}

		for (r, f) in (0..rank).rev().zip((0..file).rev()) {
			mask |= 1 << f + r * 8;
		}

		mask
	}

	fn rook_mask(&self, square: u64) -> u64 {
		let mut mask = 0u64;
		let rank = square / 8;
		let file = square % 8;

		for r in 1..7 {
			if r == rank { continue; }
			mask |= 1 << file + r * 8;
		}

		for f in (file+1)..7 {
			if f == file { continue; }
			mask |= 1 << f + rank * 8;
		}

		mask
	}

	fn bishop_blockers(&self, square: u64, blockers: u64) -> u64 {
		let mut mask = 0u64;
		let rank = square / 8;
		let file = square % 8;

		for (r, f) in (rank+1..7).zip(file+1..7) {
			let attacked_square = 1 << f + r * 8;
			mask |= attacked_square;
			if attacked_square & blockers != 0 {
				break;
			}
		}

		for (r, f) in (rank+1..7).zip((0..file).rev()) {
			let attacked_square = 1 << f + r * 8;
			mask |= attacked_square;
			if attacked_square & blockers != 0 {
				break;
			}
		}

		for (r, f) in (0..rank).rev().zip(file+1..7) {
			let attacked_square = 1 << f + r * 8;
			mask |= attacked_square;
			if attacked_square & blockers != 0 {
				break;
			}
		}

		for (r, f) in (0..rank).rev().zip((0..file).rev()) {
			let attacked_square = 1 << f + r * 8;
			mask |= attacked_square;
			if attacked_square & blockers != 0 {
				break;
			}
		}

		mask
	}

	fn rook_blockers(&self, square: u64, blockers: u64) -> u64 {
		let mut mask = 0;
		let rank = square / 8;
		let file = square % 8;

		for r in rank+1..8 {
			let attacked_square = 1 << (file + (8 * r));
			mask |= attacked_square;
			if blockers & attacked_square != 0 { 
				break;
			}
		}

		for r in (0..rank).rev() {
			let attacked_square = 1 << (file + (8 * r));
			mask |= attacked_square;
			if blockers & attacked_square != 0 { 
				break;
			}
		}

		for f in file+1..8 {
			let attacked_square = 1 << (f + (8 * rank));
			mask |= attacked_square;
			if blockers & attacked_square != 0 { 
				break;
			}
		}

		for f in (0..file).rev() {
			let attacked_square = 1 << (f + (8 * rank));
			mask |= attacked_square;
			if blockers & attacked_square != 0 { 
				break;
			}
		}

		mask
	}
}

fn index_to_u64(index: u64, num_ones: u64, mut mask: u64) -> u64 {
	let mut result = 0u64;

	for i in 0..num_ones {
		let lsb_index = mask.trailing_zeros();
		mask &= mask - 1; // Drop least significant bit set to 1
		if index & (1 << i) != 0 {
			result |= 1 << lsb_index;
		}
	}

	result
}

fn random_magic() -> u64 {
	random() & random() & random()
}

fn find_magic(board_type: BoardType, square: u64) -> Option<u64> {
	let mask = board_type.mask_for_square(square);
	let desired_magic_bits = board_type.bits_for_square(square);
	let num_ones = mask.count_ones() as u64;

	let mut boards = [0; 4096];
	let mut blockers = [0; 4096];

	for i in 0..(1 << num_ones) {
		boards[i] = index_to_u64(i as u64, num_ones, mask);
		blockers[i] = board_type.blockers(square, boards[i]);
	}

	for _attempt in 0..100_000_000 {
		let magic = random_magic();
		if ((mask * magic) & 0xFF00000000000000).count_ones() < 6 {
			// Probably a bad magic, just discard it and move on to another
			continue;
		}

		let mut used = [0; 4096];
		let mut fail = false;
		for i in 0..(1 << num_ones) {
			let position_index = (boards[i] * magic) >> (64 - desired_magic_bits);
			if used[position_index as usize] == 0 {
				// First time that the product of the position and the magic
				// had this value, so we're okay so far. Store the map of the
				// actual blocking pieces for this position.
				used[position_index as usize] = blockers[i];
			} else if used[position_index as usize] != blockers[i] {
				// We can only share the same magic as another bit configuration
				// the two are blocked by the same pieces, which means that
				// the valid moves are the same even if there's other pieces
				// behind those blockers.
				fail = true;
			}
		}

		if !fail {
			return Some(magic);
		}
	}

	None
}

fn write_magics(name: &str, magics: &[u64], file: &mut std::fs::File) {
	file.write_fmt(format_args!("pub const {}: [u64; 64] = [", name)).unwrap();
	for i in 0..16 {
		file.write(b"\n\t").unwrap();
		for j in 0..4 {
			let index = i * 4 + j;
			file.write_fmt(format_args!("0x{:016x}, ", magics[index])).unwrap();
		}
	}
	file.write(b"\n];\n").unwrap();
}

fn main() {

	let rook_magics = (0..64)
		.map(|square| find_magic(BoardType::Rook, square))
		.collect::<Option<Vec<u64>>>()
		.expect("Failed to create all rook magic numbers");

	let bishop_magics = (0..64)
		.map(|square| find_magic(BoardType::Bishop, square))
		.collect::<Option<Vec<u64>>>()
		.expect("Failed to create all bishop magic numbers");

	let mut output_file = std::fs::File::create("core/src/position/magics.rs").unwrap();
	write_magics("ROOK_MAGICS", &rook_magics, &mut output_file);
	write_magics("BISHOP_MAGICS", &bishop_magics, &mut output_file);
	
}