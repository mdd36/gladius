/* Adapted from the C++ example given on the chess programming wiki by
 * Tord Romstad: https://www.chessprogramming.org/Looking_for_Magics 
 */

use std::io::prelude::*;

#[derive(Clone)]
struct Magic {
	magic: u64,
	bits: u64,
	mask: u64,
	moves: [u64; 4096],
}

impl Default for Magic {
	fn default() -> Self {
		Self {
			magic: 0,
			bits: 0,
			mask: 0,
			moves: [0; 4096],
		}
	}
}

/// A really simple random u64 generator using /dev/urandom to avoid
/// needing to depend on the `rand` crate.
fn random() -> u64 {
	let mut f = std::fs::File::open("/dev/urandom").unwrap();
	let mut buff: [u8; 8] = [0; 8];

	f.read_exact(&mut buff).unwrap();
	u64::from_be_bytes(buff)
}

#[derive(Copy, Clone)]
enum BoardType {
	Bishop,
	Rook
}

impl BoardType {
	pub fn mask_for_square(&self, square: u64) -> u64 {
		match self {
			Self::Bishop => self.bishop_mask(square),
			Self::Rook => self.rook_mask(square),
		}
	}

	pub fn moves(&self, square: u64, blockers: u64) -> u64 {
		match self {
			Self::Bishop => self.bishop_moves(square, blockers),
			Self::Rook => self.rook_moves(square, blockers),
		}
	}

	fn bishop_mask(&self, square: u64) -> u64 {
		let mut mask = 0u64;
		let rank = square / 8;
		let file = square % 8;

		for (r, f) in (rank+1..7).zip(file+1..7) {
			mask |= 1 << f + r * 8;
		}

		for (r, f) in (rank+1..7).zip((1..file).rev()) {
			mask |= 1 << f + r * 8;
		}

		for (r, f) in (1..rank).rev().zip(file+1..7) {
			mask |= 1 << f + r * 8;
		}

		for (r, f) in (1..rank).rev().zip((1..file).rev()) {
			mask |= 1 << f + r * 8;
		}

		mask
	}

	fn rook_mask(&self, square: u64) -> u64 {
		let mut mask = 0u64;
		let rank = square / 8;
		let file = square % 8;

		for r in (rank+1)..7 {
			mask |= 1 << file + r * 8;
		}

		for r in (1..rank).rev() {
			mask |= 1 << file + r * 8;
		}

		for f in (file+1)..7 {
			mask |= 1 << f + rank * 8;
		}

		for f in (1..file).rev() {
			mask |= 1 << f + rank * 8;
		}

		mask
	}

	fn bishop_moves(&self, square: u64, blockers: u64) -> u64 {
		let mut moves = 0u64;
		let rank = square / 8;
		let file = square % 8;

		for (r, f) in (rank+1..8).zip(file+1..8) {
			let attacked_square = 1 << f + r * 8;
			moves |= attacked_square;
			if attacked_square & blockers != 0 {
				break;
			}
		}

		for (r, f) in (rank+1..8).zip((0..file).rev()) {
			let attacked_square = 1 << f + r * 8;
			moves |= attacked_square;
			if attacked_square & blockers != 0 {
				break;
			}
		}

		for (r, f) in (0..rank).rev().zip(file+1..8) {
			let attacked_square = 1 << f + r * 8;
			moves |= attacked_square;
			if attacked_square & blockers != 0 {
				break;
			}
		}

		for (r, f) in (0..rank).rev().zip((0..file).rev()) {
			let attacked_square = 1 << f + r * 8;
			moves |= attacked_square;
			if attacked_square & blockers != 0 {
				break;
			}
		}

		moves
	}

	fn rook_moves(&self, square: u64, blockers: u64) -> u64 {
		let mut moves = 0;
		let rank = square / 8;
		let file = square % 8;

		for r in rank+1..8 {
			let attacked_square = 1 << (file + (8 * r));
			moves |= attacked_square;
			if blockers & attacked_square != 0 { 
				break;
			}
		}

		for r in (0..rank).rev() {
			let attacked_square = 1 << (file + (8 * r));
			moves |= attacked_square;
			if blockers & attacked_square != 0 { 
				break;
			}
		}

		for f in file+1..8 {
			let attacked_square = 1 << (f + (8 * rank));
			moves |= attacked_square;
			if blockers & attacked_square != 0 { 
				break;
			}
		}

		for f in (0..file).rev() {
			let attacked_square = 1 << (f + (8 * rank));
			moves |= attacked_square;
			if blockers & attacked_square != 0 { 
				break;
			}
		}

		moves
	}
}

fn gen_all_blocker_configurations(mask: u64) -> [u64; 4096] {
	let mut blocker_boards = [0; 4096];
	let mut current_board = 0u64;

	for i in 0..(1 << mask.count_ones()) {
		blocker_boards[i as usize] = current_board;
		current_board = (current_board.wrapping_sub(mask)) & mask;
	}

	blocker_boards
}

fn random_magic() -> u64 {
	// Want a sparse number, so we'll thin the random with ands
	random() & random() & random()
}

fn find_magic(board_type: BoardType, square: u64, tx: std::sync::mpsc::Sender<(u64, Magic)>) {
	let mask = board_type.mask_for_square(square);
	let mut magic_bits = 12;
	let num_ones = mask.count_ones() as u64;

	let blocker_boards = gen_all_blocker_configurations(mask);
	let mut moves = [0; 4096];

	for i in 0..(1 << num_ones) {
		moves[i] = board_type.moves(square, blocker_boards[i]);
	}

	'attempt_loop: loop {
		let magic = random_magic();

		let mut used = [0; 4096];
		for i in 0..(1 << num_ones) {
			let position_index = 
				((blocker_boards[i].wrapping_mul(magic)) >> (64 - magic_bits)) as usize;
			
			if used[position_index] != 0 && used[position_index] != moves[i] {
				// We can only share the same magic as another position
				// if the two are blocked by the same pieces, which means that
				// the valid moves are the same even if there's other pieces
				// behind those blockers.
				continue 'attempt_loop;
			}

			used[position_index] = moves[i];
		}

		let magic_struct = Magic { magic, bits: magic_bits, mask, moves: used };
		tx.send((square, magic_struct)).unwrap();
		magic_bits -= 1;
	}
}

fn create_magic_file(board_type: BoardType, file_name: &str, stats_tx: std::sync::mpsc::Sender<(u64, u32)>) {
	let (tx, rx) = std::sync::mpsc::channel();
	let mut magics = Vec::with_capacity(64);

	for square in 0..64 {
		magics.push(Magic::default());
		let tx_clone = tx.clone();
		let _ = std::thread::spawn(move || find_magic(board_type, square as u64, tx_clone));
	}


	loop {
		if let Ok((square, magic)) = rx.recv() {
			magics[square as usize] = magic;
		}

		let mut num_found = 0;
		let mut total_size = 0;
		for Magic { magic, bits, .. } in &magics {
			if *magic == 0 {
				total_size += 2u32.pow(12) * 8 / 1024;
			} else {
				num_found += 1;
				total_size += 2u32.pow(*bits as u32) * 8 / 1024;
			}
		}

		if num_found == 64 {
			let mut magics_file = std::fs::File::create(file_name).unwrap();
			magics_file.write(b"[\n").unwrap();
			for Magic { magic, mask, bits, moves } in &magics {
				magics_file.write(b"\t{\n").unwrap();
				magics_file.write_fmt(format_args!(
					"\t\t\"magic\": \"0x{magic:016x}\",\n\t\t\"bits\": {bits},\n\t\t\"mask\": \"0x{mask:016x}\",\n\t\t\"moves\": [\n"
				)).unwrap();
				let x = moves.iter()
					.take(2usize.pow(*bits as u32))
					.map(|m| format!("\t\t\t\"0x{m:016x}\""))
					.reduce(|acc, s| acc + ",\n" + &s)
					.unwrap_or_default();
				magics_file.write(x.as_bytes()).unwrap();			
				magics_file.write(b"\n\t\t]\n\t},\n").unwrap();
			}
			magics_file.write(b"]").unwrap();
		}

		stats_tx.send((num_found, total_size)).unwrap();
	}
}

fn main() {
	let (bishop_tx, bishop_rx) = std::sync::mpsc::channel();
	let (rook_tx, rook_rx) = std::sync::mpsc::channel();

	let _ = std::thread::spawn(move || create_magic_file(BoardType::Bishop, "bishop_magics.json", bishop_tx));
	let _ = std::thread::spawn(move || create_magic_file(BoardType::Rook, "rook_magics.json", rook_tx));


	let mut bishops_found = 0;
	let mut bishops_size = 2048;
	let mut rooks_found = 0;
	let mut rooks_size = 2048;

	loop {
		if let Ok((found, size)) = bishop_rx.try_recv() {
			bishops_found = found;
			bishops_size = size; 
		}
		if let Ok((found, size)) = rook_rx.try_recv() {
			rooks_found = found;
			rooks_size = size;
		}

		print!(
			"Bishops: {:02}/64, {:04}KB Rooks: {:02}/64, {:04}KB\r",
			bishops_found, bishops_size, rooks_found, rooks_size,
		);
	}
}