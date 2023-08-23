use super::board::Board;

pub const KNIGHT_ATTACKS: [Board; 64] = generate_attacks(KNIGHT_MOVES);
pub const KING_ATTACKS: [Board; 64] = generate_attacks(KING_MOVES);

#[derive(Copy, Clone)]
enum Direction {
	North = 8,
	NorthEast = 9,
	East = 1,
	SouthEast = -7,
	South = -8,
	SouthWest = -9,
	West = -1,
	NorthWest = 7,
}

impl Direction {
	pub fn diagonals() -> impl Iterator<Item = Direction> {
		[
			Direction::NorthEast,
			Direction::SouthEast,
			Direction::SouthWest,
			Direction::NorthWest,
		]
		.iter()
		.copied()
	}

	pub fn cardinal_directions() -> impl Iterator<Item = Direction> {
		[
			Direction::North,
			Direction::East,
			Direction::South,
			Direction::West,
		]
		.iter()
		.copied()
	}

	pub fn iter() -> impl Iterator<Item = Direction> {
		[
			Direction::North,
			Direction::NorthEast,
			Direction::East,
			Direction::SouthEast,
			Direction::South,
			Direction::SouthWest,
			Direction::West,
			Direction::NorthWest,
		]
		.iter()
		.copied()
	}
}

const KNIGHT_MOVES: [i8; 8] = [
	2 * Direction::North as i8 + Direction::East as i8,
	Direction::North as i8 + 2 * Direction::East as i8,
	Direction::South as i8 + 2 * Direction::East as i8,
	2 * Direction::South as i8 + Direction::East as i8,
	2 * Direction::South as i8 + Direction::West as i8,
	Direction::South as i8 + 2 * Direction::West as i8,
	Direction::North as i8 + 2 * Direction::West as i8,
	2 * Direction::North as i8 + Direction::West as i8,
];

const KING_MOVES: [i8; 8] = [
	Direction::North as i8,
	Direction::NorthEast as i8,
	Direction::East as i8,
	Direction::SouthEast as i8,
	Direction::South as i8,
	Direction::SouthWest as i8,
	Direction::West as i8,
	Direction::NorthWest as i8,
];

const fn generate_attacks(move_board: [i8; 8]) -> [Board; 64] {
	let mut boards = [Board::default(); 64];
	let mut position_index = 0i8;
	while position_index < 64 {
		let file = position_index % 8;
		let mut move_index = 0;
		let mut board = 0u64;
		while move_index < move_board.len() {
			let attacked_square = position_index + move_board[move_index];
			let attacked_file = attacked_square % 8;
			if attacked_square > 0 && attacked_square < 64 && (attacked_file - file).abs() < 3 {
				board |= 0x1u64 << attacked_square;
			}
			move_index += 1;
		}
		boards[position_index as usize] = Board::from(board);
		position_index += 1;
	}
	boards
}

const ROOK_ATTACKS: [[Board; 4096]; 64] = todo!();
const BISHOP_ATTACKS: [[Board; 4096]; 64] = todo!();

const fn generate_sliding_attacks() -> [[Board; 4096]; 64] {
	let mut boards = [[Board::default(); 4096]; 64];
	let position_index = 0;
	while position_index < 64 {
		
	}
	
	boards
}

