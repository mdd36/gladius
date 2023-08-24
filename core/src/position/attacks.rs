use super::{board::{Board, Square}, Piece, magics};

pub const KNIGHT_ATTACKS: [u64; 64] = generate_attacks(KNIGHT_MOVES);
pub const KING_ATTACKS: [u64; 64] = generate_attacks(KING_MOVES);

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

const fn generate_attacks(move_board: [i8; 8]) -> [u64; 64] {
	let mut boards = [0; 64];
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
		boards[position_index as usize] = board;
		position_index += 1;
	}
	boards
}

pub fn get_attacks(position: Board, square: Square, piece: Piece) -> Board {
	match piece {
		Piece::Pawn => todo!(), // Maybe pawns should be calculated elsewhere? So many edge cases
		Piece::Knight => Board::from(KNIGHT_ATTACKS[square.as_usize()]),
		Piece::King => Board::from(KING_ATTACKS[square.as_usize()]),
		Piece::Rook => {
			let square = square.as_usize();
			let magic = magics::ROOK_MAGICS[square];
			let shift = magics::ROOK_MAGIC_SHIFT[square];
			let magic_index = (position * magic) >> shift;
			Board::from(magics::ROOK_BOARDS[square][magic_index])
		}
		Piece::Bishop => {
			let square = square.as_usize();
			let magic = magics::BISHOP_MAGICS[square];
			let shift = magics::BISHOP_MAGIC_SHIFT[square];
			let magic_index = (position * magic) >> shift;
			Board::from(magics::BISHOP_BOARDS[square][magic_index])
		}
		Piece::Queen => {
			let square = square.as_usize();
			let rook_magic = magics::ROOK_MAGICS[square];
			let rook_shift = magics::ROOK_MAGIC_SHIFT[square];
			let rook_magic_index = (position * rook_magic) >> rook_shift;
			let rook_board = Board::from(magics::ROOK_BOARDS[square][rook_magic_index]);

			let bishop_magic = magics::BISHOP_MAGICS[square];
			let bishop_shift = magics::BISHOP_MAGIC_SHIFT[square];
			let bishop_magic_index = (position * bishop_magic) >> bishop_shift;
			let bishop_board = Board::from(magics::BISHOP_BOARDS[square][bishop_magic_index]);

			bishop_board | rook_board
		}
	}
}
