use super::{board::{Board, Square}, Piece, magics, Color, Position};

pub const KNIGHT_ATTACKS: [u64; 64] = generate_attacks::<8>(KNIGHT_MOVES);
pub const KING_ATTACKS: [u64; 64] = generate_attacks::<8>(KING_MOVES);
pub const PAWN_ATTACKS: [[u64; 64]; 2] = [
	generate_attacks::<2>(PAWN_MOVES[0]),
	generate_attacks::<2>(PAWN_MOVES[1]),
];

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

const PAWN_MOVES: [[i8; 2]; 2] = [
	[
		Direction::North as i8 + Direction::East as i8,
		Direction::North as i8 + Direction::West as i8,
	],
	[
		Direction::South as i8 + Direction::East as i8,
		Direction::South as i8 + Direction::West as i8,
	]
];

const fn generate_attacks<const N: usize>(move_board: [i8; N]) -> [u64; 64] {
	let mut boards = [0; 64];
	let mut position_index = 0i8;
	while position_index < 64 {
		let file = position_index % 8;
		let mut move_index = 0;
		let mut board = 0u64;
		while move_index < N {
			let attacked_square = position_index + move_board[move_index];
			let attacked_file = attacked_square % 8;
			if attacked_square > -1 && attacked_square < 64 && (attacked_file - file).abs() < 3 {
				board |= 0x1u64 << attacked_square;
			}
			move_index += 1;
		}
		boards[position_index as usize] = board;
		position_index += 1;
	}
	boards
}

pub fn get_attacks(position: Board, square: Square, piece: Piece, color: Color) -> Board {
	match piece {
		Piece::Pawn => {
			Board::from(PAWN_ATTACKS[color as usize][square.lsb_index()])
		}
		Piece::Knight => Board::from(KNIGHT_ATTACKS[square.lsb_index()]),
		Piece::King => Board::from(KING_ATTACKS[square.lsb_index()]),
		Piece::Rook => {
			let square = square.lsb_index();
			let magic = magics::ROOK_MAGICS[square];
			let shift = magics::ROOK_MAGIC_SHIFT[square];
			let mask = magics::ROOK_MASKS[square];
			let blockers: usize = (position & mask).as_usize();
			let magic_index = (blockers.wrapping_mul(magic)) >> shift;
			Board::from(magics::ROOK_BOARDS[square][magic_index as usize])
		}
		Piece::Bishop => {
			let square = square.lsb_index();
			let magic = magics::BISHOP_MAGICS[square];
			let shift = magics::BISHOP_MAGIC_SHIFT[square];
			let mask = magics::BISHOP_MASKS[square];
			let blockers = (position & mask).as_usize();
			let magic_index = (blockers.wrapping_mul(magic)) >> shift;
			Board::from(magics::BISHOP_BOARDS[square][magic_index as usize])
		}
		Piece::Queen => {
			let square = square.lsb_index();
			let rook_magic = magics::ROOK_MAGICS[square];
			let rook_shift = magics::ROOK_MAGIC_SHIFT[square];
			let rook_mask = magics::ROOK_MASKS[square];
			let blockers = (position & rook_mask).as_usize();
			let rook_magic_index = (blockers.wrapping_mul(rook_magic)) >> rook_shift;
			let rook_board = Board::from(magics::ROOK_BOARDS[square][rook_magic_index as usize]);

			let bishop_magic = magics::BISHOP_MAGICS[square];
			let bishop_shift = magics::BISHOP_MAGIC_SHIFT[square];
			let bishop_mask = magics::BISHOP_MASKS[square];
			let bishop_blockers = (position & bishop_mask).as_usize();
			let bishop_magic_index = (bishop_blockers.wrapping_mul(bishop_magic)) >> bishop_shift;
			let bishop_board = Board::from(magics::BISHOP_BOARDS[square][bishop_magic_index as usize]);

			bishop_board | rook_board
		}
	}
}

pub fn get_attacked_squares(position: &Position, attacker_color: Color) -> Board {
	let mut attacks = Board::default();
	let attacker_board = position.get_board_for_color(attacker_color);
	let occupied = position.get_occupancy_board();

	for piece in Piece::iter() {
		for attacker_square in position.get_board_for_piece(piece) & attacker_board {
			attacks |= get_attacks(occupied, attacker_square, piece, attacker_color)
		}
	}
	attacks
}

pub fn attackers_of_square(square: Square, attacker_color: Color, position: &Position) -> Board {
	let mut attackers_board = Board::default();
	let attacker_occupancy = position.get_board_for_color(attacker_color);
	let total_occupancy = position.get_occupancy_board();
	let square_index = square.lsb_index();

	let knight_board = position.get_board_for_piece(Piece::Knight);
	attackers_board |= knight_board & attacker_occupancy & KNIGHT_ATTACKS[square_index];

	let bishop_queen_board = attacker_occupancy
	 & (position.get_board_for_piece(Piece::Bishop) | position.get_board_for_piece(Piece::Queen));
	let diagonal_attacks = get_attacks(total_occupancy, square, Piece::Bishop, attacker_color);
	attackers_board |= diagonal_attacks & bishop_queen_board;

	let rook_queen_board = attacker_occupancy
	 & (position.get_board_for_piece(Piece::Rook) | position.get_board_for_piece(Piece::Queen));
	let rank_file_attacks = get_attacks(total_occupancy, square, Piece::Rook, attacker_color);
	attackers_board |= rank_file_attacks & rook_queen_board;

	let king_board = position.get_board_for_piece(Piece::King);
	attackers_board |= king_board & attacker_occupancy & KING_ATTACKS[square_index];

	// Pawns are a little different since they can only move forward and do slide attacks
	let pawn_attacks = get_attacks(total_occupancy, square, Piece::Pawn, !attacker_color);
	let pawn_occupancy = position.get_board_for_piece(Piece::Pawn);
	attackers_board |= pawn_attacks & attacker_occupancy & pawn_occupancy;

	attackers_board
}