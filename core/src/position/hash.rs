use rand::{rngs::SmallRng, RngCore, SeedableRng};

use super::{
	board::{Board, CASTLE_RIGHTS_SQUARES},
	moves::{Move, MOVE_DIRECTION},
	CastleSide, Color, Piece, Position, PositionMetadata,
};

lazy_static::lazy_static! {
	static ref ZOBRIST_RANDOMS: [u64; ZOBRIST_COUNT] = init_zobrist();
}

// 768 = 2 colors * 6 piece types * 64 squares
// 1 for if black is to move
// 4 for castling rights
// 8 for en passant file
const ZOBRIST_COUNT: usize = 781;
// Using a seed to ensure our randoms are the same each time they're generated,
// allow me to (one day) search for a good seed that minimizes hash collisions.
const ZOBRIST_RANDOM_SEED: u64 = 0x34e1a1e8655c3200;
// After all the piece moves -- 2 sides, six pieces each, 64 squares on the board.
const TO_MOVE_ZOBRIST_INDEX: usize = 2 * 6 * 64;
// Castling values start right after the to move value.
const CASTLING_ZOBRIST_INDEX: usize = TO_MOVE_ZOBRIST_INDEX + 1;
// En passant values start after the 4 castling values.
const EN_PASSANT_ZOBRIST_INDEX: usize = CASTLING_ZOBRIST_INDEX + 4;

fn init_zobrist() -> [u64; 781] {
	let mut random = SmallRng::seed_from_u64(ZOBRIST_RANDOM_SEED);
	let mut zobrist_randoms = [0; ZOBRIST_COUNT];
	for i in 0..ZOBRIST_COUNT {
		zobrist_randoms[i] = random.next_u64();
	}
	zobrist_randoms
}

/// Hash a position. This is inherently lossy and collision will occur --
/// the total number of chess positions is on the order of 10^46, whereas
/// the number of unique 64 bit values is on the order of 10^19. There
/// isn't much we can do to avoid that, and it's a tradeoff that we need to
/// accept if we want the benefits of a position cache (and we do).
pub fn hash_position(position: &Position) -> u64 {
	hash(&position.boards, &position.metadata)
}

pub fn hash(boards: &[Board; 8], metadata: &PositionMetadata) -> u64 {
	let mut hash = 0;

	// Hash components from the piece locations
	for piece in Piece::iter() {
		let piece_locations = boards[piece as usize];
		for color in [Color::Black, Color::White] {
			let color_board = boards[color as usize];
			for square in piece_locations & color_board {
				let zobirst_value_index = (color as usize) * (64 * 6) + // Select the right color range
					(piece as usize - 2) * 64 + // Select the right piece range
					square.lsb_index(); // Select the square that the piece is on

				hash ^= ZOBRIST_RANDOMS[zobirst_value_index];
			}
		}
	}

	// Hash components from to move
	if let Color::Black = metadata.to_move() {
		hash ^= ZOBRIST_RANDOMS[TO_MOVE_ZOBRIST_INDEX];
	}

	// Hash component from castling
	if metadata.can_castle(Color::White, super::CastleSide::Queen) {
		hash ^= ZOBRIST_RANDOMS[CASTLING_ZOBRIST_INDEX];
	}

	if metadata.can_castle(Color::White, super::CastleSide::King) {
		hash ^= ZOBRIST_RANDOMS[CASTLING_ZOBRIST_INDEX + 1];
	}

	if metadata.can_castle(Color::Black, super::CastleSide::Queen) {
		hash ^= ZOBRIST_RANDOMS[CASTLING_ZOBRIST_INDEX + 2];
	}

	if metadata.can_castle(Color::Black, super::CastleSide::King) {
		hash ^= ZOBRIST_RANDOMS[CASTLING_ZOBRIST_INDEX + 3];
	}
	// Hash component from en passant
	if let Some(square) = metadata.en_passant_square() {
		hash ^= ZOBRIST_RANDOMS[EN_PASSANT_ZOBRIST_INDEX + square.file() as usize];
	}

	hash
}

/// Calculating the original hash is expensive, so we want to avoid doing it as much as
/// possible. This function will produce the hash value of a position after a given move,
/// assuming that it's given the hash of the starting position.
pub fn hash_after_move(old: u64, starting_position: &Position, to_apply: &Move) -> u64 {
	let mut hash = old;
	let piece = starting_position.piece_on(to_apply.start).unwrap();
	let to_move = starting_position.metadata.to_move();

	// Update the hash to reflect the moved piece.
	let origin_square_index =
		(to_move as usize) * (6 * 64) + (piece as usize - 2) * 64 + to_apply.start.lsb_index();
	hash ^= ZOBRIST_RANDOMS[origin_square_index];

	let destination_square_index =
		(to_move as usize) * (6 * 64) + (piece as usize - 2) * 64 + to_apply.target.lsb_index();
	hash ^= ZOBRIST_RANDOMS[destination_square_index];

	// Update the hash to remove a captured piece
	if to_apply.flags.is_en_passant() {
		let capture_square = to_apply.target >> (8 * MOVE_DIRECTION[to_move as usize]);
		let captured_piece_square_index = (1 - to_move as usize) * (6 * 64)
			+ (Piece::Pawn as usize - 2) * 64
			+ capture_square.lsb_index();
		hash ^= ZOBRIST_RANDOMS[captured_piece_square_index];
	} else if to_apply.flags.is_capture() {
		let captured_piece = starting_position.piece_on(to_apply.target).unwrap();
		let captured_piece_square_index = (1 - to_move as usize) * (6 * 64)
			+ (captured_piece as usize - 2) * 64
			+ to_apply.target.lsb_index();
		hash ^= ZOBRIST_RANDOMS[captured_piece_square_index];
	}

	// Always toggle to move.
	hash ^= ZOBRIST_RANDOMS[TO_MOVE_ZOBRIST_INDEX];

	// Update castling rights
	if piece == Piece::King {
		if starting_position.can_castle(to_move, CastleSide::King) {
			hash ^= ZOBRIST_RANDOMS[CASTLING_ZOBRIST_INDEX + to_move as usize];
		}

		if starting_position.can_castle(to_move, CastleSide::Queen) {
			hash ^= ZOBRIST_RANDOMS[CASTLING_ZOBRIST_INDEX + to_move as usize + 1];
		}
	}

	if piece == Piece::Rook && (CASTLE_RIGHTS_SQUARES & to_apply.start).has_pieces() {
		let side = CastleSide::from(to_apply.start);
		if starting_position.can_castle(to_move, side) {
			hash ^=
				ZOBRIST_RANDOMS[CASTLING_ZOBRIST_INDEX + (2 * to_move as usize) + side as usize];
		}
	}

	if to_apply.flags.is_capture() && (CASTLE_RIGHTS_SQUARES & to_apply.target).has_pieces() {
		let side = CastleSide::from(to_apply.target);
		if starting_position.can_castle(!to_move, side) {
			hash ^=
				ZOBRIST_RANDOMS[CASTLING_ZOBRIST_INDEX + (2 * !to_move as usize) + side as usize];
		}
	}

	// Update the hash from en passant
	if let Some(square) = starting_position.metadata.en_passant_square() {
		let en_passant_file = square.file() as usize;
		hash ^= ZOBRIST_RANDOMS[EN_PASSANT_ZOBRIST_INDEX + en_passant_file];
	}

	if to_apply.flags.is_double_pawn_push() {
		let en_passant_file = to_apply.target.file() as usize;
		hash ^= ZOBRIST_RANDOMS[EN_PASSANT_ZOBRIST_INDEX + en_passant_file];
	}

	hash
}

#[cfg(test)]
mod test {

	use super::*;

	#[test]
	pub fn test_hash_update() {
		let mut position = Position::default();
		let mut running_hash = hash_position(&position);

		// En passant created
		let m = Move::from_uci_str("e2e4", &position);
		running_hash = hash_after_move(running_hash, &position, &m);
		position = position.apply_move(&m);
		let expected_hash = hash_position(
			&Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
				.unwrap(),
		);
		assert_eq!(expected_hash, running_hash);

		// En passant cleared
		let m = Move::from_uci_str("g8f6", &position);
		running_hash = hash_after_move(running_hash, &position, &m);
		position = position.apply_move(&m);
		let expected_hash = hash_position(
			&Position::from_fen("rnbqkb1r/pppppppp/5n2/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1")
				.unwrap(),
		);
		assert_eq!(expected_hash, running_hash);

		// Castling rights are gone for white
		let m = Move::from_uci_str("e1e2", &position);
		running_hash = hash_after_move(running_hash, &position, &m);
		position = position.apply_move(&m);
		let expected_hash = hash_position(
			&Position::from_fen("rnbqkb1r/pppppppp/5n2/8/4P3/8/PPPPKPPP/RNBQ1BNR b kq - 0 1")
				.unwrap(),
		);
		assert_eq!(expected_hash, running_hash);

		// Captured a piece
		let m = Move::from_uci_str("f6e4", &position);
		running_hash = hash_after_move(running_hash, &position, &m);
		position = position.apply_move(&m);
		let expected_hash = hash_position(
			&Position::from_fen("rnbqkb1r/pppppppp/8/8/4n3/8/PPPPKPPP/RNBQ1BNR w kq - 0 1")
				.unwrap(),
		);
		assert_eq!(expected_hash, running_hash);
	}
}
