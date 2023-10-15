use rand::{rngs::SmallRng, RngCore, SeedableRng};

use super::{
	board::{Board, Square, ROOKS, ROOK_CASTLE_MOVE},
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
				hash ^= ZOBRIST_RANDOMS[hash_index(square, piece, color)];
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
pub fn hash_after_move(old_position: &Position, new_position: &Position, to_apply: &Move) -> u64 {
	let mut hash = old_position.hash();
	let piece = old_position.piece_on(to_apply.start).unwrap();
	let to_move = old_position.to_move();

	// Update the hash to reflect the moved piece.
	hash ^= ZOBRIST_RANDOMS[hash_index(to_apply.start, piece, to_move)];
	hash ^= ZOBRIST_RANDOMS[hash_index(to_apply.target, piece, to_move)];

	// Update the hash to remove a captured piece
	if to_apply.flags.is_en_passant() {
		let capture_square = to_apply.target >> (8 * MOVE_DIRECTION[to_move as usize]);
		hash ^= ZOBRIST_RANDOMS[hash_index(capture_square, Piece::Pawn, !to_move)];
	} else if to_apply.flags.is_capture() {
		let captured_piece = old_position.piece_on(to_apply.target).unwrap();
		hash ^= ZOBRIST_RANDOMS[hash_index(to_apply.target, captured_piece, !to_move)];
	} else if let Some(side) = to_apply.flags.castling_side() {
		// Castled, gotta update the rook locations also
		let rook_start = ROOKS[side as usize][to_move as usize];
		hash ^= ZOBRIST_RANDOMS[hash_index(rook_start, Piece::Rook, to_move)];

		let rook_end = Square::from(ROOK_CASTLE_MOVE[side as usize][to_move as usize] ^ rook_start);
		hash ^= ZOBRIST_RANDOMS[hash_index(rook_end, Piece::Rook, to_move)];
	}

	// Always toggle to move.
	hash ^= ZOBRIST_RANDOMS[TO_MOVE_ZOBRIST_INDEX];

	// Update castling rights
	if old_position.can_castle(to_move, CastleSide::King)
		^ new_position.can_castle(to_move, CastleSide::King)
	{
		hash ^= ZOBRIST_RANDOMS[CASTLING_ZOBRIST_INDEX + (2 * to_move as usize)];
	}

	if old_position.can_castle(to_move, CastleSide::Queen)
		^ new_position.can_castle(to_move, CastleSide::Queen)
	{
		hash ^= ZOBRIST_RANDOMS[CASTLING_ZOBRIST_INDEX + (2 * to_move as usize) + 1];
	}

	// Update the hash from en passant
	if let Some(square) = old_position.en_passant_square() {
		let en_passant_file = square.file() as usize;
		hash ^= ZOBRIST_RANDOMS[EN_PASSANT_ZOBRIST_INDEX + en_passant_file];
	}

	if let Some(square) = new_position.en_passant_square() {
		let en_passant_file = square.file() as usize;
		hash ^= ZOBRIST_RANDOMS[EN_PASSANT_ZOBRIST_INDEX + en_passant_file];
	}
	hash
}

fn hash_index(square: Square, piece: Piece, color: Color) -> usize {
	// 1 value per piece per square. White comes first, so offset black by
	// (6 pieces) * (64 squares) on the board.
	(color as usize) * (6 * 64)
		// Each piece has on value per square, so we need to offset by 
		// (64 squares) * index_of_piece. Subtracting 2 because piece
		// ordering starts at 2 to make indexing bitboards of the position
		// by color or piece the same.
		+ (piece as usize - 2) * 64
		// And finally, plus the index of the square, starting at 0 A1
		// up to 63 for H8. 
		+ square.lsb_index()
}

#[cfg(test)]
mod test {

	use super::*;

	#[test]
	pub fn test_hash_update() {
		let position = Position::default();

		// En passant created
		let m = Move::from_uci_str("e2e4", &position);
		let running_hash = hash_after_move(&position, &position.apply_move(&m), &m);
		let position = position.apply_move(&m);
		let expected_hash = hash_position(
			&Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
				.unwrap(),
		);
		assert_eq!(expected_hash, running_hash);

		// En passant cleared
		let m = Move::from_uci_str("g8f6", &position);
		let running_hash = hash_after_move(&position, &position.apply_move(&m), &m);
		let position = position.apply_move(&m);
		let expected_hash = hash_position(
			&Position::from_fen("rnbqkb1r/pppppppp/5n2/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1")
				.unwrap(),
		);
		assert_eq!(expected_hash, running_hash);

		// Castling rights are gone for white
		let m = Move::from_uci_str("e1e2", &position);
		let running_hash = hash_after_move(&position, &position.apply_move(&m), &m);
		let position = position.apply_move(&m);
		let expected_hash = hash_position(
			&Position::from_fen("rnbqkb1r/pppppppp/5n2/8/4P3/8/PPPPKPPP/RNBQ1BNR b kq - 0 1")
				.unwrap(),
		);
		assert_eq!(expected_hash, running_hash);

		// Captured a piece
		let m = Move::from_uci_str("f6e4", &position);
		let running_hash = hash_after_move(&position, &position.apply_move(&m), &m);
		let expected_hash = hash_position(
			&Position::from_fen("rnbqkb1r/pppppppp/8/8/4n3/8/PPPPKPPP/RNBQ1BNR w kq - 0 1")
				.unwrap(),
		);
		assert_eq!(expected_hash, running_hash);
	}
}
