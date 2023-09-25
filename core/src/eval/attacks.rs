use crate::position::{
	attacks::{get_attacked_squares, get_attacked_squares_for_piece},
	board::Board,
	Color, Piece, Position,
};

use super::material::value_of_piece;

/// A scaling factor that represents that attacking
/// a piece isn't worth the same as capturing it, but it's worth
/// some fraction of capturing that piece.
const ATTACK_POTENTIAL_SCALER: i16 = 2;

pub fn attacks_score(position: &Position, color: Color) -> i16 {
	let mut score = 0;
	let occupancy = position.get_occupancy_board();
	let their_board = position.get_board_for_color(!color);
	let their_attacks = get_attacked_squares(position, !color);

	let pawn_attacks = get_attacked_squares_for_piece(position, color, Piece::Pawn);
	score += evaluate_attacks(
		position,
		Piece::Pawn,
		pawn_attacks,
		their_board,
		their_attacks,
	);

	let rook_attacks = get_attacked_squares_for_piece(position, color, Piece::Rook);
	score += evaluate_attacks(
		position,
		Piece::Rook,
		rook_attacks,
		their_board,
		their_attacks,
	);

	let knight_attacks = get_attacked_squares_for_piece(position, color, Piece::Knight);
	score += evaluate_attacks(
		position,
		Piece::Knight,
		knight_attacks,
		their_board,
		their_attacks,
	);

	let bishop_attacks = get_attacked_squares_for_piece(position, color, Piece::Bishop);
	score += evaluate_attacks(
		position,
		Piece::Bishop,
		bishop_attacks,
		their_board,
		their_attacks,
	);

	let queen_attacks = get_attacked_squares_for_piece(position, color, Piece::Queen);
	score += evaluate_attacks(
		position,
		Piece::Queen,
		queen_attacks,
		their_board,
		their_attacks,
	);

	score
}

fn evaluate_attacks(
	position: &Position,
	piece: Piece,
	attacks_for_piece: Board,
	their_board: Board,
	their_attacks: Board,
) -> i16 {
	let mut score = 0;

	let attacked_pieces = attacks_for_piece & their_board;

	for square in attacked_pieces {
		let attacked_piece_value = value_of_piece(position.piece_on(square).unwrap());
		if their_attacks.is_occupied(square) {
			// This square is defended. Reduce the value of capturing the piece to reflect
			// that we'd likely lose the attacking piece in the process.
			score += std::cmp::max(
				(attacked_piece_value - value_of_piece(piece)) / ATTACK_POTENTIAL_SCALER,
				0,
			);
		} else {
			// This piece is hanging!
			score += attacked_piece_value / ATTACK_POTENTIAL_SCALER;
		}
	}

	score
}
