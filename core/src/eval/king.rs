use std::ops::Add;

use crate::position::{
	attacks,
	board::{file_mask, rank_mask, Square, D_FILE_INDEX, E_FILE_INDEX, H_FILE_INDEX},
	Color, Piece, Position,
};

use super::combine_phase_scores;

/// A bonus or penalty to apply based on the weighted
/// number of attackers of the king's zone
#[rustfmt::skip]
const KING_ATTACK_WEIGHTS: [i16; 8] = [
	0, 0, -10, -20, -50, -100, -200, -400
];

pub fn king_safety(position: &Position, color: Color) -> i16 {
	let mut score = 0;
	let king_square = Square::from(
		position.get_board_for_color(color) & position.get_board_for_piece(Piece::King),
	);

	score += pawn_shield(position, color, king_square);
	score += king_tropism(position, color, king_square);
	score += open_file_penalty(position, color, king_square);
	score += attacked_squares(position, color, king_square);

	combine_phase_scores(position, score, 0)
}

fn pawn_shield(position: &Position, color: Color, king: Square) -> i16 {
	let mut safety_penalty = 0;
	if king.file() > E_FILE_INDEX || king.file() < D_FILE_INDEX {
		// Basically, is the king castled
		let left_file = king.file().saturating_sub(1);
		let right_file = king.file().add(1).min(H_FILE_INDEX);

		let next_king_rank = rank_mask(king.nth_next_rank(1, color));
		let second_king_rank = rank_mask(king.nth_next_rank(2, color));

		let all_pawns = position.get_board_for_piece(Piece::Pawn);
		let our_pawns = all_pawns & position.get_board_for_color(color);

		for file in left_file..=right_file {
			let file_mask = file_mask(file);
			let file_paws = file_mask & our_pawns;

			const PAWN_SHIELD_PENALTY: i16 = -20;

			safety_penalty += if (next_king_rank & file_paws).has_pieces() {
				0
			} else if (second_king_rank & file_paws).has_pieces() {
				PAWN_SHIELD_PENALTY
			} else {
				2 * PAWN_SHIELD_PENALTY
			};
		}
	} else {
		const CASTLING_PENALTY: i16 = -30;
		safety_penalty += CASTLING_PENALTY;
	}

	safety_penalty
}

fn open_file_penalty(position: &Position, color: Color, king_square: Square) -> i16 {
	let mut penalty = 0;
	let orthogonal_attackers =
		position.get_board_for_piece(Piece::Rook) | position.get_board_for_piece(Piece::Queen);
	if (position.get_board_for_color(!color) & orthogonal_attackers).count_ones() > 1 {
		let king_file = king_square.file();
		let left_file = king_square.file().saturating_sub(1);
		let right_file = king_square.file().add(1).min(H_FILE_INDEX);

		let all_pawns = position.get_board_for_piece(Piece::Pawn);
		let our_pawns = position.get_board_for_color(color) & all_pawns;
		let enemy_pawns = all_pawns ^ our_pawns;

		for file in left_file..=right_file {
			let file_mask = file_mask(file);
			let is_king_file = file == king_file;

			const OPEN_FOR_ENEMY_PENALTY: [i16; 2] = [-15, -25];
			if (enemy_pawns & file_mask).is_empty() {
				penalty += OPEN_FOR_ENEMY_PENALTY[is_king_file as usize];
			}

			const OPEN_FOR_US_PENALTY: [i16; 2] = [-10, -15];
			if (our_pawns & file_mask).is_empty() {
				penalty += OPEN_FOR_US_PENALTY[is_king_file as usize];
			}
		}
	}
	penalty
}

fn king_tropism(position: &Position, color: Color, king_square: Square) -> i16 {
	let mut tropism = 0;
	let them = !color;
	let their_position = position.get_board_for_color(them);

	for piece in Piece::non_pawn() {
		let piece_locations = position.get_board_for_piece(piece) & their_position;
		for sq in piece_locations {
			let attacker_weight = match piece {
				Piece::Queen => 3,
				Piece::Rook => 2,
				Piece::Bishop | Piece::Knight => 1,
				p => unreachable!("Shouldn't have gotten {p:?} when evaluating king tropism"),
			};
			tropism -= (14 - sq.distance(king_square) as i16) * attacker_weight;
		}
	}

	tropism
}

fn attacked_squares(position: &Position, color: Color, king_square: Square) -> i16 {
	let king_zone = attacks::KING_ATTACKS[king_square.lsb_index()];
	let attacked_squares = attacks::get_attacked_squares(position, !color);
	let king_zone_attack_count = (attacked_squares & king_zone).count_ones();

	let ratio_attacked = (king_zone_attack_count as f64) / (king_zone.count_ones() as f64);
	let weight_index = ((KING_ATTACK_WEIGHTS.len() - 1) as f64 * ratio_attacked).round() as usize;

	KING_ATTACK_WEIGHTS[weight_index]
}

#[cfg(test)]
mod test {

	use super::*;

	#[test]
	fn queen_castle_pawn_shield() {
		let position = Position::from_fen("2k5/8/8/8/8/8/1PPP4/2K5 w - - 0 1").unwrap();
		let king_square = Square::from_algebraic_notation("c1");
		assert_eq!(0, pawn_shield(&position, Color::White, king_square));

		let position = Position::from_fen("2k5/8/8/8/8/3P4/1PP5/2K5 b - - 0 1").unwrap();
		assert_eq!(-20, pawn_shield(&position, Color::White, king_square));

		let position = Position::from_fen("2k5/8/8/8/8/1P6/2P5/2K5 b - - 0 1").unwrap();
		assert_eq!(-60, pawn_shield(&position, Color::White, king_square));
	}

	#[test]
	fn king_castle_pawn_shield() {
		let position = Position::from_fen("2k5/8/8/8/8/8/5PPP/6K1 w - - 0 1").unwrap();
		let king_square = Square::from_algebraic_notation("g1");
		assert_eq!(0, pawn_shield(&position, Color::White, king_square));

		let position = Position::from_fen("2k5/8/8/8/8/6P1/5P1P/6K1 w - - 0 1").unwrap();
		assert_eq!(-20, pawn_shield(&position, Color::White, king_square));

		let position = Position::from_fen("2k5/8/8/8/8/6P1/5P2/6K1 w - - 0 1").unwrap();
		assert_eq!(-60, pawn_shield(&position, Color::White, king_square));
	}

	#[test]
	fn uncastled_penalty() {
		let position = Position::from_fen("2k5/8/8/8/8/8/3PP3/4K3 b - - 0 1").unwrap();
		let king_square = Square::from_algebraic_notation("e1");
		assert_eq!(-30, pawn_shield(&position, Color::White, king_square));

		let position = Position::from_fen("2k5/8/8/8/8/8/3PP3/3K4 b - - 0 1").unwrap();
		let king_square = Square::from_algebraic_notation("d1");
		assert_eq!(-30, pawn_shield(&position, Color::White, king_square));
	}

	#[test]
	fn open_file() {
		let position = Position::from_fen("1rk4q/5p2/4p3/8/8/8/3PP3/4K3 b - - 0 1").unwrap();
		let king_square = Square::from_algebraic_notation("e1");
		assert_eq!(-25, open_file_penalty(&position, Color::White, king_square));

		let position = Position::from_fen("1rk4q/8/4p3/8/8/8/3P4/4K3 b - - 0 1").unwrap();
		assert_eq!(-55, open_file_penalty(&position, Color::White, king_square));
	}
}
