pub mod material;
pub mod positioning;
pub mod repetition;

use self::{
	material::material_score,
	positioning::{king_safety, pawn_structure, positioning_score},
};
use crate::position::{Piece, Position};

const PHASE_WEIGHTS: [u32; 7] = [0, 0, 0, 2, 1, 1, 4];
const TOTAL_PHASE: f64 = 24.0;

pub const MAX_SCORE: i16 = std::i16::MAX;
pub const MIN_SCORE: i16 = std::i16::MIN + 1; // To account for 2's complement
pub const CHECKMATE_SCORE: i16 = MIN_SCORE + std::u8::MAX as i16;

pub fn evaluate_position(position: &Position) -> i16 {
	let us = position.metadata.to_move();
	let them = !us;

	let our_material = material_score(position, us);
	let their_material = material_score(position, them);
	let material_difference = our_material - their_material;

	let our_position = positioning_score(position, us);
	let their_position = positioning_score(position, them);
	let positioning_difference = our_position - their_position;

	let our_pawn_structure = pawn_structure(position, us);
	let their_pawn_structure = pawn_structure(position, them);
	let pawn_structure_difference = our_pawn_structure - their_pawn_structure;

	let our_king_safety = king_safety(position, us);
	let their_king_safety = king_safety(position, them);
	let king_safety_difference = our_king_safety - their_king_safety;

	// let our_attacks = attacks_score(position, us);
	// let their_attacks = attacks_score(position, them);
	// let attack_difference = our_attacks - their_attacks;

	material_difference
		+ positioning_difference
		+ pawn_structure_difference
		+ king_safety_difference
	// + attack_difference
}

/// Get a float that represents the current phase of the game based
/// on remaining material. Approach is inspired by the CPW:
/// https://www.chessprogramming.org/Tapered_Eval
fn game_phase(position: &Position) -> f64 {
	let mut phase = 0;
	phase += position.get_board_for_piece(Piece::Rook).count_ones()
		* PHASE_WEIGHTS[Piece::Rook as usize];
	phase += position.get_board_for_piece(Piece::Knight).count_ones()
		* PHASE_WEIGHTS[Piece::Knight as usize];
	phase += position.get_board_for_piece(Piece::Bishop).count_ones()
		* PHASE_WEIGHTS[Piece::Bishop as usize];
	phase += position.get_board_for_piece(Piece::Queen).count_ones()
		* PHASE_WEIGHTS[Piece::Queen as usize];

	((TOTAL_PHASE - phase as f64) * 256.0 + (TOTAL_PHASE / 2.0)) / TOTAL_PHASE
}

fn combine_phase_scores(position: &Position, early_game: i16, end_game: i16) -> i16 {
	let phase = game_phase(position);

	(((early_game as f64 * (256.0 - phase)) + (end_game as f64 * phase)) / 256.0) as i16
}
