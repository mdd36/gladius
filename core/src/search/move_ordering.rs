use std::cmp::Ordering;

use crate::{
	eval::material::static_exchange,
	position::{moves::Move, Piece, Position},
};

use super::transposition::TranspositionTable;

pub struct MoveOrderer {
	transposition_table: TranspositionTable,
	killer_moves: Vec<Vec<Move>>,
}

impl MoveOrderer {
	pub fn new(predicted_ply: u8, transposition_table: TranspositionTable) -> Self {
		let killer_moves = vec![Vec::new(); predicted_ply as usize];

		Self {
			transposition_table,
			killer_moves,
		}
	}

	pub fn order(&self, position: &Position, ply: u8, moves: &mut Vec<Move>) {
		let ply = ply as usize;
		moves.sort_unstable_by(|a, b| {
			if self
				.killer_moves
				.get(ply)
				.map_or(false, |killers| killers.contains(a))
			{
				return Ordering::Less;
			}

			if self
				.killer_moves
				.get(ply)
				.map_or(false, |killers| killers.contains(b))
			{
				return Ordering::Greater;
			}

			if a.flags.is_capture() && !b.flags.is_capture() {
				return Ordering::Less;
			}

			if !a.flags.is_capture() && b.flags.is_capture() {
				return Ordering::Greater;
			}

			if a.flags.is_capture() && b.flags.is_capture() {
				// If the option is empty, it means the capture was an en passant so we captured a pawn.
				// All other captures end on the same space as the taken piece.
				let a_taken = position.piece_on(a.target).unwrap_or(Piece::Pawn);
				let a_moved = position.piece_on(a.start).unwrap();
				let b_taken = position.piece_on(b.target).unwrap_or(Piece::Pawn);
				let b_moved = position.piece_on(b.start).unwrap();
				// If both a and b are captures, prioritize the one with the highest static exchange,
				// eg explore PxQ before QxP
				return static_exchange(a_moved, a_taken).cmp(&static_exchange(b_moved, b_taken));
			}

			return Ordering::Equal;
		});
	}

	pub fn add_killer(&mut self, ply: u8, killer_move: Move) {
		let ply = ply as usize;

		if self.killer_moves.len() < ply {
			self.killer_moves.resize(ply, Vec::new());
		}

		self.killer_moves[ply].push(killer_move);
	}
}
