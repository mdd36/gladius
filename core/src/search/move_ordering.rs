use std::cmp::Ordering;

use crate::{
	eval::material::static_exchange,
	position::{moves::Move, Piece, Position},
};

/// Order moves so that we search the best moves first, increasing the efficacy
/// of alpha-beta pruning.
/// 
/// This ordering is stateful within a search since we want to preserve [killer
/// moves], which are sibling nodes that have different parents in the search
/// tree, and [hash moves], which are transposition table results from previous
/// searches.
/// 
/// [killer moves]: https://www.chessprogramming.org/Killer_Heuristic
/// [hash moves]: https://www.chessprogramming.org/Hash_Move
pub struct MoveOrderer {
	killer_moves: Vec<Vec<Move>>,
	hash_moves: Vec<Option<Move>>,
}

impl MoveOrderer {

	/// Create a new [`MoveOrderer`]. Because it's stateful, this should only happen
	/// once per search.
	pub fn new(predicted_ply: u8) -> Self {
		let killer_moves = vec![Vec::new(); predicted_ply as usize];
		let hash_moves = vec![None; predicted_ply as usize];

		Self {
			hash_moves,
			killer_moves,
		}
	}

	/// Order the moves for the current position, placing moves expected to be
	/// good first.
	pub fn order(&self, position: &Position, ply_from_root: u8, moves: &mut Vec<Move>) {
		let ply_from_root = ply_from_root as usize - 1; 
		moves.sort_unstable_by(|a, b| {
			match self.hash_moves.get(ply_from_root).map(Option::as_ref).flatten() {
				Some(m) if m.eq(a) => return Ordering::Less,
				Some(m) if m.eq(b) => return Ordering::Greater,
				_ => (), // Nop, hashed moves can't help us order these moves.
			};

			match self.killer_moves.get(ply_from_root) {
				Some(killers) if killers.contains(a) => return Ordering::Less,
				Some(killers) if killers.contains(b) => return Ordering::Greater,
				_ => (),
			};

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
				// eg explore PxQ before QxP.
				// TODO -- possibly we want to also consider recaptures so that we don't lose roughly equivalent
				// material on a capture, eg NxB is probably bad if it opens up RxN.
				return static_exchange(a_moved, a_taken).cmp(&static_exchange(b_moved, b_taken));
			}

			Ordering::Equal
		});
	}

	/// Add a new killer at the given ply. This should happen anytime we trigger a beta cutoff.
	pub fn add_killer(&mut self, ply_from_root: u8, killer_move: Move) {
		let ply_from_root = ply_from_root as usize; 

		if self.killer_moves.len() <= ply_from_root {
			self.killer_moves.resize(ply_from_root + 1, Vec::new());
		}

		self.killer_moves[ply_from_root].push(killer_move);
	}

	// Add a new hash move
	pub fn add_hash_move(&mut self, ply_from_root: u8, hash_move: Option<Move>) {
		let ply_from_root = ply_from_root as usize;
		
		if self.hash_moves.len() <= ply_from_root {
			self.hash_moves.resize(ply_from_root + 1, None);
		}

		self.hash_moves[ply_from_root] = hash_move;
	}
}
