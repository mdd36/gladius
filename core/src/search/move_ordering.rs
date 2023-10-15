use std::collections::HashMap;

use crate::{
	eval::material::value_of_piece,
	position::{attacks, board::Board, moves::Move, Piece, Position},
};

#[derive(Default)]
pub struct KillerTable(HashMap<u8, Vec<Move>>);

impl KillerTable {
	pub fn get(&self, ply: u8) -> Option<Vec<Move>> {
		self.0.get(&ply).map(|moves| moves.to_owned())
	}

	pub fn insert(&mut self, ply: u8, mov: Move) {
		self.0
			.entry(ply)
			.and_modify(|current_entries| {
				if current_entries.contains(&mov) {
					return;
				}
				current_entries[1] = current_entries[0];
				current_entries[0] = mov;
			})
			// Doubling up to let us assume that the array is always length 2
			// in the and_modify closure. Trying a single extra move isn't
			// penalizing enough to worry, especially since the killer will
			// hopefully trigger a beta cutoff anyway
			.or_insert(vec![mov, mov]);
	}
}

pub struct ScoredMove {
	pub mov: Move,
	pub score: i16,
}

enum MovePhase {
	Hash(Option<Move>),
	Capture(Vec<ScoredMove>),
	All(Vec<ScoredMove>),
}

pub struct MoveIterator<'a> {
	index: usize,
	all_moves: Vec<Move>,
	phase: MovePhase,
	position: &'a Position,
	killers: Option<Vec<Move>>,
	defense: Board,
}

impl<'a> MoveIterator<'a> {
	pub fn new(
		all_moves: Vec<Move>,
		position: &'a Position,
		hash_move: Option<Move>,
		killers: Option<Vec<Move>>,
	) -> Self {
		let hash_move = hash_move.filter(|hm| all_moves.contains(hm));
		Self {
			index: 0,
			all_moves,
			phase: MovePhase::Hash(hash_move),
			position,
			killers,
			defense: attacks::get_attacked_squares(position, !position.to_move()),
		}
	}

	fn score_all(&self, constraint: impl Fn(&Move) -> bool) -> Vec<ScoredMove> {
		self.all_moves
			.iter()
			.filter(|m| constraint(m))
			.map(|m| ScoredMove {
				mov: m.to_owned(),
				score: self.score(m),
			})
			.collect()
	}

	fn score(&self, mov: &Move) -> i16 {
		let mut score = 0;

		if mov.flags.is_capture() {
			let attacker = self.position.piece_on(mov.start).unwrap();
			let attacker_value = value_of_piece(attacker);
			let victim = self.position.piece_on(mov.target).unwrap_or(Piece::Pawn); // En passant edge case
			let victim_value = value_of_piece(victim);
			let material_delta = victim_value - attacker_value;

			score += material_delta;

			// I'm pretty sure this doesn't work cause the square is currently occupied by
			// a piece of the defender's color, and hence the generated attack map won't include
			// it (also, we might be still attacked by a sliding piece if we moved along its sliding
			// direction, and that's also not accounted for). I'd need to generate this map on the fly
			// each time and idk if that's wise.
			let is_defended = self.defense.is_occupied(mov.target);

			if is_defended && material_delta <= 0 {
				score -= value_of_piece(Piece::Rook);
			} else {
				score += value_of_piece(Piece::Queen);
			}
		} else {
			match &self.killers {
				Some(killers) if killers.contains(mov) => score += value_of_piece(Piece::Queen),
				_ => {}
			}
		}

		if mov.flags.is_promotion() {
			score += value_of_piece(Piece::Queen);
		}

		score
	}
}

fn selection_sort_single(moves: &mut Vec<ScoredMove>, index: usize) -> Option<Move> {
	if index >= moves.len() {
		return None;
	}

	let mut best_score = std::i16::MIN;
	let mut best_index = index;
	for i in index..moves.len() {
		let move_score = moves[i].score;
		if move_score > best_score {
			best_score = move_score;
			best_index = i;
		}
	}

	moves.swap(index, best_index);
	let to_play = moves[index].mov;

	Some(to_play)
}

impl<'a> Iterator for MoveIterator<'a> {
	type Item = Move;

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.phase {
			MovePhase::Hash(Some(hash_move)) => {
				let to_play = hash_move.to_owned();
				self.phase = MovePhase::Capture(self.score_all(|m| m.flags.is_capture()));
				Some(to_play)
			}
			MovePhase::Hash(None) => {
				self.phase = MovePhase::Capture(self.score_all(|m| m.flags.is_capture()));
				self.next()
			}
			MovePhase::Capture(capture_moves) => {
				if let Some(to_play) = selection_sort_single(capture_moves, self.index) {
					self.index += 1;
					Some(to_play)
				} else {
					self.index = 0;
					self.phase = MovePhase::All(self.score_all(|m| !m.flags.is_capture()));
					self.next()
				}
			}
			MovePhase::All(moves) => {
				let to_play = selection_sort_single(moves, self.index);
				self.index += 1;
				to_play
			}
		}
	}
}
