use std::collections::HashMap;

use crate::{
	eval::material::{static_exchange, value_of_piece},
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

				if current_entries.len() == 2 {
					current_entries.pop();
				}

				// Yeah, the insert means a shift, but since the size is
				// capped at two we're only copying one element ¯\_(ツ)_/¯
				current_entries.insert(0, mov);
			})
			.or_insert(vec![mov]);
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
			let victim = self.position.piece_on(mov.target).unwrap_or(Piece::Pawn); // En passant edge case
			let material_delta = static_exchange(attacker, victim);

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
			let attacks = attacks::get_attacks(
				self.position.get_occupancy_board() ^ mov.start ^ mov.target,
				mov.target,
				self.position.piece_on(mov.start).unwrap(),
				self.position.to_move(),
			);
			let is_check = (attacks
				& self.position.get_board_for_color(!self.position.to_move())
				& self.position.get_board_for_piece(Piece::King))
			.has_pieces();
			if is_check {
				score += value_of_piece(Piece::Queen);
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

#[cfg(test)]
mod test {

	use pretty_assertions::assert_eq;

	use super::*;

	#[test]
	pub fn killer_table_test() {
		let mut table = KillerTable::default();

		let position = Position::default();

		// Insert some killer
		table.insert(1, Move::from_uci_str("a2a4", &position));
		assert_eq!(
			table.get(1),
			Some(vec![Move::from_uci_str("a2a4", &position)])
		);

		// Ignores duplicates
		table.insert(1, Move::from_uci_str("a2a4", &position));
		assert_eq!(
			table.get(1),
			Some(vec![Move::from_uci_str("a2a4", &position)])
		);

		// Can hold a second one
		table.insert(1, Move::from_uci_str("b2b4", &position));
		assert_eq!(
			table.get(1),
			Some(vec![
				Move::from_uci_str("b2b4", &position),
				Move::from_uci_str("a2a4", &position)
			])
		);

		// Evicts the last killer when the table is full
		table.insert(1, Move::from_uci_str("c2c4", &position));
		assert_eq!(
			table.get(1),
			Some(vec![
				Move::from_uci_str("c2c4", &position),
				Move::from_uci_str("b2b4", &position)
			])
		);
	}

	#[test]
	pub fn move_iterator_test() {
		let position = Position::from_fen(
			"r3k3/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 1",
		)
		.unwrap();
		let moves = vec![
			Move::from_uci_str("e5f7", &position), // Bad capture
			Move::from_uci_str("e5g4", &position), // Quiet
			Move::from_uci_str("e1g1", &position), // Hash move
			Move::from_uci_str("d2e3", &position), // Killer 1
			Move::from_uci_str("e1c1", &position), // Killer 2
			Move::from_uci_str("c3a4", &position), // Quiet
			Move::from_uci_str("g2h3", &position), // Good capture
		];
		let killers = vec![moves[3], moves[4]];
		let hash_move = moves[2];

		let mut iterator =
			MoveIterator::new(moves.clone(), &position, Some(hash_move), Some(killers));

		assert_eq!(iterator.next(), Some(hash_move));
		// Captures, from good to bad
		assert_eq!(iterator.next(), Some(moves[6]));
		assert_eq!(iterator.next(), Some(moves[0]));

		// Quiet moves
		// Yes, we look at the hash move twice, but it's less
		// penalizing than adding a branch to check if each
		// move is the hash move so we could skip it
		assert_eq!(iterator.next(), Some(moves[3]));
		assert_eq!(iterator.next(), Some(moves[4]));
		assert_eq!(iterator.next(), Some(moves[1]));
		assert_eq!(iterator.next(), Some(moves[2]));
		assert_eq!(iterator.next(), Some(moves[5]));
	}
}
