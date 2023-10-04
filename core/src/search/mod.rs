pub mod move_ordering;
pub mod transposition;

use std::sync::{
	atomic::{AtomicBool, Ordering},
	Arc,
};

use crate::{
	eval::{
		evaluate_position, repetition::is_threefold_repetition, CHECKMATE_SCORE, MAX_SCORE,
		MIN_SCORE,
	},
	position::{
		moves::{generate_moves, Move},
		Color, Piece, Position,
	},
};

use self::{
	move_ordering::{KillerTable, MoveIterator},
	transposition::{TranspositionEntry, TranspositionTable},
};

const MAX_EXTENSION_DEPTH: u8 = 4;
const DELTA: i16 = 200; // Starting with 200 centipawns

/// Because we're thinning our move search space using [alpha-beta pruning],
/// the score determined in a search may be exact, and upper bound, or a

/// [alpha-beta pruning]: https://www.chessprogramming.org/Alpha-Beta
#[derive(Copy, Clone)]
pub enum Score {
	/// We've explored all options and determined an exact score.
	/// The first node we explore will yield and exact score since there's no
	/// other values to compare its score against for pruning.
	Exact(i16),

	/// A Cut Node where we short circuit because a move is too good for us. We
	/// assume that our opponent isn't dumb, and wouldn't let us get here if they
	/// had a chance to avoid it earlier in the search tree (beta cutoff).
	LowerBound(i16),

	/// An All Node occurs when we lack improvement over alpha, so it no
	/// longer makes sense to explore this line since we should play the
	/// the move currently associated with alpha.
	UpperBound(i16),
}

impl Score {
	pub fn inner(&self) -> i16 {
		match self {
			Self::Exact(eval) => eval.to_owned(),
			Self::LowerBound(eval) => eval.to_owned(),
			Self::UpperBound(eval) => eval.to_owned(),
		}
	}
}

pub struct SearchParameters {
	pub ply: u8,
	pub ply_from_root: u8,
	pub extensions: u8,
	pub alpha: i16,
	pub beta: i16,
	pub quiescent: bool,
}

impl SearchParameters {
	pub fn new(max_depth: u8) -> Self {
		Self {
			ply: max_depth,
			ply_from_root: 0,
			extensions: 0,
			alpha: MIN_SCORE,
			beta: MAX_SCORE,
			quiescent: false,
		}
	}

	pub fn next_ply(&self) -> Self {
		Self {
			ply: self.ply - 1,
			ply_from_root: self.ply_from_root + 1,
			extensions: self.extensions,
			alpha: -self.beta,
			beta: -self.alpha,
			quiescent: self.quiescent,
		}
	}

	pub fn add_extension(&mut self, extensions: u8) {
		self.extensions += extensions;
		self.ply += extensions;
	}

	pub fn quiescent(&self) -> Self {
		Self {
			ply: std::u8::MAX,
			ply_from_root: self.ply_from_root + 1,
			extensions: self.extensions,
			alpha: -self.beta,
			beta: -self.alpha,
			quiescent: self.quiescent,
		}
	}
}

pub struct SearchResult {
	pub score: i16,
	pub best_move: Option<Move>,
	pub nodes_explored: u64,
}

impl From<TranspositionEntry> for SearchResult {
	fn from(value: TranspositionEntry) -> Self {
		Self {
			score: value.score.inner(),
			best_move: Some(value.best_move),
			nodes_explored: 1,
		}
	}
}

pub fn search<const QUIESCENT: bool>(
	position: &Position,
	position_history: &mut Vec<u64>,
	transposition_table: &mut TranspositionTable,
	killers_table: &mut KillerTable,
	mut parameters: SearchParameters,
	stop: Arc<AtomicBool>,
) -> SearchResult {
	let mut best_score = Score::UpperBound(parameters.alpha);
	let saved_position = transposition_table.get(position.zobrist_hash);
	let mut best_move = match saved_position {
		Some(entry) => Some(entry.best_move),
		_ => None,
	};

	if is_threefold_repetition(
		position.half_move_clock(),
		parameters.ply_from_root,
		position_history,
		position.zobrist_hash,
	) {
		return SearchResult {
			score: 0,
			best_move: best_move,
			nodes_explored: 1,
		};
	}

	if QUIESCENT {
		let current_score = evaluate_position(position);
		if current_score >= parameters.beta {
			return SearchResult {
				score: parameters.beta,
				best_move,
				nodes_explored: 1,
			};
		}

		// This move might gain us something, but not enough to justify
		// looking further down this line. We could also be losing down
		// this line, which definitely means we should abandon it!
		// TODO: Turn this off when we get to the endgame
		if current_score < parameters.alpha.saturating_sub(DELTA) {
			return SearchResult {
				score: parameters.alpha,
				best_move,
				nodes_explored: 1,
			};
		}

		if current_score > parameters.alpha {
			parameters.alpha = current_score
		}
	}

	if QUIESCENT && parameters.ply == 0 {
		return SearchResult {
			score: parameters.alpha,
			best_move,
			nodes_explored: 1,
		};
	}

	if parameters.ply == 0 {
		return search::<true>(
			position,
			position_history,
			transposition_table,
			killers_table,
			parameters.quiescent(),
			stop.clone(),
		);
	}

	if let Some(entry) = saved_position {
		if entry.depth >= parameters.ply {
			return SearchResult::from(entry);
		}
	};

	let possible_moves = generate_moves::<QUIESCENT>(position);

	if QUIESCENT && possible_moves.is_empty() {
		return SearchResult {
			score: parameters.alpha,
			best_move,
			nodes_explored: 1,
		};
	}

	if possible_moves.is_empty() {
		if position.is_in_check(position.metadata.to_move()) {
			return SearchResult {
				score: CHECKMATE_SCORE + parameters.ply_from_root as i16,
				best_move,
				nodes_explored: 1,
			};
		} else {
			return SearchResult {
				score: 0,
				best_move,
				nodes_explored: 1,
			};
		}
	}

	let mut nodes_explored = 0;
	for m in MoveIterator::new(
		possible_moves,
		position,
		best_move,
		killers_table.get(parameters.ply),
	) {
		if stop.load(Ordering::Relaxed) {
			return SearchResult {
				score: best_score.inner(),
				best_move,
				nodes_explored,
			};
		}

		let new_position = position.apply_move(&m);
		let mut next_ply_parameters = parameters.next_ply();
		position_history.push(new_position.zobrist_hash);

		if !QUIESCENT {
			let extensions = extensions(parameters.extensions, &new_position, &m);
			next_ply_parameters.add_extension(extensions);
		}

		let search_result = search::<QUIESCENT>(
			&new_position,
			position_history,
			transposition_table,
			killers_table,
			next_ply_parameters,
			stop.clone(),
		);

		let score = -search_result.score;

		position_history.pop();

		if score >= parameters.beta {
			// This move is too good for us, so our opponent won't let us play it
			let table_entry =
				TranspositionEntry::from(position, m, Score::LowerBound(score), parameters.ply);
			transposition_table.insert(table_entry);

			if !m.flags.is_capture() {
				killers_table.insert(parameters.ply_from_root, m);
			}

			return SearchResult {
				score,
				best_move: Some(m),
				nodes_explored: nodes_explored + 1,
			};
		}

		if score > parameters.alpha {
			// A new best move!
			parameters.alpha = score;
			best_score = Score::Exact(score);
			best_move = Some(m);
		}

		nodes_explored += search_result.nodes_explored;
	}

	if !QUIESCENT {
		if let Some(m) = best_move {
			let table_entry = TranspositionEntry::from(position, m, best_score, parameters.ply);
			transposition_table.insert(table_entry);
		}
	}

	SearchResult {
		score: best_score.inner(),
		best_move,
		nodes_explored,
	}
}

fn extensions(num_extensions: u8, position: &Position, move_taken: &Move) -> u8 {
	let mut extension = 0;
	let moved_piece = position.piece_on(move_taken.target).unwrap(); // Position is post move here

	if moved_piece == Piece::Pawn
		&& (move_taken.target.rank() == 1 || move_taken.target.rank() == 6)
	{
		extension += 1;
	}

	if position.is_in_check(Color::White) || position.is_in_check(Color::Black) {
		extension += 1;
	}

	std::cmp::min(extension, MAX_EXTENSION_DEPTH - num_extensions) // Let's not get **too** wild
}
