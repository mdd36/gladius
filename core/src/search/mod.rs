pub mod move_ordering;
pub mod transposition;

use std::sync::{
	atomic::{AtomicBool, Ordering},
	Arc,
};

use crate::{
	eval::{
		evaluate_position, material::value_of_piece, repetition::PositionHistory, CHECKMATE_SCORE,
		MAX_SCORE, MIN_SCORE, STALEMATE_SCORE,
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
const MAX_CAPTURE_VALUE: i16 = value_of_piece(Piece::Queen);
const DELTA: i16 = 3 * value_of_piece(Piece::Pawn); // Starting with 300 centipawns

/// Because we're thinning our move search space using
/// [alpha-beta pruning](https://www.chessprogramming.org/Alpha-Beta),
/// the score determined in a search may be exact, and upper bound, or
/// a lower bound.
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

/// I wish there was a better way to handle the constant generics for
/// search, but since enums aren't supported and constant generics
/// can't be used as arguments to const functions, this is as good
/// as we can get.
pub const ROOT: usize = 0;
const _PV: usize = 1;
const NON_PV: usize = 2;

pub struct SearchParameters {
	pub depth: u8,
	pub ply: u8,
	pub extensions: u8,
	pub alpha: i16,
	pub beta: i16,
}

impl SearchParameters {
	pub fn new(max_depth: u8) -> Self {
		Self {
			depth: max_depth,
			ply: 0,
			extensions: 0,
			alpha: MIN_SCORE,
			beta: MAX_SCORE,
		}
	}

	pub fn new_with_alpha(max_depth: u8, alpha: i16) -> Self {
		Self {
			depth: max_depth,
			ply: 0,
			extensions: 0,
			alpha,
			beta: MAX_SCORE,
		}
	}

	pub fn next_ply(&self) -> Self {
		Self {
			depth: self.depth - 1,
			ply: self.ply + 1,
			extensions: self.extensions,
			alpha: -self.beta,
			beta: -self.alpha,
		}
	}

	pub fn add_extension(&mut self, extensions: u8) {
		self.extensions += extensions;
		self.depth += extensions;
	}

	pub fn quiescent(&self) -> Self {
		Self {
			depth: std::u8::MAX,
			ply: self.ply + 1,
			extensions: self.extensions,
			alpha: -self.beta,
			beta: -self.alpha,
		}
	}
}

#[derive(Debug)]
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

/// Once at the desired depth, run a search looking for a quiet position
/// before scoring. This helps abate the horizon effect, which could cause Gladius
/// to hang its pieces because we didn't see that they would be captured.
fn quiescence(
	position: &Position,
	mut alpha: i16,
	beta: i16,
	stop: Arc<AtomicBool>,
) -> SearchResult {
	let to_move = position.to_move();
	let is_in_check = position.is_color_in_check(to_move);
	let current_score = if is_in_check {
		alpha
	} else {
		evaluate_position(position)
	};

	alpha = std::cmp::max(current_score, alpha);

	if current_score >= beta {
		return SearchResult {
			score: beta,
			best_move: None,
			nodes_explored: 1,
		};
	}

	if current_score.saturating_add(MAX_CAPTURE_VALUE) < alpha {
		// Even if the opponent hangs their queen, this is still not the best
		// line for us
		return SearchResult {
			score: alpha,
			best_move: None,
			nodes_explored: 1,
		};
	}

	let moves = if is_in_check {
		// Need all the moves so we can try to evade this check
		generate_moves::<false>(position)
	} else {
		// Only capture
		generate_moves::<true>(position)
	};

	if moves.is_empty() {
		let score = if is_in_check {
			CHECKMATE_SCORE
		} else {
			current_score
		};
		return SearchResult {
			score,
			best_move: None,
			nodes_explored: 1,
		};
	}

	let mut nodes_explored = 1;

	let ordered_move_iterator = MoveIterator::new(moves, position, None, None);
	for m in ordered_move_iterator {
		if stop.load(Ordering::Relaxed) {
			return SearchResult {
				score: current_score,
				best_move: None,
				nodes_explored,
			};
		}

		// Another round of delta pruning, but don't prune any moves when we're in check!
		// We want to see *every* option to evade a checkmate
		if !is_in_check {
			let victim = position.piece_on(m.target).unwrap_or(Piece::Pawn); // En passant
			let maximum_advantage = value_of_piece(victim) + DELTA;
			if current_score + maximum_advantage < alpha {
				// If capturing the piece plus some margin doesn't show improvement
				// over alpha, it's time to abandon this line.
				continue;
			}
		}

		let search_result = quiescence(&position.apply_move(&m), -beta, -alpha, stop.clone());

		let score = -search_result.score;
		nodes_explored += search_result.nodes_explored;

		if score > alpha {
			alpha = score;

			if score >= beta {
				break;
			}
		}
	}

	SearchResult {
		score: alpha,
		best_move: None,
		nodes_explored,
	}
}

/// Start a PVS search for the best move from the current position.
/// If given enough time (order of tens of milliseconds), this will find
/// the best move to play in the current position along with the current
/// positional score and the number of positions explored.
pub fn search<const NODE_TYPE: usize>(
	position: &Position,
	position_history: &mut PositionHistory,
	transposition_table: &mut TranspositionTable,
	killers_table: &mut KillerTable,
	mut parameters: SearchParameters,
	stop: Arc<AtomicBool>,
) -> SearchResult {
	let saved_position = transposition_table.get(position.hash());
	if NODE_TYPE != ROOT {
		if parameters.depth == 0 {
			return quiescence(position, parameters.alpha, parameters.beta, stop);
		}

		let repetitions = position_history.repetitions(position);
		// Draws from board history
		if position.half_move_clock() >= 100 || repetitions == 3 {
			return SearchResult {
				score: STALEMATE_SCORE,
				best_move: None,
				nodes_explored: 1,
			};
		}

		if NODE_TYPE == NON_PV && repetitions < 2 {
			if let Some(entry) = saved_position {
				if entry.depth >= parameters.depth {
					match entry.score {
						Score::Exact(_) => return SearchResult::from(entry),
						Score::UpperBound(score) => {
							parameters.alpha = std::cmp::max(parameters.alpha, score)
						}
						Score::LowerBound(score) => {
							parameters.beta = std::cmp::min(parameters.beta, score)
						}
					}
				}
			};
		}

		if parameters.alpha >= parameters.beta {
			return SearchResult {
				score: parameters.alpha,
				best_move: None,
				nodes_explored: 1,
			};
		}
	}

	let possible_moves = generate_moves::<false>(position);

	if possible_moves.is_empty() {
		if position.is_color_in_check(position.to_move()) {
			return SearchResult {
				score: CHECKMATE_SCORE - parameters.ply as i16,
				best_move: None,
				nodes_explored: 1,
			};
		} else {
			return SearchResult {
				score: STALEMATE_SCORE,
				best_move: None,
				nodes_explored: 1,
			};
		}
	}

	let mut best_move = None;
	let mut best_score = Score::UpperBound(parameters.alpha);
	let mut nodes_explored = 1;
	let hash_move = saved_position.map(|entry| entry.best_move);
	let killers = killers_table.get(parameters.depth);
	let ordered_move_iterator = MoveIterator::new(possible_moves, position, hash_move, killers);

	for m in ordered_move_iterator {
		if stop.load(Ordering::Relaxed) {
			break;
		}

		let new_position = position.apply_move(&m);
		let mut next_ply_parameters = parameters.next_ply();
		position_history.add_position(&new_position);

		let extensions = extensions(parameters.extensions, &new_position, &m);
		next_ply_parameters.add_extension(extensions);

		let search_result = search::<NON_PV>(
			&new_position,
			position_history,
			transposition_table,
			killers_table,
			next_ply_parameters,
			stop.clone(),
		);

		let score = -search_result.score;
		nodes_explored += search_result.nodes_explored;

		position_history.pop();

		if score > parameters.alpha {
			// A new best move!
			parameters.alpha = score;
			best_score = Score::Exact(score);
			best_move = Some(m);

			if score >= parameters.beta {
				// This move is too good for us, so our opponent won't let us play it
				if !m.flags.is_capture() {
					killers_table.insert(parameters.ply, m);
				}

				best_score = Score::LowerBound(score);
				break;
			}
		}
	}

	if let Some(m) = best_move {
		let table_entry = TranspositionEntry::from(position, m, best_score, parameters.depth);
		transposition_table.insert(table_entry)
	}

	SearchResult {
		score: best_score.inner(),
		best_move,
		nodes_explored,
	}
}

/// If we find something interesting, we might want to prod a little deeper down this line
/// so we don't overlook an advantageous position. To prevent a runaway search, the number
/// of time a given line can be extended is capped by [`MAX_EXTENSION_DEPTH`].
fn extensions(num_extensions: u8, position: &Position, move_taken: &Move) -> u8 {
	let mut extension = 0;
	let moved_piece = position.piece_on(move_taken.target).unwrap();

	if moved_piece == Piece::Pawn
		&& (move_taken.target.rank() == 1 || move_taken.target.rank() == 6)
	{
		extension += 1;
	}

	if position.is_color_in_check(Color::White) || position.is_color_in_check(Color::Black) {
		extension += 1;
	}

	std::cmp::min(extension, MAX_EXTENSION_DEPTH - num_extensions)
}
