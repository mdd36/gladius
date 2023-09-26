pub mod transposition;

use std::sync::{
	atomic::{AtomicBool, Ordering},
	Arc,
};

use crate::{
	eval::evaluate_position,
	position::{
		moves::{generate_moves, Move},
		Color, Piece, Position,
	},
};

use self::transposition::{TranspositionEntry, TranspositionTable};

const MAX_EXTENSION_DEPTH: u8 = 16;
const DELTA: i16 = 200; // Starting with 200 centipawns

/// Because we're thinning our move search space using [alpha-beta pruning],
/// the score determined in a search may be exact, and upper bound, or a

/// [alpha-beta pruning]: https://www.chessprogramming.org/Alpha-Beta
#[derive(Copy, Clone)]
pub enum Score {
	/// 've explored all options and determined an exact score.
	/// The first node we explore will yield and exact score since there's no
	/// other values to compare its score against for pruning.
	Exact(i16),

	/// A Cut Node where we short circuit because a move is too good for us. We
	/// assume that our opponent isn't dumb, and wouldn't let us get here if they
	/// had a chance to avoid it earlier in the search tree.
	LowerBound(i16),

	/// An All Node where we short circuited due to too low of a score. This is
	/// where we've found an enemy response to one of our moves that's already
	/// better than any response to a different move, so it no longer makes sense
	/// to explore this line since we should play the other move instead.
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
			alpha: std::i16::MIN,
			beta: std::i16::MAX,
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
			ply: self.ply_from_root / 2, // Half as deep as our original search
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
}

impl From<TranspositionEntry> for SearchResult {
	fn from(value: TranspositionEntry) -> Self {
		Self {
			score: value.score.inner(),
			best_move: Some(value.best_move),
		}
	}
}

pub fn search<const QUIESCENT: bool>(
	position: &Position,
	transposition_table: &mut TranspositionTable,
	mut parameters: SearchParameters,
	stop: Arc<AtomicBool>,
) -> SearchResult {
	let mut best_score = Score::UpperBound(parameters.alpha);
	let mut best_move = None;

	if QUIESCENT && parameters.ply == 0 {
		return SearchResult {
			score: parameters.alpha,
			best_move,
		};
	}

	if parameters.ply == 0 {
		return search::<true>(
			position,
			transposition_table,
			parameters.quiescent(),
			stop.clone(),
		);
	}

	if QUIESCENT {
		let current_score = evaluate_position(position);
		if current_score >= parameters.beta {
			return SearchResult {
				score: parameters.beta,
				best_move,
			};
		}

		// This move might gain us something, but not enough to justify
		// looking further down its line. We could also be losing down
		// this line, which definitely means we should abandon it!
		// TODO: Turn this off when we get to the endgame
		if current_score < parameters.alpha + DELTA {
			return SearchResult {
				score: parameters.alpha,
				best_move,
			};
		}

		if current_score > parameters.alpha {
			parameters.alpha = current_score
		}
	}

	if let Some(entry) = transposition_table.get(position.zobrist_hash) {
		if entry.key == position.zobrist_hash && entry.depth >= parameters.ply {
			return SearchResult::from(entry);
		}
	};

	let mut possible_moves = generate_moves::<QUIESCENT>(position);

	if QUIESCENT && possible_moves.is_empty() {
		return SearchResult {
			score: parameters.alpha,
			best_move,
		};
	}

	if possible_moves.is_empty() {
		if position.is_in_check(position.metadata.to_move()) {
			return SearchResult {
				score: std::i16::MIN + parameters.ply_from_root as i16,
				best_move,
			};
		} else {
			return SearchResult {
				score: 0,
				best_move,
			};
		}
	}

	possible_moves.sort_by(|a, b| {
		// TODO implement move ordering
		return std::cmp::Ordering::Equal;
	});

	for m in possible_moves {
		if stop.load(Ordering::Relaxed) {
			return SearchResult {
				score: best_score.inner(),
				best_move,
			};
		}

		let new_position = position.apply_move(&m);
		let mut next_ply_parameters = parameters.next_ply();

		if !QUIESCENT {
			let extensions = extensions(parameters.extensions, &new_position, &m);
			next_ply_parameters.add_extension(extensions);
		}

		let score = -search::<QUIESCENT>(
			&new_position,
			transposition_table,
			next_ply_parameters,
			stop.clone(),
		)
		.score;

		if score >= parameters.beta {
			// This move is too good for us, so our opponent won't let us play it
			let table_entry =
				TranspositionEntry::from(position, m, Score::LowerBound(score), parameters.ply);
			transposition_table.insert(table_entry);
			return SearchResult {
				score,
				best_move: Some(m),
			};
		}

		if score > parameters.alpha {
			// A new best move!
			parameters.alpha = score;
			best_score = Score::Exact(score);
			best_move = Some(m);
		}
	}
	if let Some(m) = best_move {
		let table_entry = TranspositionEntry::from(position, m, best_score, parameters.ply);
		transposition_table.insert(table_entry);
	}

	SearchResult {
		score: best_score.inner(),
		best_move,
	}
}

fn extensions(num_extensions: u8, position: &Position, move_taken: &Move) -> u8 {
	let mut extension = 0;
	let moved_piece = position.piece_on(move_taken.target).unwrap(); // Position is post move here

	if moved_piece == Piece::Pawn
		&& (move_taken.target.rank() == 1 || move_taken.target.rank() == 6)
	{
		// Near promotion
		extension += 1;
	}

	if position.is_in_check(Color::White) || position.is_in_check(Color::Black) {
		extension += 1;
	}

	if move_taken.flags.is_capture() {
		extension += 1;
	}

	std::cmp::min(extension, MAX_EXTENSION_DEPTH - num_extensions) // Let's not get **too** wild
}
