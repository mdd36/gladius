use crate::position::{moves::Move, Position};

/// Because we're thinning our move search space using [alpha-beta pruning],
/// the score determined in a search may be exact, and upper bound, or a

/// [alpha-beta pruning]: https://www.chessprogramming.org/Alpha-Beta
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

pub fn search(position: &Position, alpha: i16, beta: i16) -> Move {
	todo!()
}
