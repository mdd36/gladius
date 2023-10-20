use crate::position::{Color, Piece, Position};

const MATERIAL_VALUES: [i16; 8] = [
	0,   // First two are dummies to allow indexing
	0,   // by the Piece enum discriminate
	100, // Pawn
	500, // Rook
	320, // Knight
	330, // Bishop
	900, // Queen
	0,   // King.
];

pub const HALF_PAWN: i16 = MATERIAL_VALUES[Piece::Pawn as usize] / 2;

pub fn material_score(position: &Position, color: Color) -> i16 {
	let color_board = position.get_board_for_color(color);
	let num_pawns = (position.get_board_for_piece(Piece::Pawn) & color_board).count_ones() as i16;
	let num_rooks = (position.get_board_for_piece(Piece::Rook) & color_board).count_ones() as i16;
	let num_knights =
		(position.get_board_for_piece(Piece::Knight) & color_board).count_ones() as i16;
	let num_bishops =
		(position.get_board_for_piece(Piece::Bishop) & color_board).count_ones() as i16;
	let num_queens = (position.get_board_for_piece(Piece::Queen) & color_board).count_ones() as i16;
	let num_kings = (position.get_board_for_piece(Piece::King) & color_board).count_ones() as i16;

	// Bonus for both bishops to avoid a color weakness
	let bishop_pair_bonus = if num_bishops > 1 { HALF_PAWN } else { 0 };

	MATERIAL_VALUES[Piece::Pawn as usize] * num_pawns
		+ MATERIAL_VALUES[Piece::Rook as usize] * num_rooks
		+ MATERIAL_VALUES[Piece::Knight as usize] * num_knights
		+ MATERIAL_VALUES[Piece::Bishop as usize] * num_bishops
		+ MATERIAL_VALUES[Piece::Queen as usize] * num_queens
		+ MATERIAL_VALUES[Piece::King as usize] * num_kings
		+ bishop_pair_bonus
}

/// Check if a position has enough material for at least one side to force a checkmate.
/// This implements the USCF rules since they're more commonly used outside of tournament
/// play. See [this][insufficient material definition] for more a breakdown of what's
/// considered insufficient material.
///
/// [insufficient material definition]: https://support.chess.com/article/128-what-does-insufficient-mating-material-mean
pub fn insufficient_material(position: &Position) -> bool {
	let pieces_left = position.get_occupancy_board().count_ones();
	if pieces_left > 4 {
		return false;
	} else if pieces_left == 2 {
		return true;
	}

	let knights = position.get_board_for_piece(Piece::Knight);
	let bishops = position.get_board_for_piece(Piece::Bishop);
	let knight_or_bishop = knights | bishops;
	let white = position.get_board_for_color(Color::White);

	return knights.count_ones() == 2
		|| pieces_left == 3 && knight_or_bishop.has_pieces()
		|| (knight_or_bishop).count_ones() == 2 && (knight_or_bishop & white).count_ones() == 1;
}

pub const fn value_of_piece(piece: Piece) -> i16 {
	MATERIAL_VALUES[piece as usize]
}

pub fn static_exchange(attacker: Piece, victim: Piece) -> i16 {
	value_of_piece(victim) - value_of_piece(attacker)
}

#[cfg(test)]
mod test {

	use super::*;

	mod insufficient_material {

		use super::*;

		fn test_position(fen: &str, expected: bool) {
			let position = Position::from_fen(fen).unwrap();
			assert_eq!(expected, insufficient_material(&position));
		}

		/// ---- FALSE TESTS: We have sufficient material to force a checkmate ----
		#[test]
		fn piece_count_check() {
			test_position("8/2bkn3/8/8/8/8/2BKN3/8 w - - 0 1", false);
		}

		#[test]
		fn low_count_with_other_pieces() {
			test_position("8/3kr3/8/8/8/8/2QK4/8 w - - 0 1", false);
			test_position("8/3k4/2q5/8/8/8/4N3/3K4 w - - 0 1", false);
			test_position("8/3k4/2q5/8/8/1B6/8/3K4 w - - 0 1", false);
		}

		#[test]
		fn two_bishops_same_side() {
			test_position("8/3k4/8/8/1B6/3B4/8/3K4 b - - 0 1", false);
		}

		/// TRUE TESTS: ---- We have insufficient material ----
		#[test]
		fn two_knights() {
			test_position("8/3k4/8/8/8/8/2N1N3/3K4 b - - 0 1", true);
			test_position("8/3kn3/8/8/8/8/2N5/3K4 b - - 0 1", true);
		}

		#[test]
		fn one_bishop_each() {
			test_position("8/3k4/4b3/8/8/3B4/8/3K4 b - - 0 1", true);
		}

		#[test]
		fn one_bishop_one_knight() {
			test_position("8/3k4/4n3/8/8/3B4/8/3K4 w - - 0 1", true);
		}

		#[test]
		fn three_pieces_left() {
			test_position("8/3k4/8/8/8/3B4/8/3K4 w - - 0 1", true);
			test_position("8/3k4/4n3/8/8/8/8/3K4 w - - 0 1", true);
		}

		#[test]
		fn kings_only() {
			test_position("8/3k4/8/8/8/8/8/3K4 w - - 0 1", true)
		}
	}
}
