use crate::position::{Color, Piece, Position};

const MATERIAL_VALUES: [i16; 8] = [
	0,     // First two are dummies to allow indexing
	0,     // by the Piece enum discriminate
	100,   // Pawn
	500,   // Rook
	320,   // Knight
	330,   // Bishop
	900,   // Queen
	20000, // King
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
