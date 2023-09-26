#[rustfmt::skip]
pub mod magics;
pub mod attacks;
pub mod board;
pub mod hash;
pub mod moves;

use std::ops::BitOrAssign;

use board::{Board, Square, CASTLE_RIGHTS_SQUARES, ROOKS};
use moves::MOVE_DIRECTION;

use self::{
	attacks::attackers_of_square,
	board::ROOK_CASTLE_MOVE,
	hash::{hash, hash_after_move},
};

#[derive(Copy, Clone)]
pub enum CastleSide {
	Queen,
	King,
}

impl From<Square> for CastleSide {
	fn from(value: Square) -> Self {
		let king = CastleSide::King as usize;
		let queen = CastleSide::Queen as usize;
		let white = Color::White as usize;
		let black = Color::Black as usize;

		if value == ROOKS[king][white] || value == ROOKS[king][black] {
			return CastleSide::King;
		}

		if value == ROOKS[queen][white] || value == ROOKS[queen][black] {
			return CastleSide::Queen;
		}

		panic!("Not a valid castling square: {value:?}")
	}
}

#[derive(Copy, Clone)]
pub enum Color {
	White,
	Black,
}

impl std::ops::Not for Color {
	type Output = Color;

	fn not(self) -> Self::Output {
		match self {
			Self::White => Self::Black,
			Self::Black => Self::White,
		}
	}
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Piece {
	Pawn = 2,
	Rook,
	Knight,
	Bishop,
	Queen,
	King,
}

impl Piece {
	pub fn iter() -> impl Iterator<Item = Piece> {
		[
			Piece::Pawn,
			Piece::Rook,
			Piece::Knight,
			Piece::Bishop,
			Piece::Queen,
			Piece::King,
		]
		.iter()
		.copied()
	}

	pub fn non_pawn() -> impl Iterator<Item = Piece> {
		[
			Piece::Rook,
			Piece::Knight,
			Piece::Bishop,
			Piece::Queen,
			Piece::King,
		]
		.iter()
		.copied()
	}
}

/// 16 bits packed to represent the metadata for a given position.
/// Packing looks like this, starting with the most significant bits:
///
/// Unused extra bit (1 bit)
///   Not currently used, simply fills out the u16.
/// En Passant square (4 bits)
///   Where an en passant capture can occur. The first bit represents
///   if en passant is possible, and the last three bits determine the file
///   for the en passant target, starting from the a file and increasing
///   to the h file. The rank can be determined based on the the to move flag.
///   0 if white, 1 if black
/// To Move (1 bit)
///   Will be 0 for white's move, 1 for black's
/// Castling rights (4 bits)
///   Tracks the castling rights for each side. Each bit represents
///   one possible castle move, in order: blacks's king side, blacks's
///   queen side, whites's king side, whites's queen side.
/// Half move clock (6 bits)
///   The number of moves since the last pawn move or capture. Used to
///   enforce a stalemate once it reaches 50.
///
/// For example, if en passant is possible on the c3 square, it's black's
/// move, black can castle both king and queen's side, white can castle
/// only king's side, and there's 5 moves on the half move clock, then
/// the bit value would look like this:
///
/// 0 1010 1 1011 000101
///
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct PositionMetadata(u16);

const EN_PASSANT_MASK: u16 = 0b0111100000000000;
const EN_PASSANT_SET_MASK: u16 = 0b0100000000000000;
const TO_MOVE_MASK: u16 = 0b0000010000000000;
const CASTLING_MASK: u16 = 0b0000001111000000;
const HALF_CLOCK_MASK: u16 = 0b0000000000111111;

impl BitOrAssign<u16> for PositionMetadata {
	fn bitor_assign(&mut self, rhs: u16) {
		*self = Self(self.0 | rhs)
	}
}

impl Default for PositionMetadata {
	/// Normal starting metadata for chess
	///
	/// - No en passant square
	/// - White to move
	/// - Both sides have all their castling rights
	/// - 0 moves on the half move block
	fn default() -> Self {
		Self(0b0_0000_0_1111_000000)
	}
}

impl std::fmt::Debug for PositionMetadata {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let half_clock = self.0 & HALF_CLOCK_MASK;
		let castling = (self.0 & CASTLING_MASK) >> 6;
		let to_move = if self.0 & TO_MOVE_MASK == 0 {
			"white"
		} else {
			"black"
		};
		let en_passant_bits = (self.0 & EN_PASSANT_MASK) >> 11;
		let en_passant_possible = en_passant_bits & 0x0008 != 0;
		let en_passant_file = (en_passant_bits & 0x0007) as u8 + ('a' as u8);
		let en_passant_rank = if self.0 & TO_MOVE_MASK == 0 { "6" } else { "3" };
		f.debug_struct("PositionMetadata")
			.field("Half Clock", &half_clock)
			.field("Castling", &format!("{castling:b}"))
			.field("To Move", &to_move)
			.field("En Passant Possible", &en_passant_possible)
			.field(
				"En Passant Square",
				&format!("{}{en_passant_rank}", en_passant_file as char),
			)
			.finish()
	}
}

impl PositionMetadata {
	pub fn blank() -> Self {
		Self(0)
	}

	pub fn can_castle(&self, color: Color, side: CastleSide) -> bool {
		let castle_mask = 0b01 << (side as u16) << (2 * color as u16) << 6;
		self.0 & castle_mask != 0
	}

	pub fn to_move(&self) -> Color {
		if self.0 & TO_MOVE_MASK != 0 {
			Color::Black
		} else {
			Color::White
		}
	}

	/// Raises the half move counter by 1
	pub fn increment_half_move(&mut self) {
		*self = Self(self.0 + 1)
	}

	pub fn reset_move_clock(&mut self) {
		*self = Self(self.0 & !HALF_CLOCK_MASK);
	}

	pub fn toggle_to_move(&mut self) {
		*self = Self(self.0 ^ TO_MOVE_MASK);
	}

	pub fn revoke_castling_rights_for_side(&mut self, color: Color, side: CastleSide) {
		let castle_mask = 0b01 << (side as u16) << (2 * color as u16) << 6;
		*self = Self(self.0 & !castle_mask);
	}

	pub fn revoke_castling_rights_for_color(&mut self, color: Color) {
		let castle_mask = 0b11 << (2 * color as u16) << 6;
		*self = Self(self.0 & !castle_mask);
	}

	pub fn en_passant_square(&self) -> Option<Square> {
		let en_passant_bits = self.0 >> 11;

		if en_passant_bits & 0x8 == 0 {
			return None;
		}

		let file = (en_passant_bits & 0x7) as u8;
		let rank = match self.to_move() {
			Color::Black => 2,
			Color::White => 5,
		};

		Some(Square::from_rank_and_file(rank, file))
	}

	pub fn clear_en_passant(&mut self) {
		*self = Self(self.0 & !EN_PASSANT_MASK);
	}

	pub fn set_en_passant_square(&mut self, square: Square) {
		let file = square.file() as u16;
		let shifted_file = file << 11;

		*self = Self(self.0 & !EN_PASSANT_MASK | EN_PASSANT_SET_MASK | shifted_file)
	}
}

#[derive(Clone, PartialEq, Eq)]
pub struct Position {
	pub boards: [Board; 8],
	pub metadata: PositionMetadata,
	pub zobrist_hash: u64,
}

impl Default for Position {
	/// Initializes the position to the default starting position for
	/// chess games.
	fn default() -> Self {
		let boards = [
			Board::from(0x000000000000ffff), // White
			Board::from(0xffff000000000000), // Black
			Board::from(0x00ff00000000ff00), // Pawns
			Board::from(0x8100000000000081), // Rooks
			Board::from(0x4200000000000042), // Knights
			Board::from(0x2400000000000024), // Bishops
			Board::from(0x0800000000000008), // Queens
			Board::from(0x1000000000000010), // Kings
		];
		let metadata = PositionMetadata::default();
		let zobrist_hash = hash(&boards, &metadata);
		Self {
			boards,
			metadata,
			zobrist_hash,
		}
	}
}

impl std::fmt::Debug for Position {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"Board:\n{}\nMetadata: {:?}",
			self.as_display_string(),
			self.metadata
		)
	}
}

impl Position {
	pub fn blank() -> Self {
		let boards = Default::default();
		let metadata = PositionMetadata::blank();
		let zobrist_hash = hash(&boards, &metadata);
		Self {
			boards,
			metadata,
			zobrist_hash,
		}
	}

	/// Initialize the board from a given [FEN] string. A standard chess
	/// start is needed, prefer [`Position::default()`] which uses hard
	/// coded values and hence is faster than parsing the string.
	///
	/// Will fail if the provided string isn't valid FEN.
	///
	/// [FEN]: https://en.wikipedia.org/wiki/Forsyth–Edwards_Notation
	///
	/// ### Example
	/// ```
	/// use gladius_core::position::Position;
	///
	/// assert_eq!(
	///     Position::default(),
	///     Position::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
	///         .unwrap()
	/// );
	/// ```
	pub fn from_fen(fen: &str) -> Result<Self, ()> {
		let mut position = Self::blank();
		let mut fen_components = fen.split(" ");

		// Parse the location of all the pieces
		let mut rank = 7u8;
		let mut file = 0u8;
		for char in fen_components.next().ok_or(())?.chars() {
			if char == '/' {
				rank -= 1;
				file = 0;
				continue;
			}

			if char.is_numeric() {
				file += (char as u8) - ('0' as u8);
				continue;
			}

			let square = Square::from_rank_and_file(rank, file);

			if char.is_uppercase() {
				position.boards[Color::White as usize] |= square;
			} else {
				position.boards[Color::Black as usize] |= square;
			}

			let piece = match char.to_ascii_lowercase() {
				'r' => Piece::Rook,
				'n' => Piece::Knight,
				'b' => Piece::Bishop,
				'q' => Piece::Queen,
				'k' => Piece::King,
				'p' => Piece::Pawn,
				_ => return Err(()),
			};

			position.boards[piece as usize] |= square;

			file += 1;
		}

		// Who's move is it?
		match fen_components.next().ok_or(())? {
			"b" => position.metadata |= TO_MOVE_MASK,
			"w" => {}
			_ => return Err(()),
		};

		// Castling rights
		for castle_char in fen_components.next().ok_or(())?.chars() {
			match castle_char {
				'k' => position.metadata |= 0x0200,
				'q' => position.metadata |= 0x0100,
				'K' => position.metadata |= 0x0080,
				'Q' => position.metadata |= 0x0040,
				_ => {}
			}
		}

		// En passant
		match fen_components.next().ok_or(())? {
			"-" => {}
			square => {
				position.metadata |= 1 << 14;
				let file = square.as_bytes()[0] - ('a' as u8);
				position.metadata |= (file as u16) << 11;
			}
		}

		// Half move clock
		let moves: u16 = fen_components
			.next()
			.map(|clock| clock.parse().ok())
			.flatten()
			.unwrap_or(0);
		position.metadata.0 += moves;

		Ok(position)
	}

	pub fn get_board_for_color(&self, color: Color) -> Board {
		self.boards[color as usize]
	}

	pub fn get_board_for_piece(&self, piece: Piece) -> Board {
		self.boards[piece as usize]
	}

	pub fn get_occupancy_board(&self) -> Board {
		self.get_board_for_color(Color::White) | self.get_board_for_color(Color::Black)
	}

	/// Check if a given color can castle on a side. This check only
	/// determines if the player has rights to castle on a given side,
	/// not that all conditions are met. Those are deferred to the
	/// [`gladius_core::position::moves::is_legal`] check.
	pub fn can_castle(&self, color: Color, side: CastleSide) -> bool {
		self.metadata.can_castle(color, side)
	}

	/// Check what piece is on a square, if any.
	pub fn piece_on(&self, square: Square) -> Option<Piece> {
		if self.boards[Piece::Rook as usize].is_occupied(square) {
			Some(Piece::Rook)
		} else if self.boards[Piece::Knight as usize].is_occupied(square) {
			Some(Piece::Knight)
		} else if self.boards[Piece::Bishop as usize].is_occupied(square) {
			Some(Piece::Bishop)
		} else if self.boards[Piece::Queen as usize].is_occupied(square) {
			Some(Piece::Queen)
		} else if self.boards[Piece::King as usize].is_occupied(square) {
			Some(Piece::King)
		} else if self.boards[Piece::Pawn as usize].is_occupied(square) {
			Some(Piece::Pawn)
		} else {
			None
		}
	}

	pub fn color_on(&self, square: Square) -> Option<Color> {
		if self.get_board_for_color(Color::White).is_occupied(square) {
			Some(Color::White)
		} else if self.get_board_for_color(Color::Black).is_occupied(square) {
			Some(Color::Black)
		} else {
			None
		}
	}

	pub fn full_move_clock(&self) -> u8 {
		todo!()
	}

	pub fn is_in_check(&self, color: Color) -> bool {
		let king_square =
			Square::from(self.get_board_for_color(color) & self.get_board_for_piece(Piece::King));
		attackers_of_square(king_square, !color, self).has_pieces()
	}

	pub fn apply_move(&self, next_move: &moves::Move) -> Position {
		let color_to_move = self.metadata.to_move() as usize;
		let moved_piece = self.piece_on(next_move.start).unwrap();
		let flags = next_move.flags;

		let mut boards = self.boards.clone();

		// TODO profile if this branching is a big penalty
		if let Some(captured_piece) = self.piece_on(next_move.target) {
			boards[1 - color_to_move] ^= next_move.target;
			boards[captured_piece as usize] ^= next_move.target;
		} else if let Some(side) = flags.castling_side() {
			// Move the Rook also
			let rook_move = ROOK_CASTLE_MOVE[side as usize][color_to_move];
			boards[Piece::Rook as usize] ^= rook_move;
			boards[color_to_move] ^= rook_move;
		} else if flags.is_en_passant() {
			let captured_piece_square = next_move.target >> (8 * MOVE_DIRECTION[color_to_move]);
			boards[Piece::Pawn as usize] ^= captured_piece_square;
			boards[1 - color_to_move] ^= captured_piece_square;
		}

		boards[color_to_move] ^= next_move.start | next_move.target;
		boards[moved_piece as usize] ^= next_move.start | next_move.target;

		if let Some(piece) = flags.promotion_piece() {
			boards[Piece::Pawn as usize] ^= next_move.target;
			boards[piece as usize] ^= next_move.target;
		}

		// This will copy by value since PositionMetadata implements Copy
		let mut metadata = self.metadata;

		// Set en passant square
		if flags.is_double_pawn_push() {
			let en_passant_file = next_move.start.file();
			let en_passant_rank = next_move.start.rank() as i8 + MOVE_DIRECTION[color_to_move];

			metadata.set_en_passant_square(Square::from_rank_and_file(
				en_passant_rank as u8,
				en_passant_file,
			));
		} else {
			metadata.clear_en_passant();
		}

		// Flip to move
		metadata.toggle_to_move();

		// Adjust castling rights
		if ((next_move.start | next_move.target) & CASTLE_RIGHTS_SQUARES).has_pieces() {
			for side in [CastleSide::Queen, CastleSide::King] {
				for color in [Color::White, Color::Black] {
					let rook_square = ROOKS[side as usize][color as usize];
					if (next_move.target | next_move.start).is_occupied(rook_square) {
						metadata.revoke_castling_rights_for_side(color, side);
					}
				}
			}
		} else if moved_piece == Piece::King {
			metadata.revoke_castling_rights_for_color(self.metadata.to_move());
		}

		// Increment half move clock, if needed
		if flags.is_capture() || moved_piece == Piece::Pawn {
			metadata.reset_move_clock();
		} else {
			metadata.increment_half_move();
		}

		// Calculate the new hash
		let zobrist_hash = hash_after_move(self.zobrist_hash, &self, next_move);

		Self {
			boards,
			metadata,
			zobrist_hash,
		}
	}

	/// Print the board to the console.
	///
	/// ```
	/// use gladius_core::position::Position;
	///
	/// let start_position_string = indoc::indoc! {"
	/// 	♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
	///		♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
	///		▢ ▧ ▢ ▧ ▢ ▧ ▢ ▧
	///		▧ ▢ ▧ ▢ ▧ ▢ ▧ ▢
	///		▢ ▧ ▢ ▧ ▢ ▧ ▢ ▧
	///		▧ ▢ ▧ ▢ ▧ ▢ ▧ ▢
	///		♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
	///		♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖
	/// "};
	///
	/// assert_eq!(
	/// 	start_position_string.trim(),
	/// 	Position::default().as_display_string().trim()
	/// );
	/// ```
	pub fn as_display_string(&self) -> String {
		// Assuming worst case of 4 bytes per unicode char for the chess
		// pieces, plus a space between each. Letting the overestimation
		// of the required UTF-8 bytes buffer things like linebreaks
		let mut board_string = String::with_capacity(4 * 64 * 2);

		let combined_board =
			self.get_board_for_color(Color::White) | self.get_board_for_color(Color::Black);

		for rank in (0u8..8).rev() {
			// Rev so black's side is printed first
			for file in 0u8..8 {
				let square = Square::from_rank_and_file(rank, file);
				if (combined_board & square).is_empty() {
					if (rank + file) % 2 == 0 {
						board_string.push_str("▧ ");
					} else {
						board_string.push_str("▢ ");
					}
					continue;
				}

				// Unicode offset for the black pieces is 6 since there's six unique
				// chess pieces and white comes first
				let color_offset = if (self.get_board_for_color(Color::White) & square).has_pieces()
				{
					0
				} else {
					6
				};

				let piece_offset = if (self.get_board_for_piece(Piece::King) & square).has_pieces()
				{
					0
				} else if (self.get_board_for_piece(Piece::Queen) & square).has_pieces() {
					1
				} else if (self.get_board_for_piece(Piece::Rook) & square).has_pieces() {
					2
				} else if (self.get_board_for_piece(Piece::Bishop) & square).has_pieces() {
					3
				} else if (self.get_board_for_piece(Piece::Knight) & square).has_pieces() {
					4
				} else {
					5
				};

				let white_king_unicode_code_point = 0x2654u32;
				let chess_piece_unicode =
					char::from_u32(white_king_unicode_code_point + piece_offset + color_offset)
						.unwrap();
				board_string.push(chess_piece_unicode);
				board_string.push(' ');
			}
			board_string.remove(board_string.len() - 1); // Trim final space
			board_string.push('\n');
		}
		board_string.remove(board_string.len() - 1); // Trim trailing newline
		board_string
	}
}

#[cfg(test)]
mod test {

	use super::*;
	use pretty_assertions::assert_eq;

	#[test]
	pub fn from_fen_default_position() {
		assert_eq!(
			Position::default(),
			Position::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
		)
	}

	#[test]
	pub fn from_fen_arbitrary_position() {
		let position = Position::from_fen(
			"r3k1nr/ppp2ppp/2nb4/3ppb2/3P3q/N1P2P2/PP2P1PP/1RBQKBNR w Kkq - 0 5",
		)
		.unwrap();
		let expected_board_string = indoc::indoc! {"
			♜ ▧ ▢ ▧ ♚ ▧ ♞ ♜
			♟ ♟ ♟ ▢ ▧ ♟ ♟ ♟
			▢ ▧ ♞ ♝ ▢ ▧ ▢ ▧
			▧ ▢ ▧ ♟ ♟ ♝ ▧ ▢
			▢ ▧ ▢ ♙ ▢ ▧ ▢ ♛
			♘ ▢ ♙ ▢ ▧ ♙ ▧ ▢
			♙ ♙ ▢ ▧ ♙ ▧ ♙ ♙
			▧ ♖ ♗ ♕ ♔ ♗ ♘ ♖
		"};
		assert_eq!(
			position.as_display_string().trim(),
			expected_board_string.trim()
		);

		let mut expected_metadata = PositionMetadata::default();
		expected_metadata.revoke_castling_rights_for_side(Color::White, CastleSide::Queen);
		assert_eq!(position.metadata, expected_metadata);
	}

	#[test]
	pub fn castling_rights_checked_when_castling() {
		assert_eq!(
			false,
			Position::from_fen("rnbqkbnr/pppppppp/8/8/7P/5NP1/PPPPPPB1/RNBQ1K1R w kq - 0 1")
				.unwrap()
				.can_castle(Color::White, CastleSide::King)
		);
	}

	#[test]
	pub fn can_castle_king_side() {
		assert!(Position::from_fen(
			"rnbqkb1r/pppp1pp1/4pn2/7p/2B1P3/5PPN/PPPP3P/RNBQK2R w KQkq - 0 1"
		)
		.unwrap()
		.can_castle(Color::White, CastleSide::King));

		assert!(Position::from_fen(
			"rnbqk2r/ppp2ppp/5n2/3pp3/1b1PP3/2N2P2/PPPB2PP/R2QKBNR b KQkq - 0 1"
		)
		.unwrap()
		.can_castle(Color::Black, CastleSide::King));
	}

	#[test]
	pub fn can_castle_queen_side() {
		assert!(Position::from_fen(
			"r1bqkbnr/ppp2ppp/2n5/3pp3/3P4/2NQB3/PPP1PPPP/R3KBNR w KQkq - 0 1"
		)
		.unwrap()
		.can_castle(Color::White, CastleSide::Queen));

		assert!(
			Position::from_fen("r3kbnr/pppppppp/b1n5/5q2/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1")
				.unwrap()
				.can_castle(Color::Black, CastleSide::Queen)
		);
	}
}
