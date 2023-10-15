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

#[derive(Copy, Clone, Debug)]
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

	pub fn to_char(&self) -> &str {
		match self {
			Self::Pawn => "p",
			Self::Rook => "r",
			Self::Knight => "n",
			Self::Bishop => "b",
			Self::Queen => "q",
			Self::King => "k",
		}
	}
}

/// 16 bits packed to represent the metadata for a given position.
/// Packing looks like this, starting with the most significant bits:
///
/// Unused extra bit (7 bits)
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
///
/// For example, if en passant is possible on the c3 square, it's black's
/// move, black can castle both king and queen's side, white can castle
/// only king's side, and there's 5 moves on the half move clock, then
/// the bit value would look like this:
///
/// 0000000 1010 1 1011
///
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct PositionMetadata(u16);

const EN_PASSANT_MASK: u16 = 0b0000000111000000;
const EN_PASSANT_SET_MASK: u16 = 0b0000000000100000;
const TO_MOVE_MASK: u16 = 0b0000000000010000;
const CASTLING_MASK: u16 = 0b0000000000001111;

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
	fn default() -> Self {
		Self(0b0000000000001111)
	}
}

impl std::fmt::Debug for PositionMetadata {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let castling = self.0 & CASTLING_MASK;
		let to_move = self.to_move();
		let en_passant_possible = self.0 & EN_PASSANT_SET_MASK != 0;
		let en_passant_file = ((self.0 & EN_PASSANT_MASK) >> 6) as u8 + ('a' as u8);
		let en_passant_rank = if self.0 & TO_MOVE_MASK == 0 { "6" } else { "3" };
		f.debug_struct("PositionMetadata")
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
		let castle_mask = 0b01 << (side as u16) << (2 * color as u16);
		self.0 & castle_mask != 0
	}

	pub fn to_move(&self) -> Color {
		if self.0 & TO_MOVE_MASK != 0 {
			Color::Black
		} else {
			Color::White
		}
	}

	pub fn toggle_to_move(&mut self) {
		*self = Self(self.0 ^ TO_MOVE_MASK);
	}

	pub fn set_to_move(&mut self, color: Color) {
		let to_move_bit = (color as u16) << TO_MOVE_MASK.trailing_zeros();
		*self = Self((self.0 & !TO_MOVE_MASK) | to_move_bit);
	}

	pub fn grant_castle_rights(&mut self, color: Color, side: CastleSide) {
		let castle_mask = 0b01 << (side as u16) << (2 * color as u16);
		*self = Self(self.0 | castle_mask);
	}

	pub fn revoke_castling_rights_for_side(&mut self, color: Color, side: CastleSide) {
		let castle_mask = 0b01 << (side as u16) << (2 * color as u16);
		*self = Self(self.0 & !castle_mask);
	}

	pub fn revoke_castling_rights_for_color(&mut self, color: Color) {
		let castle_mask = 0b11 << (2 * color as u16);
		*self = Self(self.0 & !castle_mask);
	}

	pub fn en_passant_square(&self) -> Option<Square> {
		if self.0 & EN_PASSANT_SET_MASK == 0 {
			return None;
		}

		let file: u8 = (self.0 >> 6) as u8;
		let rank = match self.to_move() {
			Color::Black => 2,
			Color::White => 5,
		};

		Some(Square::from_rank_and_file(rank, file))
	}

	pub fn clear_en_passant(&mut self) {
		*self = Self(self.0 & !EN_PASSANT_MASK & !EN_PASSANT_SET_MASK);
	}

	pub fn set_en_passant_square(&mut self, square: Square) {
		let file = square.file() as u16;
		let shifted_file = file << 6;

		*self = Self(self.0 & !EN_PASSANT_MASK | EN_PASSANT_SET_MASK | shifted_file)
	}
}

#[derive(Clone, PartialEq, Eq)]
pub struct Position {
	boards: [Board; 8],
	metadata: PositionMetadata,
	zobrist_hash: u64,
	half_move_clock: u8,
	full_move_clock: u16,
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
			half_move_clock: 0,
			full_move_clock: 1,
		}
	}
}

impl std::fmt::Debug for Position {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"Board:\n{}\nMetadata: {:?}\nHash: {}",
			self.as_display_string(),
			self.metadata,
			self.zobrist_hash,
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
			half_move_clock: 0,
			full_move_clock: 1,
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
		let to_move = match fen_components.next().ok_or(())? {
			"b" => Color::Black,
			"w" => Color::White,
			_ => return Err(()),
		};
		position.metadata.set_to_move(to_move);

		// Castling rights
		for castle_char in fen_components.next().ok_or(())?.chars() {
			let (side, color) = match castle_char {
				'k' => (CastleSide::King, Color::Black),
				'q' => (CastleSide::Queen, Color::Black),
				'K' => (CastleSide::King, Color::White),
				'Q' => (CastleSide::Queen, Color::White),
				_ => continue,
			};
			position.metadata.grant_castle_rights(color, side);
		}

		// En passant
		match fen_components.next().ok_or(())? {
			"-" => {}
			square => {
				let square = Square::from_algebraic_notation(square);
				position.metadata.set_en_passant_square(square);
			}
		}

		// Half move clock
		position.half_move_clock = fen_components
			.next()
			.map(|clock| clock.parse().ok())
			.flatten()
			.unwrap_or(0);

		// Full move clock
		position.full_move_clock = fen_components
			.next()
			.map(|clock| clock.parse().ok())
			.flatten()
			.unwrap_or(1);

		// Add the hash
		position.zobrist_hash = hash(&position.boards, &position.metadata);

		Ok(position)
	}

	pub fn to_fen(&self) -> String {
		let mut fen_string = String::with_capacity(60);

		let combined_board =
			self.get_board_for_color(Color::White) | self.get_board_for_color(Color::Black);

		let mut skipped = 0;
		for rank in (0u8..8).rev() {
			// Rev so black's side is printed first
			for file in 0u8..8 {
				let square = Square::from_rank_and_file(rank, file);
				if (combined_board & square).is_empty() {
					skipped += 1;
					continue;
				}

				if skipped > 0 {
					fen_string.push_str(&skipped.to_string());
				}
				skipped = 0;

				// Unicode offset for the black pieces is 6 since there's six unique
				// chess pieces and white comes first
				let color = self.color_on(square).unwrap();
				let piece = self.piece_on(square).unwrap();

				let piece_char = match color {
					Color::White => piece.to_char().to_uppercase(),
					Color::Black => piece.to_char().to_owned(),
				};
				fen_string.push_str(&piece_char);
			}

			if skipped > 0 {
				fen_string.push_str(&skipped.to_string());
			}
			if rank > 0 {
				skipped = 0;
				fen_string.push('/');
			}
		}
		fen_string.push(' ');

		match self.metadata.to_move() {
			Color::White => fen_string.push('w'),
			Color::Black => fen_string.push('b'),
		}
		fen_string.push(' ');

		if self.can_castle(Color::White, CastleSide::King) {
			fen_string.push('K');
		}

		if self.can_castle(Color::White, CastleSide::Queen) {
			fen_string.push('Q');
		}

		if self.can_castle(Color::Black, CastleSide::King) {
			fen_string.push('k');
		}

		if self.can_castle(Color::Black, CastleSide::Queen) {
			fen_string.push('q');
		}

		if fen_string.ends_with(' ') {
			// No castling rights left, to the last thing in the string
			// is the space after to move.
			fen_string.push('-');
		}

		fen_string.push(' ');

		if let Some(square) = self.metadata.en_passant_square() {
			fen_string.push_str(&square.as_algebraic_notation());
		} else {
			fen_string.push('-');
		}

		fen_string.push(' ');

		fen_string.push_str(&format!("{}", self.half_move_clock()));
		fen_string.push(' ');
		fen_string.push_str(&self.full_move_clock.to_string());

		fen_string
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

	pub fn to_move(&self) -> Color {
		self.metadata.to_move()
	}

	pub fn half_move_clock(&self) -> u8 {
		self.half_move_clock
	}

	pub fn full_move_clock(&self) -> u16 {
		self.full_move_clock
	}

	pub fn en_passant_square(&self) -> Option<Square> {
		self.metadata.en_passant_square()
	}

	pub fn hash(&self) -> u64 {
		self.zobrist_hash
	}

	pub fn any_checks(&self) -> bool {
		self.is_color_in_check(Color::White) || self.is_color_in_check(Color::Black)
	}

	pub fn is_color_in_check(&self, color: Color) -> bool {
		let king_square =
			Square::from(self.get_board_for_color(color) & self.get_board_for_piece(Piece::King));
		attackers_of_square(king_square, !color, self).has_pieces()
	}

	pub fn apply_move(&self, next_move: &moves::Move) -> Position {
		let color_to_move = self.metadata.to_move() as usize;
		let moved_piece = self.piece_on(next_move.start).unwrap();
		let flags = next_move.flags;

		let mut new_position = self.to_owned();

		if let Some(captured_piece) = self.piece_on(next_move.target) {
			new_position.boards[1 - color_to_move] ^= next_move.target;
			new_position.boards[captured_piece as usize] ^= next_move.target;
		} else if let Some(side) = flags.castling_side() {
			// Move the Rook also
			let rook_move = ROOK_CASTLE_MOVE[side as usize][color_to_move];
			new_position.boards[Piece::Rook as usize] ^= rook_move;
			new_position.boards[color_to_move] ^= rook_move;
		} else if flags.is_en_passant() {
			let captured_piece_square = next_move.target >> (8 * MOVE_DIRECTION[color_to_move]);
			new_position.boards[Piece::Pawn as usize] ^= captured_piece_square;
			new_position.boards[1 - color_to_move] ^= captured_piece_square;
		}

		new_position.boards[color_to_move] ^= next_move.start | next_move.target;
		new_position.boards[moved_piece as usize] ^= next_move.start | next_move.target;

		if let Some(piece) = flags.promotion_piece() {
			new_position.boards[Piece::Pawn as usize] ^= next_move.target;
			new_position.boards[piece as usize] ^= next_move.target;
		}

		// Set en passant square
		if flags.is_double_pawn_push() {
			let en_passant_file = next_move.start.file();
			let en_passant_rank = next_move.start.rank() as i8 + MOVE_DIRECTION[color_to_move];

			new_position
				.metadata
				.set_en_passant_square(Square::from_rank_and_file(
					en_passant_rank as u8,
					en_passant_file,
				));
		} else {
			new_position.metadata.clear_en_passant();
		}

		// Flip to move
		new_position.metadata.toggle_to_move();

		// Adjust castling rights
		if ((next_move.start | next_move.target) & CASTLE_RIGHTS_SQUARES).has_pieces() {
			for side in [CastleSide::Queen, CastleSide::King] {
				for color in [Color::White, Color::Black] {
					let rook_square = ROOKS[side as usize][color as usize];
					if (next_move.target | next_move.start).is_occupied(rook_square) {
						new_position
							.metadata
							.revoke_castling_rights_for_side(color, side);
					}
				}
			}
		} else if moved_piece == Piece::King {
			new_position
				.metadata
				.revoke_castling_rights_for_color(self.metadata.to_move());
		}

		// Increment or reset half move clock
		new_position.half_move_clock = if flags.is_capture() || moved_piece == Piece::Pawn {
			0
		} else {
			self.half_move_clock + 1
		};

		new_position.full_move_clock = match new_position.metadata.to_move() {
			Color::White => self.full_move_clock + 1,
			Color::Black => self.full_move_clock,
		};

		// Calculate the new hash
		new_position.zobrist_hash = hash_after_move(&self, &new_position, next_move);

		new_position
	}

	/// Print the board to the console.
	///
	/// ```
	/// use gladius_core::position::Position;
	///
	/// let start_position_string = indoc::indoc! {"
	/// 	8 ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
	///		7 ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
	///		6 ▢ ▧ ▢ ▧ ▢ ▧ ▢ ▧
	///		5 ▧ ▢ ▧ ▢ ▧ ▢ ▧ ▢
	///		4 ▢ ▧ ▢ ▧ ▢ ▧ ▢ ▧
	///		3 ▧ ▢ ▧ ▢ ▧ ▢ ▧ ▢
	///		2 ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
	///		1 ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖
	///		  A B C D E F G H
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

		for rank in (0u8..8).rev() {
			// Rev so black's side is printed first
			board_string.push((rank + '1' as u8) as char);
			board_string.push(' ');
			for file in 0u8..8 {
				let square = Square::from_rank_and_file(rank, file);

				let square_contents = match (self.color_on(square), self.piece_on(square)) {
					(Some(Color::White), Some(Piece::Pawn)) => '♙',
					(Some(Color::White), Some(Piece::Rook)) => '♖',
					(Some(Color::White), Some(Piece::Knight)) => '♘',
					(Some(Color::White), Some(Piece::Bishop)) => '♗',
					(Some(Color::White), Some(Piece::Queen)) => '♕',
					(Some(Color::White), Some(Piece::King)) => '♔',
					(Some(Color::Black), Some(Piece::Pawn)) => '♟',
					(Some(Color::Black), Some(Piece::Rook)) => '♜',
					(Some(Color::Black), Some(Piece::Knight)) => '♞',
					(Some(Color::Black), Some(Piece::Bishop)) => '♝',
					(Some(Color::Black), Some(Piece::Queen)) => '♛',
					(Some(Color::Black), Some(Piece::King)) => '♚',
					_ if (rank + file) % 2 == 0 => '▧',
					_ => '▢',
				};

				board_string.push(square_contents);
				board_string.push(' ');
			}
			board_string.remove(board_string.len() - 1); // Trim final space
			board_string.push('\n');
		}
		board_string.push_str("  A B C D E F G H");
		board_string
	}
}

#[cfg(test)]
mod test {

	use super::moves::Move;
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
			8 ♜ ▧ ▢ ▧ ♚ ▧ ♞ ♜
			7 ♟ ♟ ♟ ▢ ▧ ♟ ♟ ♟
			6 ▢ ▧ ♞ ♝ ▢ ▧ ▢ ▧
			5 ▧ ▢ ▧ ♟ ♟ ♝ ▧ ▢
			4 ▢ ▧ ▢ ♙ ▢ ▧ ▢ ♛
			3 ♘ ▢ ♙ ▢ ▧ ♙ ▧ ▢
			2 ♙ ♙ ▢ ▧ ♙ ▧ ♙ ♙
			1 ▧ ♖ ♗ ♕ ♔ ♗ ♘ ♖
			  A B C D E F G H
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
		let position =
			Position::from_fen("rnbqkb1r/pppp1pp1/4pn2/7p/2B1P3/5PPN/PPPP3P/RNBQK2R w KQkq - 0 1")
				.unwrap();
		assert!(position.can_castle(Color::White, CastleSide::King));

		let position = position.apply_move(&Move::from_uci_str("e1g1", &position));
		assert_eq!(
			Some(Piece::King),
			position.piece_on(Square::from_algebraic_notation("g1"))
		);
		assert_eq!(
			Some(Piece::Rook),
			position.piece_on(Square::from_algebraic_notation("f1"))
		);
		assert!(!position.can_castle(Color::White, CastleSide::King));
		assert!(!position.can_castle(Color::White, CastleSide::Queen));

		let position = Position::from_fen(
			"rnbqk2r/ppp2ppp/5n2/3pp3/1b1PP3/2N2P2/PPPB2PP/R2QKBNR b KQkq - 0 1",
		)
		.unwrap();
		assert!(position.can_castle(Color::Black, CastleSide::King));

		let position = position.apply_move(&Move::from_uci_str("e8g8", &position));
		assert_eq!(
			Some(Piece::King),
			position.piece_on(Square::from_algebraic_notation("g8"))
		);
		assert_eq!(
			Some(Piece::Rook),
			position.piece_on(Square::from_algebraic_notation("f8"))
		);
		assert!(!position.can_castle(Color::Black, CastleSide::King));
		assert!(!position.can_castle(Color::Black, CastleSide::Queen));
	}

	#[test]
	pub fn can_castle_queen_side() {
		// Try to long castle with white
		let position =
			Position::from_fen("r1bqkbnr/ppp2ppp/2n5/3pp3/3P4/2NQB3/PPP1PPPP/R3KBNR w KQkq - 0 1")
				.unwrap();
		assert!(position.can_castle(Color::White, CastleSide::Queen));

		let position = position.apply_move(&Move::from_uci_str("e1c1", &position));
		assert_eq!(
			Some(Piece::King),
			position.piece_on(Square::from_algebraic_notation("c1"))
		);
		assert_eq!(
			Some(Piece::Rook),
			position.piece_on(Square::from_algebraic_notation("d1"))
		);
		assert!(!position.can_castle(Color::White, CastleSide::King));
		assert!(!position.can_castle(Color::White, CastleSide::Queen));

		// Repeat with black
		let position =
			Position::from_fen("r3kbnr/pppppppp/b1n5/5q2/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1")
				.unwrap();
		assert!(position.can_castle(Color::Black, CastleSide::Queen));

		let position = position.apply_move(&Move::from_uci_str("e8c8", &position));
		assert_eq!(
			Some(Piece::King),
			position.piece_on(Square::from_algebraic_notation("c8"))
		);
		assert_eq!(
			Some(Piece::Rook),
			position.piece_on(Square::from_algebraic_notation("d8"))
		);
		assert!(!position.can_castle(Color::Black, CastleSide::King));
		assert!(!position.can_castle(Color::Black, CastleSide::Queen));
	}
}
