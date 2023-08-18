use super::{board::Square, Piece};

/// Various settings that could be true for a move. For example, if a capture
/// occurred, if there's an en passant square after the move, or if a promotion
/// occurred during the move. Structured like this, from most to least
/// significant bit:
///
/// - Null Move (1 bit)
/// - Capture (1 bit)
/// - En Passant (1 bits)
/// - Queen's Side Castle (1 bit)
/// - King's Side Castle (1 bit)
/// - Promotion (1 bit)
#[derive(Copy, Clone, Default)]
pub struct MoveFlags(u8);

impl std::fmt::Debug for MoveFlags {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let is_null = self.0 & 0b00100000 != 0;
		let capture = self.0 & 0b00010000 != 0;
		let en_passant = self.0 & 0b00001000 != 0;
		let queen_castle = self.0 & 0b00000100 != 0;
		let king_castle = self.0 & 0b00000010 != 0;
		let promotion = self.0 & 0b00000001 != 0;
		f.debug_struct("MoveFlags")
			.field("Null Move", &is_null)
			.field("Capture", &capture)
			.field("En Passant", &en_passant)
			.field("Queen Castle", &queen_castle)
			.field("King Castle", &king_castle)
			.field("Promotions", &promotion)
			.finish()
	}
}

impl MoveFlags {
	pub fn is_null_move(&self) -> bool {
		self.0 & 0x80 != 0
	}

	pub fn is_capture(&self) -> bool {
		self.0 & 0x40 != 0
	}

	pub fn is_en_passant(&self) -> bool {
		self.0 & 0x20 != 0
	}

	pub fn is_queen_castle(&self) -> bool {
		self.0 & 0x10 != 0
	}

	pub fn is_king_castle(&self) -> bool {
		self.0 & 0x08 != 0
	}

	pub fn is_promotion(&self) -> bool {
		self.0 & 0x04 != 0
	}
}

#[derive(Debug)]
pub struct Move {
	pub flags: MoveFlags,
	pub start: Square,
	pub target: Square,
	pub piece: Piece,
	pub promotion_piece: Option<Piece>,
	pub captured_piece: Option<Piece>,
}

impl From<&str> for Move {
	/// Create a move from a string that describes the starting square and the
	/// ending square, in that order, using algebraic notation.
	///
	/// ### Examples:
	///
	/// ```
	/// use gladius_core::position::{Move, Square};
	///
	/// let first_move = Move::from("g1f3"); // White's king's side horse to f3
	/// let second_move = Move::from("e7e5"); // Black's queen pawn moves forward
	///
	/// assert_eq!(
	///   first_move.start,
	///   Square::from_algebraic_notion("g1")
	/// );
	/// assert_eq!(
	///   first_move.target,
	///   Square::from_algebraic_notion("f3")
	/// );
	///
	/// assert_eq!(
	///   second_move.start,
	///   Square::from_algebraic_notion("e7")
	/// );
	/// assert_eq!(
	///   second_move.target,
	///   Square::from_algebraic_notion("e5")
	/// );
	/// ```
	fn from(value: &str) -> Self {
		let promotion_piece = if value.len() == 5 {
			match &value[5..5] {
				"q" | "Q" => Some(Piece::Queen),
				"r" | "R" => Some(Piece::Rook),
				"b" | "B" => Some(Piece::Bishop),
				"n" | "N" => Some(Piece::Knight),
				_ => None,
			}
		} else {
			None
		};

		Self {
			flags: MoveFlags(0),
			start: Square::from_algebraic_notion(&value[..2]),
			target: Square::from_algebraic_notion(&value[2..4]),
			promotion_piece,
			piece: Piece::Bishop,
			captured_piece: None,
		}
	}
}

impl Move {
	/// If an en passant square is created from this move, find the square
	/// and return it. If no en passant square was created, returns None.
	pub fn en_passant_square(&self) -> Option<Square> {
		if self.piece != Piece::Pawn {
			return None;
		}

		let start_rank = self.start.rank();
		let end_rank = self.target.rank();

		if start_rank == 1 && end_rank == 3 {
			Some(Square::from_rank_and_file(2, self.start.file()))
		} else if start_rank == 6 && end_rank == 4 {
			Some(Square::from_rank_and_file(5, self.start.file()))
		} else {
			None
		}
	}
}
