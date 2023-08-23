use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXorAssign, ShlAssign};

pub const WHITE_QUEEN_ROOK: Square = Square(0x8000000000000000);
pub const WHITE_KING_ROOK: Square = Square(0x0100000000000000);
pub const BLACK_QUEEN_ROOK: Square = Square(0x0000000000000080);
pub const BLACK_KING_ROOK: Square = Square(0x0000000000000001);

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Square(u64);

impl std::ops::Deref for Square {
	type Target = u64;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl From<u64> for Square {
	fn from(value: u64) -> Self {
		Self(value)
	}
}

impl BitAndAssign for Square {
	fn bitand_assign(&mut self, rhs: Self) {
		*self = Self(self.0 & rhs.0)
	}
}

impl BitAnd<u64> for Square {
	type Output = u64;

	fn bitand(self, rhs: u64) -> Self::Output {
		self.0 & rhs
	}
}

impl BitOr for Square {
	type Output = Self;

	fn bitor(self, rhs: Self) -> Self::Output {
		Self(self.0 | rhs.0)
	}
}

impl std::fmt::Debug for Square {
	/// Prints the position in algebraic notation for simplicity.
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.as_algebraic_notation())
	}
}

impl Square {
	/// Convert the algebraic notation for a single square into a [`Square`].
	/// ### Examples:
	///
	/// ```
	/// use gladius_core::position::Square;
	///
	/// assert_eq!(1, Square::from_algebraic_notion("a1").as_u64());
	/// assert_eq!(268435456, Square::from_algebraic_notion("e4").as_u64());
	/// assert_eq!(0x8000000000000000, Square::from_algebraic_notion("h8").as_u64());
	/// ```
	pub fn from_algebraic_notion(position: &str) -> Self {
		// Will be ASCII so we can just read them as bytes
		let file = position.as_bytes()[0] - ('a' as u8);
		let rank = position.as_bytes()[1] - ('1' as u8);

		Self::from_rank_and_file(rank, file)
	}

	/// Convert a 0-indexed rank and file into a [`Square`]
	///
	/// ### Examples:
	///
	/// ```
	/// use gladius_core::position::Square;
	///
	/// assert_eq!("a1", Square::from_rank_and_file(0, 0).as_algebraic_notation());
	/// assert_eq!("h8", Square::from_rank_and_file(7, 7).as_algebraic_notation());
	/// ```
	pub fn from_rank_and_file(rank: u8, file: u8) -> Self {
		let board = 1u64 << file << (8 * rank);
		Self(board)
	}

	/// Get the index of the only bit set to one, representing the selected square.
	/// This allows the engine to use the square to index into arrays of attacks
	/// based on the square of the piece.
	pub fn as_bit_index(&self) -> u32 {
		self.0.trailing_zeros()
	}

	/// See the square as a raw u64.
	pub fn as_u64(&self) -> u64 {
		self.0
	}

	/// Represent this square by its rank and file in a string.
	///
	/// ### Example
	/// ```
	/// use gladius_core::position::Square;
	///
	/// assert_eq!("a1", Square::from(1).as_algebraic_notation());
	/// assert_eq!("h8", Square::from(0x8000000000000000).as_algebraic_notation());
	/// assert_eq!("e4", Square::from_algebraic_notion("e4").as_algebraic_notation());
	/// ```
	pub fn as_algebraic_notation(&self) -> String {
		let file = self.file();
		let rank = self.rank();

		let file_char = (file + ('a' as u8)) as char;
		format!("{file_char}{rank}")
	}

	pub fn file(&self) -> u8 {
		(self.0.trailing_zeros() % 8) as u8
	}

	pub fn rank(&self) -> u8 {
		(self.0.trailing_zeros() / 8 + 1) as u8
	}

	/// To make it easier to grok the position, Debug is implemented to
	/// print the board with an X at the location specified by this
	/// position. All other squares are shown with a dash ("-").
	pub fn as_board_string(&self) -> String {
		// 2 chars per square = 64 * 2 = 128
		// Letter and number for rank and file markers = 32
		// Newlines = 9
		// Total = 128 + 32 + 9 = 169B since this all ASCII
		let mut board = String::with_capacity(169);

		for rank in (0u8..8).rev() {
			let file_char = (rank + ('0' as u8)) as char;
			board.push(file_char);
			board.push(' ');
			let rank = 8 * rank;
			for file in 0u8..8 {
				let square = Square::from_rank_and_file(rank, file);
				if square.0 == self.0 {
					board.push_str("X ");
				} else if rank + file % 2 == 0 {
					board.push_str("▢ ");
				} else {
					board.push_str("▧ ");
				}
			}
			board.push('\n');
		}
		board.push_str("  A B C D E F G H");
		board
	}
}

#[derive(Copy, Clone, Default, Eq, PartialEq)]
pub struct Board(u64);

impl std::ops::Deref for Board {
	type Target = u64;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl From<u64> for Board {
	fn from(value: u64) -> Self {
		Self(value)
	}
}

impl BitAndAssign for Board {
	fn bitand_assign(&mut self, rhs: Self) {
		*self = Self(self.0 & rhs.0);
	}
}

impl BitAndAssign<u64> for Board {
	fn bitand_assign(&mut self, rhs: u64) {
		*self = Self(self.0 & rhs);
	}
}

impl BitOrAssign for Board {
	fn bitor_assign(&mut self, rhs: Self) {
		*self = Self(self.0 | rhs.0);
	}
}

impl BitOrAssign<u64> for Board {
	fn bitor_assign(&mut self, rhs: u64) {
		*self = Self(self.0 | rhs)
	}
}

impl BitAnd<u64> for Board {
	type Output = u64;

	fn bitand(self, rhs: u64) -> Self::Output {
		self.0 & rhs
	}
}

impl BitAnd<Square> for Board {
	type Output = Board;

	fn bitand(self, rhs: Square) -> Self::Output {
		Self(self.0 & rhs.0)
	}
}

impl BitOr for Board {
	type Output = Self;

	fn bitor(self, rhs: Self) -> Self::Output {
		Self(self.0 | rhs.0)
	}
}

impl BitOr<Square> for Board {
	type Output = Self;

	fn bitor(self, rhs: Square) -> Self::Output {
		Self(self.0 | rhs.0)
	}
}

impl BitXorAssign<Square> for Board {
	fn bitxor_assign(&mut self, rhs: Square) {
		*self = Self(self.0 ^ rhs.0);
	}
}

impl ShlAssign<usize> for Board {
	fn shl_assign(&mut self, rhs: usize) {
		*self = Self(self.0 << rhs)
	}
}

impl BitOrAssign<Square> for Board {
	fn bitor_assign(&mut self, rhs: Square) {
		*self = Self(self.0 | rhs.0);
	}
}

impl BitAndAssign<Square> for Board {
	fn bitand_assign(&mut self, rhs: Square) {
		*self = Self(self.0 & rhs.0);
	}
}

impl BitXorAssign<u64> for Board {
	fn bitxor_assign(&mut self, rhs: u64) {
		*self = Self(self.0 ^ rhs);
	}
}

impl Board {
	pub fn as_u64(&self) -> u64 {
		self.0
	}

	pub fn is_occupied(&self, square: Square) -> bool {
		self.0 & square.0 != 0
	}

	pub fn is_empty(&self) -> bool {
		self.0 == 0
	}

	pub fn has_pieces(&self) -> bool {
		self.0 != 0
	}
}
