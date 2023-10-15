use std::ops::{
	BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Mul, Not, Shl, ShlAssign, Shr,
};

use crate::position::{attacks, Color, Piece};

use super::CastleSide;

pub const CASTLE_RIGHTS_SQUARES: Board = Board(0x8100000000000081);

const ROOKS: [[Square; 2]; 2] = [
	[Square(0x0000000000000001), Square(0x0100000000000000)],
	[Square(0x0000000000000080), Square(0x8000000000000000)],
];

const KING_START: [Square; 2] = [Square(0x0000000000000010), Square(0x1000000000000000)];

const CASTLING_TARGET_SQUARE: [[Square; 2]; 2] = [
	[Square(0x0000000000000004), Square(0x0400000000000000)],
	[Square(0x0000000000000040), Square(0x4000000000000000)],
];

const ROOK_CASTLE_MOVE: [[Board; 2]; 2] = [
	[Board(0x0000000000000009), Board(0x0900000000000000)],
	[Board(0x00000000000000a0), Board(0xa000000000000000)],
];

#[inline]
pub const fn rook_start(side: CastleSide, color: Color) -> Square {
	ROOKS[side as usize][color as usize]
}

#[inline]
pub const fn king_start(color: Color) -> Square {
	KING_START[color as usize]
}

#[inline]
pub const fn castle_target_square(side: CastleSide, color: Color) -> Square {
	CASTLING_TARGET_SQUARE[side as usize][color as usize]
}

#[inline]
pub const fn rook_castle_move(side: CastleSide, color: Color) -> Board {
	ROOK_CASTLE_MOVE[side as usize][color as usize]
}

pub const A_FILE: Board = Board(0x0101010101010101);
pub const B_FILE: Board = Board(A_FILE.0 << 1);
pub const C_FILE: Board = Board(A_FILE.0 << 2);
pub const D_FILE: Board = Board(A_FILE.0 << 3);
pub const E_FILE: Board = Board(A_FILE.0 << 4);
pub const F_FILE: Board = Board(A_FILE.0 << 5);
pub const G_FILE: Board = Board(A_FILE.0 << 6);
pub const H_FILE: Board = Board(A_FILE.0 << 7);

pub const FIRST_RANK: Board = Board(0x0000000000000ff);
pub const SECOND_RANK: Board = Board(FIRST_RANK.0 << (1 * 8));
pub const THIRD_RANK: Board = Board(FIRST_RANK.0 << (2 * 8));
pub const FOURTH_RANK: Board = Board(FIRST_RANK.0 << (3 * 8));
pub const FIFTH_RANK: Board = Board(FIRST_RANK.0 << (4 * 8));
pub const SIXTH_RANK: Board = Board(FIRST_RANK.0 << (5 * 8));
pub const SEVENTH_RANK: Board = Board(FIRST_RANK.0 << (6 * 8));
pub const EIGHTH_RANK: Board = Board(FIRST_RANK.0 << (7 * 8));

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

impl From<Board> for Square {
	fn from(value: Board) -> Self {
		assert!(value.as_u64().count_ones() < 2);
		Self(value.as_u64())
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
	type Output = Board;

	fn bitor(self, rhs: Self) -> Self::Output {
		Board(self.0 | rhs.0)
	}
}

impl Shl<i8> for Square {
	type Output = Square;

	fn shl(self, rhs: i8) -> Self::Output {
		if rhs > 0 {
			Self(self.0 << rhs)
		} else {
			Self(self.0 >> rhs.abs())
		}
	}
}

impl Shr<i8> for Square {
	type Output = Square;

	fn shr(self, rhs: i8) -> Self::Output {
		if rhs > 0 {
			Self(self.0 >> rhs)
		} else {
			Self(self.0 << rhs.abs())
		}
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
	/// use gladius_core::position::board::Square;
	///
	/// assert_eq!(1, Square::from_algebraic_notation("a1").as_u64());
	/// assert_eq!(268435456, Square::from_algebraic_notation("e4").as_u64());
	/// assert_eq!(0x8000000000000000, Square::from_algebraic_notation("h8").as_u64());
	/// ```
	pub fn from_algebraic_notation(position: &str) -> Self {
		// Will be ASCII so we can just read them as bytes
		let file = position.as_bytes()[0] - ('a' as u8);
		let rank = position.as_bytes()[1] - ('1' as u8);

		Self::from_rank_and_file(rank, file)
	}

	pub fn from_lsb_index(index: u32) -> Self {
		let index = index as u8;
		Self::from_rank_and_file(index / 8, index % 8)
	}

	/// Convert a 0-indexed rank and file into a [`Square`]
	///
	/// ### Examples:
	///
	/// ```
	/// use gladius_core::position::board::Square;
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
	pub fn lsb_index(&self) -> usize {
		self.0.trailing_zeros() as usize
	}

	/// See the square as a raw u64.
	pub fn as_u64(&self) -> u64 {
		self.0
	}

	// Get the square as a usize
	pub fn as_usize(&self) -> usize {
		self.0 as usize
	}

	pub fn mirror(&self) -> Self {
		let rank = self.rank();
		let file = self.file();
		Self::from_rank_and_file(7 - rank, 7 - file)
	}

	/// Represent this square by its rank and file in a string.
	///
	/// ### Example
	/// ```
	/// use gladius_core::position::board::Square;
	///
	/// assert_eq!("a1", Square::from(1).as_algebraic_notation());
	/// assert_eq!("h8", Square::from(0x8000000000000000).as_algebraic_notation());
	/// assert_eq!("e4", Square::from_algebraic_notation("e4").as_algebraic_notation());
	/// ```
	pub fn as_algebraic_notation(&self) -> String {
		let file = self.file();
		let rank = self.rank() + 1;

		let file_char = (file + ('a' as u8)) as char;
		format!("{file_char}{rank}")
	}

	pub fn file(&self) -> u8 {
		(self.0.trailing_zeros() % 8) as u8
	}

	pub fn rank(&self) -> u8 {
		(self.0.trailing_zeros() / 8) as u8
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

impl From<Square> for Board {
	fn from(value: Square) -> Self {
		Self(value.as_u64())
	}
}

impl From<u64> for Board {
	fn from(value: u64) -> Self {
		Self(value)
	}
}

impl Iterator for Board {
	type Item = Square;

	fn next(&mut self) -> Option<Self::Item> {
		if self.0 == 0 {
			None
		} else {
			let lsb_index = self.0.trailing_zeros();
			*self = Self(self.0 & self.0 - 1);
			Some(Square::from_lsb_index(lsb_index))
		}
	}
}

impl Mul<usize> for Board {
	type Output = usize;

	fn mul(self, rhs: usize) -> Self::Output {
		rhs * self.0 as usize
	}
}

impl Not for Board {
	type Output = Self;

	fn not(self) -> Self::Output {
		Self(!self.0)
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

impl BitAnd for Board {
	type Output = Self;

	fn bitand(self, rhs: Self) -> Self::Output {
		Self(self.0 & rhs.0)
	}
}

impl BitAnd<u64> for Board {
	type Output = Self;

	fn bitand(self, rhs: u64) -> Self::Output {
		Self(self.0 & rhs)
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

impl BitXor for Board {
	type Output = Self;

	fn bitxor(self, rhs: Self) -> Self::Output {
		Self(self.0 ^ rhs.0)
	}
}

impl BitXor<Square> for Board {
	type Output = Self;

	fn bitxor(self, rhs: Square) -> Self::Output {
		Self(self.0 ^ rhs.0)
	}
}

impl BitXorAssign for Board {
	fn bitxor_assign(&mut self, rhs: Self) {
		*self = Self(self.0 ^ rhs.0)
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

impl std::fmt::Display for Board {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut inner = self.0;
		for i in 0..8 {
			write!(f, "{} ", 8 - i)?;
			let mut mask = 0x0100000000000000u64;
			for _ in 0..8 {
				if inner & mask == 0 {
					write!(f, "  ")?;
				} else {
					write!(f, "X ")?;
				}
				mask <<= 1;
			}
			inner <<= 8;
			write!(f, "\n")?;
		}
		write!(f, "  ")?;
		for i in 0..8u8 {
			write!(f, "{} ", (i + 'a' as u8) as char)?;
		}
		Ok(())
	}
}

impl Board {
	pub fn full() -> Self {
		Self(std::u64::MAX)
	}

	pub fn as_u64(&self) -> u64 {
		self.0
	}

	pub fn as_usize(&self) -> usize {
		self.0 as usize
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

lazy_static::lazy_static! {
	/// A precomputed bitboard array that contains masks that connect
	/// two squares on the board. This could be diagonally or cardinally.
	/// It includes the target square, but not the origin square.
	static ref BETWEEN: [[Board; 64]; 64] = create_between();

	/// A precomputed bitboard array that provides a line that spans the
	/// entire board and passes through two squares.
	static ref RAY_THROUGH: [[Board; 64]; 64] = create_rays();
}

/// Get a bitboard to represent all the squares on a rank, file,
/// or diagonal between `start` and `target`. `target` is included
/// in the resulting board, but `start` is not. If they can't be
/// connected by a rank, file, or diagonal, the board will be empty.
pub fn between(start: Square, target: Square) -> Board {
	BETWEEN[start.lsb_index()][target.lsb_index()]
}

/// Get a bitboard to represent all the squares across an entire rank,
/// column, or diagonal that contains both `a` and `b`. If no such
/// rank, file, or diagonal exists, the board will be empty.
pub fn ray(a: Square, b: Square) -> Board {
	RAY_THROUGH[a.lsb_index()][b.lsb_index()]
}

fn create_between() -> [[Board; 64]; 64] {
	let mut between = [[Board::default(); 64]; 64];
	for start in 0..64 {
		let start_square = Square::from_lsb_index(start);
		for piece in [Piece::Rook, Piece::Bishop] {
			for target in 0..64 {
				let target_square = Square::from_lsb_index(target);
				let attacks_from_start =
					attacks::get_attacks(target_square.into(), start_square, piece, Color::White);
				if attacks_from_start.is_occupied(target_square) {
					let attacks_from_target = attacks::get_attacks(
						start_square.into(),
						target_square,
						piece,
						Color::White,
					);
					between[start as usize][target as usize] =
						attacks_from_start & attacks_from_target | target_square;
				}
			}
		}
	}
	between
}

fn create_rays() -> [[Board; 64]; 64] {
	let mut rays = [[Board::default(); 64]; 64];
	for start in 0..64 {
		let start_square = Square::from_lsb_index(start);
		for piece in [Piece::Rook, Piece::Bishop] {
			let attacks_from_start =
				attacks::get_attacks(start_square.into(), start_square, piece, Color::White);
			for target in 0..64 {
				let target_square = Square::from_lsb_index(target);
				if attacks_from_start.is_occupied(target_square) {
					let attacks_from_target = attacks::get_attacks(
						target_square.into(),
						target_square,
						piece,
						Color::White,
					);
					rays[start as usize][target as usize] =
						(attacks_from_start & attacks_from_target) | target_square | start_square;
				}
			}
		}
	}
	rays
}
