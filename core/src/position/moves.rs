use super::{board::{Square, KING_START, KING_CASTLE_SQUARE, QUEEN_CASTLE_SQUARE}, Piece, Position, attacks, CastleSide};

pub const MOVE_DIRECTION: [i8; 2] = [1, -1];
pub const STARTING_PAWNS: [u64; 2] = [0x000000000000ff00, 0x00ff000000000000];
pub const BACK_RANK: [u8; 2] = [7, 0];

/// Various settings that could be true for a move. For example, if a capture
/// occurred, if there's an en passant square after the move, or if a promotion
/// occurred during the move. See the chess programming wiki for an explanation
/// of the different values:
/// https://www.chessprogramming.org/Encoding_Moves#From-To_Based
/// 
/// This is essentially a hand spun enum that saves spaces compared to a normal
/// enum since it uses only 1 byte instead of the minimum of 8 bytes (1 64-bit
/// machine word) used as the discriminate in vanilla enums. In an effort to use
/// as few dependencies as possible for my own amusement, I've also opted out of
/// using the `primitive_enum` crate.
#[derive(Copy, Clone, Default)]
pub struct MoveFlags(u8);

impl std::fmt::Debug for MoveFlags {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let move_type = match self.0 {
			0 => "Quiet move",
			1 => "Double pawn push",
			2 => "King castle",
			3 => "Queen castle",
			4 => "Capture",
			5 => "En Passant",
			8 => "Knight promotion",
			9 => "Bishop promotion",
			10 => "Rook promotion",
			11 => "Queen promotion",
			12 => "Knight promotion with capture",
			13 => "Bishop promotion with capture",
			14 => "Rook promotion, with capture",
			15 => "Queen promotion, with capture",
			_ => unreachable!("Invalid move metadata: {}", self.0),
		};
		write!(f, "{move_type}")
	}
}

impl MoveFlags {
	pub fn quiet_move() -> Self {
		Self(0)
	}

	pub fn double_pawn_push() -> Self {
		Self(1)
	}

	pub fn king_castling() -> Self {
		Self(2)
	}

	pub fn queen_castling() -> Self {
		Self(3)
	}

	pub fn capture() -> Self {
		Self(4)
	}

	pub fn en_passant() -> Self {
		Self(5)
	}

	pub fn knight_promotion() -> Self {
		Self(8)
	}

	pub fn bishop_promotion() -> Self {
		Self(9)
	}

	pub fn rook_promotion() -> Self {
		Self(10)
	}

	pub fn queen_promotion() -> Self {
		Self(11)
	}

	pub fn knight_promotion_capture() -> Self {
		Self(12)
	}

	pub fn bishop_promotion_capture() -> Self {
		Self(13)
	}

	pub fn rook_promotion_capture() -> Self {
		Self(14)
	}

	pub fn queen_promotion_capture() -> Self {
		Self(15)
	}

	pub fn promotion_piece(&self) -> Option<Piece> {
		match self.0 {
			8  | 12 => Some(Piece::Knight),
			9  | 13 => Some(Piece::Bishop),
			10 | 14 => Some(Piece::Rook),
			11 | 15 => Some(Piece::Queen),
			_ => None,
		}
	}

	pub fn is_quiet_move(&self) -> bool {
		self.0 == 0	
	}

	pub fn is_double_pawn_push(&self) -> bool {
		self.0 == 1
	} 

	pub fn is_capture(&self) -> bool {
		self.0 == 4  || self.0 == 5  || self.0 >= 12
	}

	pub fn is_en_passant(&self) -> bool {
		self.0 == 5
	}

	pub fn is_queen_castle(&self) -> bool {
		self.0 == 3
	}

	pub fn is_king_castle(&self) -> bool {
		self.0 == 2
	}

	pub fn is_promotion(&self) -> bool {
		self.0 >= 8
	}
}

#[derive(Debug)]
pub struct Move {
	pub flags: MoveFlags,
	pub start: Square,
	pub target: Square,
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
		todo!()
	}
}

pub fn parse_move(move_str: &str, position: &Position) -> Move {
	let metadata = position.metadata;
	let color_to_move = metadata.to_move();
	let start = Square::from_algebraic_notion(&move_str[0..2]);
	let target = Square::from_algebraic_notion(&move_str[2..4]);

	let mut flags = MoveFlags::default();

	let capture = position.get_board_for_color(!color_to_move).is_occupied(target);
	if capture {
		flags = MoveFlags::capture(); 
	}

	match move_str.get(5..5) {
		Some("q") | Some ("Q") => {
			if capture { 
				flags = MoveFlags::queen_promotion_capture(); 
			} else {
				flags = MoveFlags::queen_promotion();
			}
		}
		Some("n") | Some("N") => {
			if capture { 
				flags = MoveFlags::knight_promotion_capture();
			} else {
				flags = MoveFlags::knight_promotion();
			}
		}
		Some("b") | Some ("B") => {
			if capture { 
				flags = MoveFlags::bishop_promotion_capture();
			} else {
				flags = MoveFlags::bishop_promotion();
			}
		}
		Some("r") | Some ("R") => {
			if capture { 
				flags = MoveFlags::rook_promotion_capture(); 
			} else {
				flags = MoveFlags::rook_promotion();
			}
		}
		_ => {}
	}
	
	let piece = position.piece_on(start).unwrap();

	if piece == Piece::Pawn {
		let is_en_passant = match metadata.en_passant_square() {
			Some(en_passant_square) => en_passant_square == target,
			None => false,
		};

		if is_en_passant {
			flags = MoveFlags::en_passant();
		}

		if start.rank().abs_diff(target.rank()) == 2 {
			flags = MoveFlags::double_pawn_push();
		}
	}

	let starting_king_square = KING_START[color_to_move as usize];
	if piece == Piece::King && start == starting_king_square {
		let king_side_castle_square = KING_CASTLE_SQUARE[color_to_move as usize];
		let queen_side_castle_square = KING_CASTLE_SQUARE[color_to_move as usize];
		if target == king_side_castle_square {
			flags = MoveFlags::king_castling();
		} else if target == queen_side_castle_square {
			flags = MoveFlags::queen_castling();
		}
	}

	Move {
		flags,
		start,
		target,
	}
}

pub fn generate_moves(position: &Position) -> Vec<Move> {
	let metadata = position.metadata;
	let to_move = metadata.to_move();

	let mut moves = Vec::new();

	// Normal moves for the less weird pieces
	standard_moves_for_piece(Piece::Rook, position, &mut moves);
	standard_moves_for_piece(Piece::Knight, position, &mut moves);
	standard_moves_for_piece(Piece::Bishop, position, &mut moves);
	standard_moves_for_piece(Piece::Queen, position, &mut moves);
	standard_moves_for_piece(Piece::King, position, &mut moves);

	// Castling
	if metadata.can_castle(to_move, CastleSide::King) {
		let start = KING_START[to_move as usize];
		let target = KING_CASTLE_SQUARE[to_move as usize];
		moves.push(Move { start, target, flags: MoveFlags::king_castling() });
	}

	if metadata.can_castle(to_move, CastleSide::Queen) {
		let start = KING_START[to_move as usize];
		let target = QUEEN_CASTLE_SQUARE[to_move as usize];
		moves.push(Move { start, target, flags: MoveFlags::queen_castling() });
	}

	// Pawns (ugh)
	pawn_single_moves(position, &mut moves);
	pawn_double_moves(position, &mut moves);

	moves
}

/// Get all the standard moves for a piece, eg the ones that are created by it executing
/// the normal way it moves. It excludes things like castling and pawns, since pawns have
/// *so many edge cases*.
fn standard_moves_for_piece(piece: Piece, position: &Position, moves: &mut Vec<Move>) {
	let board_piece = position.get_board_for_piece(piece);
	let to_move_color_board = position.get_board_for_color(position.metadata.to_move());
	let opponent_board = position.get_board_for_color(!position.metadata.to_move());
	let total_occupancy = to_move_color_board | opponent_board; 
	let piece_of_color = board_piece & to_move_color_board;

	for square in piece_of_color {
		let attacks = attacks::get_attacks(total_occupancy, square, piece) & !to_move_color_board;
		for attacked_square in attacks {
			let flags = if opponent_board.is_occupied(attacked_square) {
				MoveFlags::capture()
			} else {
				MoveFlags::quiet_move()
			};
			moves.push(Move { start: square, target: attacked_square, flags });
		}
	}
}

fn pawn_single_moves(position: &Position, moves: &mut Vec<Move>) {
	let to_move_color = position.metadata.to_move();
	let board_piece = position.get_board_for_piece(Piece::Pawn);
	let to_move_color_board = position.get_board_for_color(position.metadata.to_move());
	let opponent_board = position.get_board_for_color(!position.metadata.to_move());
	let total_occupancy = to_move_color_board | opponent_board; 
	let piece_of_color = board_piece & to_move_color_board;
	let move_direction = MOVE_DIRECTION[to_move_color as usize];
	let back_rank = BACK_RANK[to_move_color as usize];
	let en_passant_square = position.metadata.en_passant_square();

	for start in piece_of_color {
		// Single forward push
		let next_rank = (start.rank() as i8 + move_direction) as u8;
		let target = Square::from_rank_and_file(next_rank, start.file());
		if !total_occupancy.is_occupied(target) {
			if next_rank == back_rank {
				moves.push(Move { start, target, flags: MoveFlags::knight_promotion() });
				moves.push(Move { start, target, flags: MoveFlags::bishop_promotion() });
				moves.push(Move { start, target, flags: MoveFlags::rook_promotion() });
				moves.push(Move { start, target, flags: MoveFlags::queen_promotion() });
			} else {
				moves.push(Move { start, target, flags: MoveFlags::quiet_move() });
			}
		}

		// Left diagonal attacks and en passant
		let left_file = start.file() as i8 - 1;
		if left_file > 0 {
			let target = Square::from_rank_and_file(next_rank, left_file as u8);
			let targeting_en_passant = en_passant_square.map(|sq| sq == target).unwrap_or(false);
			if opponent_board.is_occupied(target) {
				moves.push(Move { start, target,  flags: MoveFlags::capture() });
			} else if targeting_en_passant {
				moves.push(Move { start, target, flags: MoveFlags::en_passant() })
			}
		}

		// Right diagonal attacks and en passant
		let right_file = start.file() + 1;
		if right_file < 8 {
			let target = Square::from_rank_and_file(next_rank, right_file);
			let targeting_en_passant = en_passant_square.map(|sq| sq == target).unwrap_or(false);
			if opponent_board.is_occupied(target) {
				moves.push(Move { start, target,  flags: MoveFlags::capture() });
			} else if targeting_en_passant {
				moves.push(Move { start, target, flags: MoveFlags::en_passant() })
			}
		}
	}
}

fn pawn_double_moves(position: &Position, moves: &mut Vec<Move>) {
	let to_move_color = position.metadata.to_move();
	let board_piece = position.get_board_for_piece(Piece::Pawn) & STARTING_PAWNS[to_move_color as usize];
	let to_move_color_board = position.get_board_for_color(position.metadata.to_move());
	let opponent_board = position.get_board_for_color(!position.metadata.to_move());
	let total_occupancy = to_move_color_board | opponent_board; 
	let piece_of_color = board_piece & to_move_color_board;

	for start in piece_of_color {
		let target_rank = (start.rank() as i8 + MOVE_DIRECTION[to_move_color as usize] * 2) as u8;
		let target = Square::from_rank_and_file(target_rank, start.file());
		if !total_occupancy.is_occupied(target) {
			moves.push(Move { start, target, flags: MoveFlags::double_pawn_push() });
		}
	}
} 
