use super::{
	attacks,
	board::{
		between, ray, Board, Square, A_FILE, EIGHTH_RANK, FIRST_RANK, H_FILE, KING_CASTLE_SQUARE,
		KING_START, QUEEN_CASTLE_SQUARE, ROOKS,
	},
	CastleSide, Piece, Position,
};

pub const MOVE_DIRECTION: [i8; 2] = [1, -1];
pub const STARTING_PAWNS: [u64; 2] = [0x000000000000ff00, 0x00ff000000000000];

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
#[derive(Copy, Clone, Default, Eq, PartialEq)]
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
			8 | 12 => Some(Piece::Knight),
			9 | 13 => Some(Piece::Bishop),
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
		self.0 & 4 != 0
	}

	pub fn is_en_passant(&self) -> bool {
		self.0 == 5
	}

	pub fn is_castling(&self) -> bool {
		self.is_queen_castle() || self.is_king_castle()
	}

	pub fn castling_side(&self) -> Option<CastleSide> {
		match self.0 {
			2 => Some(CastleSide::King),
			3 => Some(CastleSide::Queen),
			_ => None,
		}
	}

	pub fn is_queen_castle(&self) -> bool {
		self.0 == 3
	}

	pub fn is_king_castle(&self) -> bool {
		self.0 == 2
	}

	pub fn is_promotion(&self) -> bool {
		self.0 & 8 != 0
	}
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Move {
	/// Extra information about the move, like capture status or promotions
	pub flags: MoveFlags,

	/// The origin square
	pub start: Square,

	/// The destination square.
	pub target: Square,
}

impl Move {
	pub fn null_move() -> Self {
		Self::new(Square::from(1), Square::from(1), MoveFlags::default())
	}

	pub fn new(start: Square, target: Square, flags: MoveFlags) -> Self {
		Self {
			start,
			target,
			flags,
		}
	}
	/// Read a move from a UCI formatted string, like a2a4.
	///
	/// ### Example
	/// ```
	/// use gladius_core::position::{Position, moves::{Move, MoveFlags}, board::Square};
	///
	/// let parsed_move = Move::from_uci_str("a2a4", &Position::default());
	/// assert_eq!(
	/// 	parsed_move.start,
	/// 	Square::from_algebraic_notation("a2")
	/// );
	///
	/// assert_eq!(
	/// 	parsed_move.target,
	/// 	Square::from_algebraic_notation("a4")
	/// );
	///
	/// assert_eq!(
	/// 	parsed_move.flags,
	/// 	MoveFlags::double_pawn_push()
	/// );
	/// ```
	pub fn from_uci_str(move_str: &str, position: &Position) -> Move {
		let metadata = position.metadata;
		let color_to_move = metadata.to_move();
		let start = Square::from_algebraic_notation(&move_str[0..2]);
		let target = Square::from_algebraic_notation(&move_str[2..4]);

		let mut flags = MoveFlags::default();

		let capture = position
			.get_board_for_color(!color_to_move)
			.is_occupied(target);
		if capture {
			flags = MoveFlags::capture();
		}

		match move_str.get(5..5) {
			Some("q") | Some("Q") => {
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
			Some("b") | Some("B") => {
				if capture {
					flags = MoveFlags::bishop_promotion_capture();
				} else {
					flags = MoveFlags::bishop_promotion();
				}
			}
			Some("r") | Some("R") => {
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
			let queen_side_castle_square = QUEEN_CASTLE_SQUARE[color_to_move as usize];
			if target == king_side_castle_square {
				flags = MoveFlags::king_castling();
			} else if target == queen_side_castle_square {
				flags = MoveFlags::queen_castling();
			}
		}

		Move::new(start, target, flags)
	}
}

impl std::fmt::Display for Move {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if let Some(piece) = self.flags.promotion_piece() {
			let piece_string = match piece {
				Piece::Knight => "n",
				Piece::Rook => "r",
				Piece::Bishop => "b",
				Piece::Queen => "q",
				_ => "",
			};
			write!(
				f,
				"{}{}{}",
				self.start.as_algebraic_notation(),
				self.target.as_algebraic_notation(),
				piece_string
			)
		} else {
			write!(
				f,
				"{}{}",
				self.start.as_algebraic_notation(),
				self.target.as_algebraic_notation()
			)
		}
	}
}

/// Generate all legal moves from the starting position.
/// #### Parameters:
/// - position: the current board position.
/// #### Return
/// A vector of all the moves that can be played from the current
/// position. If the list is empty, it's checkmate!
pub fn generate_moves<const QUIESCENT: bool>(position: &Position) -> Vec<Move> {
	let metadata = position.metadata;
	let to_move = metadata.to_move();

	let mut moves = Vec::new();

	// Find all the absolute pins, which prevents a piece from moving.
	// En passant pins are handled separately.
	let pins = pins(position);

	// Normal moves for the less weird pieces
	standard_moves_for_piece(Piece::Rook, position, pins, &mut moves);
	standard_moves_for_piece(Piece::Knight, position, pins, &mut moves);
	standard_moves_for_piece(Piece::Bishop, position, pins, &mut moves);
	standard_moves_for_piece(Piece::Queen, position, pins, &mut moves);
	standard_moves_for_piece(Piece::King, position, pins, &mut moves);

	// Castling
	if metadata.can_castle(to_move, CastleSide::King) {
		let start = KING_START[to_move as usize];
		let target = KING_CASTLE_SQUARE[to_move as usize];
		moves.push(Move::new(start, target, MoveFlags::king_castling()));
	}

	if metadata.can_castle(to_move, CastleSide::Queen) {
		let start = KING_START[to_move as usize];
		let target = QUEEN_CASTLE_SQUARE[to_move as usize];
		moves.push(Move::new(start, target, MoveFlags::queen_castling()));
	}

	// Pawns
	pawn_single_moves(position, pins, &mut moves);
	pawn_double_moves(position, pins, &mut moves);

	let king_square = Square::from(
		position.get_board_for_piece(Piece::King) & position.get_board_for_color(to_move),
	);
	let king_attackers = attacks::attackers_of_square(king_square, !to_move, position);

	if QUIESCENT {
		moves
			.into_iter()
			.filter(|m| m.flags.is_capture() && is_legal(position, king_attackers, m))
			.collect()
	} else {
		moves
			.into_iter()
			.filter(|m| is_legal(position, king_attackers, m))
			.collect()
	}
}

/// Get all the standard moves for a piece, eg the ones that are created by it executing
/// the normal way it moves. It excludes things like castling and pawns, since pawns have
/// *so many edge cases*.
fn standard_moves_for_piece(piece: Piece, position: &Position, pins: Board, moves: &mut Vec<Move>) {
	let board_piece = position.get_board_for_piece(piece);
	let to_move_color = position.metadata.to_move();
	let to_move_color_board = position.get_board_for_color(to_move_color);
	let opponent_board = position.get_board_for_color(!to_move_color);
	let total_occupancy = position.get_occupancy_board();
	let piece_of_color = board_piece & to_move_color_board;
	let king_square = Square::from(position.get_board_for_piece(Piece::King) & to_move_color_board);

	for square in piece_of_color {
		let pin_limitation = pin_mask(pins, king_square, square);
		let attacks = attacks::get_attacks(total_occupancy, square, piece, to_move_color)
			& !to_move_color_board
			& pin_limitation;
		for attacked_square in attacks {
			let flags = if opponent_board.is_occupied(attacked_square) {
				MoveFlags::capture()
			} else {
				MoveFlags::quiet_move()
			};
			moves.push(Move::new(square, attacked_square, flags));
		}
	}
}

fn pawn_single_moves(position: &Position, pins: Board, moves: &mut Vec<Move>) {
	let to_move_color = position.metadata.to_move();
	let board_piece = position.get_board_for_piece(Piece::Pawn);
	let to_move_color_board = position.get_board_for_color(position.metadata.to_move());
	let opponent_board = position.get_board_for_color(!position.metadata.to_move());
	let total_occupancy = position.get_occupancy_board();
	let piece_of_color = board_piece & to_move_color_board;
	let move_direction = MOVE_DIRECTION[to_move_color as usize];
	let en_passant_square = position.metadata.en_passant_square();
	let king_square = Square::from(position.get_board_for_piece(Piece::King) & to_move_color_board);
	let promotion_ranks = FIRST_RANK | EIGHTH_RANK;

	for start in piece_of_color {
		// Single forward push
		let target = start << (8 * move_direction);
		let pin_limitation = pin_mask(pins, king_square, start);
		if !total_occupancy.is_occupied(target) && pin_limitation.is_occupied(target) {
			if promotion_ranks.is_occupied(target) {
				moves.push(Move::new(start, target, MoveFlags::knight_promotion()));
				moves.push(Move::new(start, target, MoveFlags::bishop_promotion()));
				moves.push(Move::new(start, target, MoveFlags::rook_promotion()));
				moves.push(Move::new(start, target, MoveFlags::queen_promotion()));
			} else {
				moves.push(Move::new(start, target, MoveFlags::quiet_move()));
			}
		}
	}

	// Diagonal attacks and en passant
	for start in piece_of_color & !A_FILE {
		let target = start << ((8 - move_direction) * move_direction);
		let pin_limitation = pin_mask(pins, king_square, start);
		if opponent_board.is_occupied(target) && pin_limitation.is_occupied(target) {
			if promotion_ranks.is_occupied(target) {
				moves.push(Move::new(
					start,
					target,
					MoveFlags::knight_promotion_capture(),
				));
				moves.push(Move::new(
					start,
					target,
					MoveFlags::bishop_promotion_capture(),
				));
				moves.push(Move::new(
					start,
					target,
					MoveFlags::rook_promotion_capture(),
				));
				moves.push(Move::new(
					start,
					target,
					MoveFlags::queen_promotion_capture(),
				));
			} else {
				moves.push(Move::new(start, target, MoveFlags::capture()));
			}
		} else if en_passant_square.map(|sq| sq == target).unwrap_or(false) {
			moves.push(Move::new(start, target, MoveFlags::en_passant()));
		}
	}

	for start in piece_of_color & !H_FILE {
		let target = start << ((8 + move_direction) * move_direction);
		let pin_limitation = pin_mask(pins, king_square, start);
		if opponent_board.is_occupied(target) && pin_limitation.is_occupied(target) {
			if promotion_ranks.is_occupied(target) {
				moves.push(Move::new(
					start,
					target,
					MoveFlags::knight_promotion_capture(),
				));
				moves.push(Move::new(
					start,
					target,
					MoveFlags::bishop_promotion_capture(),
				));
				moves.push(Move::new(
					start,
					target,
					MoveFlags::rook_promotion_capture(),
				));
				moves.push(Move::new(
					start,
					target,
					MoveFlags::queen_promotion_capture(),
				));
			} else {
				moves.push(Move::new(start, target, MoveFlags::capture()));
			}
		} else if en_passant_square.map(|sq| sq == target).unwrap_or(false) {
			moves.push(Move::new(start, target, MoveFlags::en_passant()));
		}
	}
}

fn pawn_double_moves(position: &Position, pins: Board, moves: &mut Vec<Move>) {
	let to_move_color = position.metadata.to_move();
	let board_piece =
		position.get_board_for_piece(Piece::Pawn) & STARTING_PAWNS[to_move_color as usize];
	let to_move_color_board = position.get_board_for_color(position.metadata.to_move());
	let total_occupancy = position.get_occupancy_board();
	let piece_of_color = board_piece & to_move_color_board;
	let king_square = Square::from(position.get_board_for_piece(Piece::King) & to_move_color_board);

	for start in piece_of_color {
		let pin_limitation = pin_mask(pins, king_square, start);
		let skip_square = start << (8 * MOVE_DIRECTION[to_move_color as usize]);
		let target = start << (16 * MOVE_DIRECTION[to_move_color as usize]);
		let required_empty = skip_square | target;
		if (total_occupancy & required_empty).is_empty() && pin_limitation.is_occupied(target) {
			moves.push(Move::new(start, target, MoveFlags::double_pawn_push()));
		}
	}
}

/// A handful of additional checks for move legality deferred to now. These conditions
/// don't apply to most moves, so it's better to defer them and check the conditions
/// later.
fn is_legal(position: &Position, checkers: Board, proposed_move: &Move) -> bool {
	let to_move_color = position.metadata.to_move();
	let moved_piece = position.piece_on(proposed_move.start).unwrap();

	let king_square = Square::from(
		position.get_board_for_color(to_move_color) & position.get_board_for_piece(Piece::King),
	);

	let num_checkers = checkers.count_ones();

	if let Some(side) = proposed_move.flags.castling_side() {
		let occupancy = position.get_occupancy_board();
		let rook_square = ROOKS[side as usize][to_move_color as usize];
		let attacked_squares = attacks::get_attacked_squares(position, !to_move_color);
		let king_path = between(proposed_move.start, proposed_move.target) | king_square;
		let between_rook_and_king = between(king_square, rook_square) ^ rook_square;
		// No attacks allowed on the square the king passes through when castling,
		// and the squares must be empty
		return (attacked_squares & king_path).is_empty()
			&& (occupancy & between_rook_and_king).is_empty();
	}

	if proposed_move.flags == MoveFlags::en_passant() {
		// Look for checks resulting from or broken by en passant.
		let next_position = position.apply_move(proposed_move);
		let our_king =
			position.get_board_for_color(to_move_color) & position.get_board_for_piece(Piece::King);
		let attacks_on_king =
			attacks::attackers_of_square(our_king.into(), !to_move_color, &next_position);
		return attacks_on_king.is_empty();
	}

	if moved_piece == Piece::King {
		let attacked_squares =
			attacks::get_attacked_squares(&position.apply_move(proposed_move), !to_move_color);
		return !attacked_squares.is_occupied(proposed_move.target);
	} else if num_checkers > 1 {
		// Can't make anything but a king move during double check
		return false;
	} else if num_checkers > 0 {
		let check_blocking_squares = between(king_square, checkers.into());
		// Either block the check or take the sole attacker
		return check_blocking_squares.is_occupied(proposed_move.target)
			|| checkers.is_occupied(proposed_move.target);
	}

	true
}

/// Get a mask of the squares where this piece can move given its current
/// pin state. This ensures that pieces that are absolutely pinned to the
/// king remain pinned.
fn pin_mask(pins: Board, king_square: Square, to_move_square: Square) -> Board {
	if pins.is_occupied(to_move_square) {
		ray(king_square, to_move_square) // Can only move along the direction of the attacker's pin
	} else {
		Board::full()
	}
}

/// Find all pieces that are pinned to our king. A piece is considered absolutely
/// pinned if moving it would create a discovered attack on the king; for example,
/// if a white bishop is the only piece between the white king a black rook, then
/// the white bishop is absolutely pinned -- moving it would let the rook attack
/// its king!
///
/// A piece might be pinned, but still have valid moves, like if a white rook is
/// blocking a black rook's attack on the white king -- the white rook can move
/// between its king and the black rook, even capturing it if it wants,
/// but it can't move off the rank or file of the enemy rook.
///
/// We calculate a board of pinned pieces by pretending that our king is an enemy
/// sliding piece, then checking the overlap of its attacks with the actual enemy
/// sliding pieces. If they overlap on only a single square, that means that they
/// both are attacking the same piece and hence that piece is the only thing preventing
/// an attack on our king. Thanks to the chess programming wiki for this very clever
/// algorithm!
/// https://www.chessprogramming.org/Checks_and_Pinned_Pieces_(Bitboards)#Opposite_Ray-Directions
///
fn pins(position: &Position) -> Board {
	let to_move_color = position.metadata.to_move();
	let king_board =
		position.get_board_for_color(to_move_color) & position.get_board_for_piece(Piece::King);
	let king_square = Square::from(king_board);

	let friendly_board = position.get_board_for_color(to_move_color);
	let enemy_board = position.get_board_for_color(!to_move_color);
	let occupancy = position.get_occupancy_board();

	let attacking_rqs =
		(
			// Attacks from rooks and queens
			position.get_board_for_piece(Piece::Rook) | position.get_board_for_piece(Piece::Queen)
		) & attacks::get_attacks(king_board, king_square, Piece::Rook, to_move_color)
			& enemy_board;

	let attacking_bqs =
		(
			// Attacks from bishops and queens
			position.get_board_for_piece(Piece::Bishop) | position.get_board_for_piece(Piece::Queen)
		) & attacks::get_attacks(king_board, king_square, Piece::Bishop, to_move_color)
			& enemy_board;

	let mut pin_board = Board::default();

	for square in attacking_rqs {
		let blockers = between(king_square, square) & occupancy ^ square;
		if blockers.count_ones() == 1 {
			// Both sides can block, but only our side is considered pinned
			pin_board |= blockers & friendly_board;
		}
	}

	for square in attacking_bqs {
		let blockers = between(king_square, square) & occupancy ^ square;
		if blockers.count_ones() == 1 {
			pin_board |= blockers & friendly_board;
		}
	}

	pin_board
}

/// For debugging. Calculate the total number of nodes at
/// a given depth, grouped by the move at the first depth.
pub fn divide(initial: Position, depth: u8) -> MoveDivision {
	let mut divided_moves = Vec::new();
	let tic = std::time::Instant::now();
	let moves = generate_moves::<false>(&initial);

	for m in moves {
		let mut total_positions = 1;
		let mut positions = vec![initial.apply_move(&m)];
		for _ in 1..depth {
			let moves: Vec<Vec<Move>> = positions
				.iter()
				.map(|pos| generate_moves::<false>(pos))
				.collect();

			positions = moves
				.iter()
				.zip(positions.iter())
				.flat_map(|(m, pos)| {
					return m
						.iter()
						.map(|m| pos.apply_move(m))
						.collect::<Vec<Position>>();
				})
				.collect();
			total_positions = positions.len();
		}
		divided_moves.push((m.to_string(), total_positions));
	}
	let toc = tic.elapsed();

	MoveDivision {
		elapsed: toc,
		moves: divided_moves,
	}
}

/// Small newtype to encapsulate display logic for [`divide`] output.
pub struct MoveDivision {
	moves: Vec<(String, usize)>,
	elapsed: std::time::Duration,
}

impl std::ops::Deref for MoveDivision {
	type Target = Vec<(String, usize)>;

	fn deref(&self) -> &Self::Target {
		&self.moves
	}
}

impl std::fmt::Display for MoveDivision {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for (move_string, node_count) in self.iter() {
			write!(f, "{move_string}: {node_count}\n")?;
		}
		let total_nodes = self.iter().map(|tup| tup.1.to_owned()).sum::<usize>();
		write!(
			f,
			"Moves: {}\nNodes: {}\nElapsed: {}ms ({:.3} nodes/sec)",
			self.moves.len(),
			total_nodes,
			self.elapsed.as_millis(),
			((total_nodes as f64) / (self.elapsed.as_millis() as f64)) * 1000f64
		)
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use pretty_assertions::assert_eq;

	#[test]
	pub fn rook_move_test() {
		let position = Position::from_fen("8/2k5/2p5/4b3/2R2PK1/8/2n5/8 w - - 0 1").unwrap();

		let rook_square = Square::from_algebraic_notation("c4");
		let expected_moves = vec![
			Move::new(
				rook_square,
				Square::from_algebraic_notation("c2"),
				MoveFlags::capture(),
			),
			Move::new(
				rook_square,
				Square::from_algebraic_notation("c3"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				rook_square,
				Square::from_algebraic_notation("a4"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				rook_square,
				Square::from_algebraic_notation("b4"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				rook_square,
				Square::from_algebraic_notation("d4"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				rook_square,
				Square::from_algebraic_notation("e4"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				rook_square,
				Square::from_algebraic_notation("c5"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				rook_square,
				Square::from_algebraic_notation("c6"),
				MoveFlags::capture(),
			),
		];

		let actual_moves: Vec<Move> = generate_moves::<false>(&position)
			.into_iter()
			.filter(|m| m.start == rook_square)
			.collect();

		assert_eq!(actual_moves, expected_moves);
	}

	#[test]
	pub fn knight_move_test() {
		let position = Position::from_fen("8/8/8/5p1k/8/6N1/4P3/5K2 w - - 0 1").unwrap();

		let knight_square = Square::from_algebraic_notation("g3");
		let expected_moves = vec![
			Move::new(
				knight_square,
				Square::from_algebraic_notation("h1"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				knight_square,
				Square::from_algebraic_notation("e4"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				knight_square,
				Square::from_algebraic_notation("f5"),
				MoveFlags::capture(),
			),
			Move::new(
				knight_square,
				Square::from_algebraic_notation("h5"),
				MoveFlags::capture(),
			),
		];

		let actual_moves: Vec<Move> = generate_moves::<false>(&position)
			.into_iter()
			.filter(|m| m.start == knight_square)
			.collect();

		assert_eq!(actual_moves, expected_moves);
	}

	#[test]
	pub fn bishop_move_test() {
		let position = Position::from_fen("2k5/pp6/8/8/4B3/8/2R5/1n4K1 w - - 0 1").unwrap();

		let bishop_square = Square::from_algebraic_notation("e4");
		let expected_moves = vec![
			Move::new(
				bishop_square,
				Square::from_algebraic_notation("h1"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				bishop_square,
				Square::from_algebraic_notation("g2"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				bishop_square,
				Square::from_algebraic_notation("d3"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				bishop_square,
				Square::from_algebraic_notation("f3"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				bishop_square,
				Square::from_algebraic_notation("d5"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				bishop_square,
				Square::from_algebraic_notation("f5"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				bishop_square,
				Square::from_algebraic_notation("c6"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				bishop_square,
				Square::from_algebraic_notation("g6"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				bishop_square,
				Square::from_algebraic_notation("b7"),
				MoveFlags::capture(),
			),
			Move::new(
				bishop_square,
				Square::from_algebraic_notation("h7"),
				MoveFlags::quiet_move(),
			),
		];

		let actual_moves: Vec<Move> = generate_moves::<false>(&position)
			.into_iter()
			.filter(|m| m.start == bishop_square)
			.collect();

		assert_eq!(actual_moves, expected_moves);
	}

	#[test]
	pub fn queen_move_test() {
		let position = Position::from_fen("2k5/pp6/4n3/1b4p1/8/4Q3/2R5/1n4K1 w - - 0 1").unwrap();

		let queen_square = Square::from_algebraic_notation("e3");
		let expected_moves = vec![
			Move::new(
				queen_square,
				Square::from_algebraic_notation("c1"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("e1"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("d2"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("e2"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("f2"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("a3"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("b3"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("c3"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("d3"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("f3"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("g3"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("h3"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("d4"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("e4"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("f4"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("c5"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("e5"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("g5"),
				MoveFlags::capture(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("b6"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("e6"),
				MoveFlags::capture(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("a7"),
				MoveFlags::capture(),
			),
		];

		let actual_moves: Vec<Move> = generate_moves::<false>(&position)
			.into_iter()
			.filter(|m| m.start == queen_square)
			.collect();

		assert_eq!(actual_moves, expected_moves);
	}

	#[test]
	pub fn king_move_test() {
		let position = Position::from_fen("2k5/pp6/4n3/2b3p1/8/4Q3/2R3pP/1n4K1 w - - 0 1").unwrap();

		let king_square = Square::from_algebraic_notation("g1");
		let expected_moves = vec![
			Move::new(
				king_square,
				Square::from_algebraic_notation("f2"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				king_square,
				Square::from_algebraic_notation("g2"),
				MoveFlags::capture(),
			),
		];

		let actual_moves: Vec<Move> = generate_moves::<false>(&position)
			.into_iter()
			.filter(|m| m.start == king_square)
			.collect();

		assert_eq!(actual_moves, expected_moves);
	}

	#[test]
	pub fn castling_test() {
		let position =
			Position::from_fen("2k5/pp6/4n3/6p1/5pb1/6P1/2R2P1P/1n2K2R w K - 0 1").unwrap();

		let king_square = Square::from_algebraic_notation("e1");
		let destination_square = Square::from_algebraic_notation("g1");

		let actual_move = generate_moves::<false>(&position)
			.into_iter()
			.find(|m| m.start == king_square && m.target == destination_square);

		assert!(actual_move.is_some());
		assert!(actual_move.unwrap().flags == MoveFlags::king_castling());

		let position =
			Position::from_fen("2k5/pp6/3nn3/6p1/5pb1/6P1/4PP1P/R3K2R w KQ - 0 1").unwrap();

		let destination_square = Square::from_algebraic_notation("c1");

		let actual_move = generate_moves::<false>(&position)
			.into_iter()
			.find(|m| m.start == king_square && m.target == destination_square);

		assert!(actual_move.is_some());
		assert!(actual_move.unwrap().flags == MoveFlags::queen_castling());
	}

	#[test]
	pub fn cannot_move_into_check_test() {
		let position =
			Position::from_fen("2k5/pp6/1b1nn3/6p1/5pb1/6P1/r6P/R3K2R w KQ - 0 1").unwrap();

		let king_square = Square::from_algebraic_notation("e1");
		let expected_moves = vec![Move::new(
			king_square,
			Square::from_algebraic_notation("f1"),
			MoveFlags::quiet_move(),
		)];

		let actual_moves: Vec<Move> = generate_moves::<false>(&position)
			.into_iter()
			.filter(|m| m.start == king_square)
			.collect();

		assert_eq!(actual_moves, expected_moves);
	}

	#[test]
	pub fn pin_test() {
		let position =
			Position::from_fen("2k5/pp6/3nn3/6p1/1b3pb1/6P1/r2Q3P/R3K2R w KQ - 0 1").unwrap();

		let queen_square = Square::from_algebraic_notation("d2");
		let expected_moves = vec![
			Move::new(
				queen_square,
				Square::from_algebraic_notation("c3"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				queen_square,
				Square::from_algebraic_notation("b4"),
				MoveFlags::capture(),
			),
		];

		let actual_moves: Vec<Move> = generate_moves::<false>(&position)
			.into_iter()
			.filter(|m| m.start == queen_square)
			.collect();

		assert_eq!(actual_moves, expected_moves);
	}

	#[test]
	pub fn en_passant_pin_test() {
		let position = Position::from_fen("3k4/8/8/KPp4r/8/8/8/8 w - c6 0 1").unwrap();

		let pawn_square = Square::from_algebraic_notation("b5");
		let expected_moves = vec![Move::new(
			pawn_square,
			Square::from_algebraic_notation("b6"),
			MoveFlags::quiet_move(),
		)];

		let actual_moves: Vec<Move> = generate_moves::<false>(&position)
			.into_iter()
			.filter(|m| m.start == pawn_square)
			.collect();
		assert_eq!(actual_moves, expected_moves);
	}

	#[test]
	pub fn pawn_single_move_test() {
		let position =
			Position::from_fen("2k5/pp6/4n1p1/7P/5pP1/1n6/5P1P/R3K2R b K g3 0 1").unwrap();

		let pawn_square = Square::from_algebraic_notation("g6");
		let expected_moves = vec![
			Move::new(
				pawn_square,
				Square::from_algebraic_notation("g5"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				pawn_square,
				Square::from_algebraic_notation("h5"),
				MoveFlags::capture(),
			),
		];

		let actual_moves: Vec<Move> = generate_moves::<false>(&position)
			.into_iter()
			.filter(|m| m.start == pawn_square)
			.collect();

		assert_eq!(actual_moves, expected_moves);
	}

	#[test]
	pub fn pawn_double_move_test() {
		let position =
			Position::from_fen("2k5/pp6/4n1p1/8/5pP1/1n6/5P1P/R3K2R b K g3 0 1").unwrap();

		let pawn_square = Square::from_algebraic_notation("b7");
		let expected_moves = vec![
			Move::new(
				pawn_square,
				Square::from_algebraic_notation("b6"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				pawn_square,
				Square::from_algebraic_notation("b5"),
				MoveFlags::double_pawn_push(),
			),
		];

		let actual_move: Vec<Move> = generate_moves::<false>(&position)
			.into_iter()
			.filter(|m| m.start == pawn_square)
			.collect();

		assert_eq!(actual_move, expected_moves);
	}

	#[test]
	pub fn pawn_en_passant_test() {
		let position =
			Position::from_fen("2k5/pp6/4n1p1/8/5pP1/1n6/5P1P/R3K2R b K g3 0 1").unwrap();

		let pawn_square = Square::from_algebraic_notation("f4");
		let expected_moves = vec![
			Move::new(
				pawn_square,
				Square::from_algebraic_notation("f3"),
				MoveFlags::quiet_move(),
			),
			Move::new(
				pawn_square,
				Square::from_algebraic_notation("g3"),
				MoveFlags::en_passant(),
			),
		];

		let actual_move: Vec<Move> = generate_moves::<false>(&position)
			.into_iter()
			.filter(|m| m.start == pawn_square)
			.collect();

		assert_eq!(actual_move, expected_moves);
	}

	#[test]
	pub fn pawn_promotion_test() {
		let position =
			Position::from_fen("2k3n1/pp5P/4n1p1/8/5pP1/1n6/5P1P/R3K2R w K - 0 1").unwrap();

		let pawn_square = Square::from_algebraic_notation("h7");
		let expected_moves = vec![
			Move::new(
				pawn_square,
				Square::from_algebraic_notation("h8"),
				MoveFlags::knight_promotion(),
			),
			Move::new(
				pawn_square,
				Square::from_algebraic_notation("h8"),
				MoveFlags::bishop_promotion(),
			),
			Move::new(
				pawn_square,
				Square::from_algebraic_notation("h8"),
				MoveFlags::rook_promotion(),
			),
			Move::new(
				pawn_square,
				Square::from_algebraic_notation("h8"),
				MoveFlags::queen_promotion(),
			),
			Move::new(
				pawn_square,
				Square::from_algebraic_notation("g8"),
				MoveFlags::knight_promotion_capture(),
			),
			Move::new(
				pawn_square,
				Square::from_algebraic_notation("g8"),
				MoveFlags::bishop_promotion_capture(),
			),
			Move::new(
				pawn_square,
				Square::from_algebraic_notation("g8"),
				MoveFlags::rook_promotion_capture(),
			),
			Move::new(
				pawn_square,
				Square::from_algebraic_notation("g8"),
				MoveFlags::queen_promotion_capture(),
			),
		];

		let actual_moves: Vec<Move> = generate_moves::<false>(&position)
			.into_iter()
			.filter(|m| m.start == pawn_square)
			.collect();

		assert_eq!(actual_moves, expected_moves);
	}
}

#[cfg(test)]
mod movegen_perft {

	use super::*;
	use pretty_assertions::assert_eq;

	fn perft_test(intial_position: Position, expected_nodes: &[usize]) {
		let mut positions = vec![intial_position];
		let tic = std::time::Instant::now();
		for expected in expected_nodes {
			positions = generate_next_positions(&positions);
			assert_eq!(positions.len(), *expected);
		}
		let toc = tic.elapsed().as_millis();

		println!(
			"Depth: {}: Found {} nodes in {}ms ({:.3} nodes/sec)",
			expected_nodes.len(),
			positions.len(),
			toc,
			(positions.len() as f64) / (toc as f64) * 1000f64
		);
	}

	fn generate_next_positions(positions: &Vec<Position>) -> Vec<Position> {
		let moves: Vec<Vec<Move>> = positions
			.iter()
			.map(|pos| generate_moves::<false>(pos))
			.collect();

		moves
			.iter()
			.zip(positions.iter())
			.flat_map(|(m, pos)| {
				return m
					.iter()
					.map(|m| pos.apply_move(m))
					.collect::<Vec<Position>>();
			})
			.collect()
	}

	#[test]
	pub fn from_starting() {
		perft_test(Position::default(), &[20, 400, 8902, 197281, 4865609]);
	}

	#[test]
	pub fn kiwipete() {
		let starting_position =
			Position::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")
				.unwrap();
		perft_test(starting_position, &[48, 2039, 97862, 4085603]);
	}

	#[test]
	pub fn kiwipete_two() {
		let starting_position =
			Position::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -").unwrap();
		perft_test(starting_position, &[14, 191, 2812, 43238, 674624]);
	}

	#[test]
	pub fn kiwipete_three() {
		let starting_position =
			Position::from_fen("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1 ")
				.unwrap();
		perft_test(starting_position, &[6, 264, 9467, 422333, 15833292]);
	}

	#[test]
	pub fn promotion_heavy() {
		let starting_position =
			Position::from_fen("n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1").unwrap();
		perft_test(starting_position, &[24, 496, 9483, 182838, 3605103]);
	}
}
