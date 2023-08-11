use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, ShlAssign};

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

impl std::fmt::Debug for Square {
    /// Prints the position in algebraic notation for simplicity.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_algebraic_notation())
    }
}

impl Square {
    /// Convert the algebraic notation for a single square into a [`Position`].
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
        let mut bit_position: u64 = 1;
        // Will be ASCII so we can just read them as bytes
        let file = position.as_bytes()[0] - ('a' as u8);
        let rank = position.as_bytes()[1] - ('1' as u8);

        bit_position = bit_position << file;
        bit_position = bit_position << (8 * rank);

        Self(bit_position)
    }

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
        let mut pos = self.0;
        let mut rank = 1;
        while pos >= 256 {
            pos = pos >> 8;
            rank += 1;
        }

        let mut file: u8 = 0;
        while pos > 1 {
            pos = pos >> 1;
            file += 1;
        }

        let file_char = (file + ('a' as u8)) as char;
        format!("{file_char}{rank}")
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

        for rank in 0u8..8 {
            let file_char = ((8 - rank) + ('0' as u8)) as char;
            board.push(file_char);
            board.push(' ');
            let rank = 8 * (8 - rank - 1);
            for file in 0u8..8 {
                let pos = (1u64 << file) << rank;
                if pos == self.0 {
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

impl BitOr for Board {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
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

impl Board {
    pub fn as_u64(&self) -> u64 {
        self.0
    }
}

pub enum Color {
    White,
    Black,
}

pub enum Piece {
    Pawn = 2,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
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
///   one possible castle move, in order: white's king side, white's
///   queen side, black's king side, black's queen side.
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
pub struct PositionMetadata(u16);

const EN_PASSANT_MASK: u16 = 0b0111100000000000;
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

    /// Raises the half move counter by 1
    pub fn increment_half_move(&mut self) {
        *self = Self(self.0 + 1)
    }
}

pub struct Position {
    pub boards: [Board; 8],
    pub metadata: PositionMetadata,
}

impl Default for Position {
    /// Initializes the position to the default starting position for
    /// chess games.
    fn default() -> Self {
        Self {
            boards: [
                Board::from(0x000000000000ffff), // White
                Board::from(0xffff000000000000), // Black
                Board::from(0x00ff00000000ff00), // Pawns
                Board::from(0x8100000000000081), // Rooks
                Board::from(0x4200000000000042), // Knights
                Board::from(0x2400000000000024), // Bishops
                Board::from(0x1000000000000010), // Queens
                Board::from(0x0800000000000008), // Kings
            ],
            metadata: PositionMetadata::default(),
        }
    }
}

impl Position {
    pub fn blank() -> Self {
        Self {
            boards: Default::default(),
            metadata: PositionMetadata::blank(),
        }
    }

    /// Initialize the board from a given [FEN] string. A standard chess
    /// start is needed, prefer [`Position::default()`] which uses hard
    /// coded values and hence is faster than parsing the string.
    ///
    /// Will fail if the provided string isn't valid FEN.
    ///
    /// [FEN]: https://en.wikipedia.org/wiki/Forsyth–Edwards_Notation
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

            let square = (1u64 << 7 - file) << (rank * 8);

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
                'K' => position.metadata |= 0x0200,
                'Q' => position.metadata |= 0x0100,
                'k' => position.metadata |= 0x0080,
                'q' => position.metadata |= 0x0040,
                _ => return Err(()),
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
        let moves: u16 = fen_components.next().ok_or(())?.parse().unwrap();
        position.metadata.0 += moves;

        Ok(position)
    }

    pub fn get_board_for_color(&self, color: Color) -> Board {
        self.boards[color as usize]
    }

    pub fn get_board_for_piece(&self, piece: Piece) -> Board {
        self.boards[piece as usize]
    }

    /// Print the board to the console.
    ///
    /// ```
    /// use gladius_core::position::Position;
    ///
    /// Position::default().print_board();
    /// ```
    pub fn print_board(&self) {
        #[allow(unused_results)]
        // Assuming worst case of 4 bytes per unicode char for the chess
        // pieces, plus a space between each. Letting the overestimation
        // of the required UTF-8 bytes buffer things like linebreaks
        let mut board_string = String::with_capacity(4 * 64 * 2);

        let combined_board =
            self.get_board_for_color(Color::White) | self.get_board_for_color(Color::Black);

        let mut sweep = 0x8000000000000000u64;

        for rank in 0u8..8 {
            for file in 0u8..8 {
                if combined_board & sweep == 0 {
                    if (rank + file) % 2 == 0 {
                        board_string.push_str("▢ ");
                    } else {
                        board_string.push_str("▧ ");
                    }
                    sweep >>= 1;
                    continue;
                }

                // Unicode offset for the black pieces is 6 since there's six unique
                // chess pieces and white comes first
                let color_offset = if self.get_board_for_color(Color::White) & sweep > 0 {
                    0
                } else {
                    6
                };

                let piece_offset = if self.get_board_for_piece(Piece::King) & sweep > 0 {
                    0
                } else if self.get_board_for_piece(Piece::Queen) & sweep > 0 {
                    1
                } else if self.get_board_for_piece(Piece::Rook) & sweep > 0 {
                    2
                } else if self.get_board_for_piece(Piece::Bishop) & sweep > 0 {
                    3
                } else if self.get_board_for_piece(Piece::Knight) & sweep > 0 {
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
                sweep >>= 1;
            }
            board_string.push('\n');
        }

        println!("{board_string}");
    }
}

#[derive(Debug)]
pub struct Move {
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
        Self {
            start: Square::from_algebraic_notion(&value[..2]),
            target: Square::from_algebraic_notion(&value[2..]),
        }
    }
}
