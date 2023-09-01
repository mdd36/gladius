use itertools::Itertools;
use std::{rc::Rc, str::SplitAsciiWhitespace};

use gladius_core::position::{moves::Move, Position};

// For the official spec, see
// https://gist.github.com/DOBRO/2592c6dad754ba67e6dcaec8c90165bf#file-uci-protocol-specification-txt-L41
pub enum UciCommand {
	UCI,
	Debug(bool),
	IsReady,
	SetOption {
		name: String,
		value: Option<String>,
	},
	Register,
	NewGame,
	Position(Position),
	Go {
		search_moves: Option<Rc<[Move]>>,
		wtime: Option<u32>,
		btime: Option<u32>,
		winc: Option<u32>,
		binc: Option<u32>,
		depth: Option<u8>,
		nodes: Option<u32>,
		move_time: Option<u32>,
		infinite: Option<bool>,
	},
	Stop,
	Quit,
	Help,
}

impl std::fmt::Debug for UciCommand {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::UCI => write!(f, "UCI"),
			Self::Debug(_) => write!(f, "Debug"),
			Self::IsReady => write!(f, "IsReady"),
			Self::SetOption { .. } => write!(f, "SetOption"),
			Self::Register => write!(f, "Register"),
			Self::NewGame => write!(f, "NewGame"),
			Self::Position { .. } => write!(f, "Position"),
			Self::Go { .. } => write!(f, "Go"),
			Self::Stop => write!(f, "Stop"),
			Self::Quit => write!(f, "Quit"),
			Self::Help => write!(f, "Help"),
		}
	}
}

pub fn parse_input(input: String) -> Result<UciCommand, &'static str> {
	let mut split = input.split_ascii_whitespace();

	// UCI technically allows garage before the actual command, so we may need to
	// step over some nonsense to get to the actual command
	while let Some(command) = split.next() {
		match command.to_lowercase().as_str() {
			"uci" => return Ok(UciCommand::UCI),
			"debug" => return Ok(UciCommand::Debug(matches!(split.next(), Some("on")))),
			"isready" => return Ok(UciCommand::IsReady),
			"ucinewgame" => return Ok(UciCommand::NewGame),
			"stop" => return Ok(UciCommand::Stop),
			"quit" => return Ok(UciCommand::Quit),
			"position" => return parse_position(&input, split),
			"register" => return Ok(UciCommand::Register),
			"help" => return Ok(UciCommand::Help),
			_ => continue,
		}
	}

	Err(r#"Unknown command. For help, enter "help"."#)
}

fn parse_position(
	input: &str,
	mut cmd: SplitAsciiWhitespace<'_>,
) -> Result<UciCommand, &'static str> {
	let mut position = None;
	let mut moves: Vec<&str> = Vec::new();

	while let Some(word) = cmd.next() {
		match word {
			"fen" => {
				// This is pretty gross, but basically to avoid allocating something on the heap
				// to stich the space separated input back together, we can look at the string
				// slices's address within the split, then subtract out the address of the input
				// string, and then use that information to get a slice containing only the FEN
				// information from the overall input.
				let start_address = cmd.next().ok_or("No fen string")?.as_ptr() as usize;

				let end_word = cmd
					.take_while_ref(|&w| w != "startpos" && w != "moves")
					.last()
					.ok_or("Invalid fen string")?;
				let end_address = end_word.as_ptr() as usize + end_word.len();

				let start = start_address - input.as_ptr() as usize;
				let end = end_address - input.as_ptr() as usize;

				let parsed_position =
					Position::from_fen(&input[start..end]).map_err(|_| "Failed to parse fen")?;
				position = Some(parsed_position);
			}
			"startpos" => position = Some(Position::default()),
			"moves" => {
				moves = cmd
					.take_while_ref(|&w| w != "fen" && w != "startpos")
					.collect();
			}
			_ => {}
		}
	}

	if let Some(starting_position) = position {
		let mut after_moves_position = starting_position;
		for m in moves {
			let to_apply = Move::from_uci_str(m, &after_moves_position);
			after_moves_position = after_moves_position.apply_move(&to_apply);
		}

		return Ok(UciCommand::Position(after_moves_position));
	}

	return Err("No position specified in command");
}
