use itertools::Itertools;
use std::{
	any::type_name,
	panic::catch_unwind,
	str::{FromStr, SplitAsciiWhitespace},
	time::Duration,
};

use gladius_core::{
	engine::{EngineOption, SearchParameters},
	position::{moves::Move, Position},
};

pub enum GoAction {
	Perft(u8),
	Search(SearchParameters),
}

// For the official spec, see
// https://gist.github.com/DOBRO/2592c6dad754ba67e6dcaec8c90165bf#file-uci-protocol-specification-txt-L41
pub enum UciCommand {
	Debug(bool),
	Display,
	Evaluate,
	Fen,
	Go(GoAction),
	Help,
	IsReady,
	NewGame,
	Position(Position, Vec<Move>),
	Quit,
	Register,
	SetOption(EngineOption),
	Stop,
	UCI,
}

impl std::fmt::Debug for UciCommand {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Debug(_) => write!(f, "Debug"),
			Self::Display => write!(f, "Display"),
			Self::Evaluate => write!(f, "Evaluate"),
			Self::Fen => write!(f, "Fen"),
			Self::Go { .. } => write!(f, "Go"),
			Self::Help => write!(f, "Help"),
			Self::IsReady => write!(f, "IsReady"),
			Self::NewGame => write!(f, "NewGame"),
			Self::Position { .. } => write!(f, "Position"),
			Self::Quit => write!(f, "Quit"),
			Self::Register => write!(f, "Register"),
			Self::SetOption { .. } => write!(f, "SetOption"),
			Self::Stop => write!(f, "Stop"),
			Self::UCI => write!(f, "UCI"),
		}
	}
}

pub fn parse_input(input: String) -> Result<UciCommand, String> {
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
			"d" | "display" => return Ok(UciCommand::Display),
			"go" => return parse_go(split),
			"setoption" => return parse_setopt(split),
			"evaluate" => return Ok(UciCommand::Evaluate),
			"fen" => return Ok(UciCommand::Fen),
			_ => continue,
		}
	}

	Err(r#"unknown command. For help, enter "help""#.to_owned())
}

fn parse_position(input: &str, mut cmd: SplitAsciiWhitespace<'_>) -> Result<UciCommand, String> {
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
		let mut after_moves_position = starting_position.clone();
		let mut parsed_moves = Vec::new();
		for m in moves {
			// TODO maybe this should be handled by the engine
			let to_apply = match catch_unwind(|| Move::from_uci_str(m, &after_moves_position)) {
				Ok(m) => m,
				Err(e) => return Err(format!("failed to parse move {e:?}")),
			};
			after_moves_position = after_moves_position.apply_move(&to_apply);
			parsed_moves.push(to_apply);
		}

		return Ok(UciCommand::Position(starting_position, parsed_moves));
	}

	return Err("No position specified in command".to_owned());
}

fn parse_go(mut cmd: SplitAsciiWhitespace<'_>) -> Result<UciCommand, String> {
	let mut params = SearchParameters::default();

	while let Some(parameter_name) = cmd.next() {
		match parameter_name {
			"perft" => return parse_perf(cmd),
			"wtime" => params.wtime = Some(parse(cmd.next())?),
			"btime" => params.btime = Some(parse(cmd.next())?),
			"winc" => params.winc = Some(parse(cmd.next())?),
			"binc" => params.binc = Some(parse(cmd.next())?),
			"depth" => params.depth = Some(parse(cmd.next())?),
			"movetime" => params.move_time = Some(Duration::from_millis(parse(cmd.next())?)),
			"infinite" => params.infinite = true,
			_ => {}
		}
	}

	Ok(UciCommand::Go(GoAction::Search(params)))
}

fn parse_perf(mut cmd: SplitAsciiWhitespace<'_>) -> Result<UciCommand, String> {
	match cmd.next() {
		Some("depth") => {}
		Some(other) => {
			return Err(format!(r#"expecting "depth", but found {other}"#));
		}
		None => return Err(r#"expected "depth", but it was missing"#.to_owned()),
	}
	let depth = match cmd.next() {
		Some(d) => d,
		None => {
			return Err(r#"missing parameter "depth""#.to_owned());
		}
	};

	let depth = match depth.parse() {
		Ok(d) => d,
		Err(e) => {
			return Err(format!("could not parse {depth} to a number: {e}"));
		}
	};

	Ok(UciCommand::Go(GoAction::Perft(depth)))
}

fn parse_setopt(mut cmd: SplitAsciiWhitespace<'_>) -> Result<UciCommand, String> {
	let mut name = None;
	let mut value = None;
	while let Some(word) = cmd.next() {
		match word {
			"name" => {
				name = Some(cmd.take_while_ref(|&w| w != "value").fold(
					String::new(),
					|mut acc, s| {
						acc.push_str(s);
						acc
					},
				))
			}
			"value" => value = cmd.next(),
			_ => {}
		}
	}

	let (name, value) = match (name, value) {
		(Some(n), Some(v)) => (n, v),
		_ => return Err("missing name or value".to_owned()),
	};

	let option = match name.as_ref() {
		"UCI_AnalyseMode" | "UCI_AnalyzeMode" => {
			EngineOption::AnalyzeMode(value.eq_ignore_ascii_case("true"))
		}
		"Hash" => {
			let size = value
				.parse()
				.map_err(|_| "specified table size isn't an integer")?;
			EngineOption::TableSize(size)
		}
		"MoveOverhead" => {
			let millis = value
				.parse()
				.map_err(|_| "specified overhead isn't an integer")?;
			EngineOption::MoveOverhead(Duration::from_millis(millis))
		}
		"Threads" => {
			let max_thread_count = value
				.parse()
				.map_err(|_| "specified thread count isn't supported")?;
			EngineOption::Threads(max_thread_count)
		}
		opt => return Err(format!("unsupported option: {opt}")),
	};

	Ok(UciCommand::SetOption(option))
}

fn parse<T: FromStr>(value: Option<&str>) -> Result<T, String> {
	value.map(|v| v.parse().ok()).flatten().ok_or(format!(
		"failed to parse {} from input {:?} ",
		type_name::<T>(),
		value
	))
}
