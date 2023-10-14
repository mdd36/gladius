mod parser;

use std::{
	process::exit,
	sync::mpsc::{channel, Receiver, TryRecvError},
};

use gladius_core::{
	engine::{AnalysisData, Engine, EngineMessage, EngineOption, EngineOpts, GladiusEngine},
	eval::Evaluation,
};
use parser::{parse_input, UciCommand};
use rustyline::{error::ReadlineError, Config, DefaultEditor};

use crate::parser::GoAction;

const CLI_BANNER: &str = r#"
 _____ _           _ _           
|  __ \ |         | (_)          
| |  \/ | __ _  __| |_ _   _ ___ 
| | __| |/ _` |/ _` | | | | / __|
| |_\ \ | (_| | (_| | | |_| \__ \
 \____/_|\__,_|\__,_|_|\__,_|___/

      /| ________________
O|===|* >________________>
      \|

© 2023 Matthew Dickson
"#;

const HELP_DIALOG: &str = r#"
Supported Universal Chess Interface (UCI) Commands

uci
    Tell the engine to start in UCI mode. The engine will respond with
		some identifying information and the list of supported options.
debug [ on | off ]
    Switch debug mode on or off. In debug mode, the engine will forward
    additional information to the front end to help with debugging.
isready
    Synchronizes the front end with the engine. If the engine appears to be
    hanging, the front end can send this command again to wait until the 
    engine is caught up and ready to start processing new commands.
setoption name <id> [value <x>]
    Change an internal parameter in the engine. If the value is simply a flag,
    the value can be omitted and the engine will toggle the current state.
register [ later | name <x> code <y> ]
    Since this engine doesn't require registration, this is a noop.
ucinewgame
    Tell the engine to start a new game.
position [ fen <fenstring> | startpos ] moves <move 1> ... <move n>
    Set up an initial position and apply all the provided moves. The initial
    position can come from either a FEN string or can default to the normal
    chess start using "startpos". The moves are optional, and are formed in
    algebraic notation for the start square and the end square. For example,
    the move of a pawn from e2 to e4 is represented as "e2e4".
display
		Show the current position represented as a chess board.
go perft [depth <x>]
		Run a perft test on the current position to the specified depth.
go [ wtime <x> ] [ btime <x> ] [ winc <x> ] [ binc <x> ] [ depth <x> | nodes <x> | movetime <x> | infinite ]
		Start calculating what the best next move is. wtime specifies
		the number of milliseconds left on white's clock, and btime is the same for
		black. winc specifies the increment per move for white, and binc is the same
		for black. depth tells the engine number of half moves into the future to
    search. nodes will limit the search to exactly the provided number of
    nodes. movetime will limit the engine's search to the provided number of
    milliseconds. infinite will let the engine search indefinitely until it
    receives a stop command. 
stop
    Stop calculating as soon as possible. Will provide the best move determined
    by the engine.
ponderhit
    Not supported by the engine.
evaluate
		Display the engine's current evaluation of ths position. The evaluation is
		always from the perspective of the player to move, not necessarily the side
		that the engine is playing.
quit
    Exit the program.
"#;

fn main() {
	println!("{CLI_BANNER}");
	let stdin_rx = stdin_channel();
	let (engine_tx, engine_rx) = channel();
	let mut engine = GladiusEngine::new(EngineOpts::default(), engine_tx);

	loop {
		match engine_rx.try_recv() {
			Ok(EngineMessage::ReadyOk) => println!("readyok"),
			Ok(EngineMessage::Perft(move_division)) => println!("{move_division}"),
			Ok(EngineMessage::BestMove(bm)) => println!("bestmove {bm}"),
			Ok(EngineMessage::Info(msg)) => println!("info string {msg}"),
			Ok(EngineMessage::Error(msg)) => eprintln!("error {msg}"),
			Ok(EngineMessage::Evaluation(eval)) => println!("info {}", eval.to_uci_string()),
			Ok(EngineMessage::AnalysisData(data)) => {
				println!("info string {}", data.to_uci_string())
			}
			Err(_) => {}
		};

		let line = match stdin_rx.try_recv() {
			Ok(line) => line,
			Err(TryRecvError::Empty) => {
				std::thread::sleep(std::time::Duration::from_millis(200));
				continue;
			}
			Err(_) => return, // EOF sent to stdin
		};

		let result = match parse_input(line) {
			Ok(cmd) => cmd,
			Err(str) => {
				eprintln!("error {str}");
				continue;
			}
		};

		match result {
			UciCommand::UCI => {
				println!("id name Gladius");
				println!("id author Matthew Dickson");

				EngineOption::iter()
					.for_each(|option| println!("option {}", option.to_uci_string()));

				println!("uciok");
			}
			UciCommand::NewGame => {
				engine.stop();
				engine.new_game();
			}
			UciCommand::Position(start_position, moves) => {
				engine.stop();
				engine.setup_pos(start_position, moves);
			}
			UciCommand::Display => {
				println!("{}", engine.display());
			}
			UciCommand::Debug(debug) => {
				engine.set_opt(EngineOption::Debug(debug));
			}
			UciCommand::Fen => {
				println!("fen {}", engine.show_fen());
			}
			UciCommand::IsReady => {
				engine.ready();
			}
			UciCommand::SetOption(option) => {
				engine.set_opt(option);
			}
			UciCommand::Go(GoAction::Perft(depth)) => {
				engine.perft(depth);
			}
			UciCommand::Go(parser::GoAction::Search(parameters)) => {
				engine.go(parameters);
			}
			UciCommand::Stop => {
				engine.stop();
			}
			UciCommand::Register => println!("registration ok"),
			UciCommand::Help => println!("{HELP_DIALOG}"),
			UciCommand::Quit => exit(0),
			UciCommand::Evaluate => engine.evaluate(),
		}
	}
}

fn stdin_channel() -> Receiver<String> {
	let (tx, rx) = channel();
	let config = Config::builder()
		.auto_add_history(true)
		.edit_mode(rustyline::EditMode::Vi)
		.build();
	let mut line_reader =
		DefaultEditor::with_config(config).expect("Failed to initialize command interface.");
	std::thread::spawn(move || loop {
		let line = match line_reader.readline("") {
			Ok(line) => line,
			Err(ReadlineError::Interrupted | ReadlineError::Eof) => exit(0),
			Err(_) => continue,
		};
		tx.send(line).unwrap();
	});
	rx
}

trait ToUciString {
	fn to_uci_string(&self) -> String;
}

impl ToUciString for AnalysisData {
	fn to_uci_string(&self) -> String {
		format!(
			"score cp {:.1} depth {} time {} pv {} nodes {} nps {:.2}",
			self.score as f64,
			self.depth,
			self.time.as_millis(),
			self.best_move,
			self.nodes,
			(self.nodes as f64 / self.time.as_millis() as f64) * 1000.0
		)
	}
}

impl ToUciString for EngineOption {
	fn to_uci_string(&self) -> String {
		match self {
			Self::AnalyzeMode(_) => "name UCI_AnalyseMode type check default false",
			Self::TableSize(_) => "name Hash type spin min 0 max 512 default 4",
			Self::MoveOverhead(_) => "name MoveOverhead type spin min 0 default 0",
			Self::Threads(_) => "name Threads type spin min 1 max 4 default 4",
			Self::Debug(_) => "name Debug type check default false",
		}
		.to_owned()
	}
}

impl ToUciString for Evaluation {
	fn to_uci_string(&self) -> String {
		format!(
			"total cp {} material cp {} location cp {} pawns cp {} king safety cp {}",
			self.total,
			self.material,
			self.material_location,
			self.pawn_structure,
			self.king_safety,
		)
	}
}
