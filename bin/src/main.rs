use gladius_core::uci::{model::UciCommand, parse_input};
use rustyline::error::ReadlineError;

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
    Tell the engine to start in UCI mode
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
    Since this engine doesn't require registration, this is a noop. The command
    is supported to align with the entire UCI interface.
ucinewgame
    Tell the engine to start a new game.
position [ fen <fenstring> | startpos ] moves <move 1> ... <move n>
    Set up an initial position and apply all the provided moves. The initial
    position can come from either a FEN string or can default to the normal
    chess start using "startpos". The moves are optional, and are formed in
    algebraic notation for the start square and the end square. For example,
    the move of a pawn from e2 to e4 is represented as "e2e4".
go [ wtime <x> ] [ btime <x> ] [ winc <x> ] [ binc <x> ] 
      [ depth <x> | nodes <x> | movetime <x> | infinite ]
    Start calculating what the best next move is. wtime specifies the number
    of milliseconds left on white's clock, and btime is the same for black.
    winc specifies the increment per move for white, and binc is the same for
    black. depth tells the engine number of half moves into the future to
    search. nodes will limit the search to exactly the provided number of
    nodes. movetime will limit the engine's search to the provided number of
    milliseconds. infinite will let the engine search indefinitely until it
    receives a stop command. 
stop
    Stop calculating as soon as possible. Will provide the best move determined
    by the engine.
ponderhit
    Not supported by the engine.
quit
    Exit the program.
"#;

fn main() {
    println!("{CLI_BANNER}");
    uci_loop()
}

pub fn uci_loop() {
    let mut line_reader =
        rustyline::DefaultEditor::new().expect("Failed to initialize command interface.");
    loop {
        let line = match line_reader.readline("> ") {
            Ok(line) => {
                line_reader.add_history_entry(&line).unwrap();
                line
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => return,
            Err(_) => continue,
        };
        let result = match parse_input(line) {
            Ok(cmd) => cmd,
            Err(str) => {
                println!("{str}");
                continue;
            }
        };

        match result {
            UciCommand::UCI => {
                println!("id name Gladius");
                println!("id author Matthew Dickson");
                println!("uciok");
            }
            UciCommand::Position {
                start_position,
                moves,
            } => {
                start_position.print_board();
                println!("{:?}", &start_position.metadata);
                println!("{moves:?}");
            }
            UciCommand::Register => println!("registration ok"),
            UciCommand::Help => println!("{HELP_DIALOG}"),
            UciCommand::Quit => return,
            cmd => println!("{cmd:?}"),
        }
    }
}
