use crate::{
	eval::{evaluate_position_verbose, Evaluation, MIN_SCORE},
	position::{
		moves::{divide, Move, MoveDivision},
		Color, Position,
	},
	search::{self, move_ordering::KillerTable, search, transposition::TranspositionTable},
};
use std::{
	panic::catch_unwind,
	sync::{
		atomic::{AtomicBool, Ordering},
		mpsc::Sender,
		Arc,
	},
	time::Duration,
};

// Taken from https://chess.stackexchange.com/questions/2506/what-is-the-average-length-of-a-game-of-chess,
// plus a little buffer
const AVERAGE_MOVES_PER_GAME: u64 = 40;
// The engine will never assume that there's fewer than this many
// moves left when determining its time controls.
const MIN_MOVES_REMAINING: u64 = 5;

#[derive(Debug, Default)]
pub struct SearchParameters {
	pub wtime: Option<u32>,
	pub btime: Option<u32>,
	pub winc: Option<u32>,
	pub binc: Option<u32>,
	pub depth: Option<u8>,
	pub move_time: Option<Duration>,
	pub infinite: bool,
}

pub trait Engine {
	fn setup_pos(&mut self, position: Position, move_history: Vec<Move>);

	fn new_game(&mut self);

	fn display(&self) -> String;

	fn perft(&self, depth: u8);

	fn stop(&self);

	fn go(&self, parameters: SearchParameters);

	fn ready(&self);

	fn set_opt(&mut self, option: EngineOption);

	fn evaluate(&self);
}

#[derive(Debug)]
pub enum EngineOption {
	Debug(bool),
	TableSize(usize),
	AnalyzeMode(bool),
	MoveOverhead(Duration),
	Threads(u8),
	MaxMoveTime(Duration),
}

impl EngineOption {
	pub fn iter() -> impl Iterator<Item = Self> {
		[
			Self::TableSize(4),
			Self::AnalyzeMode(false),
			Self::MoveOverhead(Duration::ZERO),
			Self::Threads(4),
			Self::MaxMoveTime(Duration::MAX),
		]
		.into_iter()
	}
}

/// Various flags and values that affect engine behavior.
pub struct EngineOpts {
	pub debug: bool,
	pub table_size: usize,
	pub analyze_mode: bool,
	pub move_overhead: Duration,
	pub threads: u8,
	pub max_move_time: Duration,
}

impl Default for EngineOpts {
	fn default() -> Self {
		Self {
			debug: false,
			table_size: 4, // MB
			analyze_mode: false,
			threads: 4,
			move_overhead: Duration::ZERO,
			max_move_time: Duration::from_secs(5),
		}
	}
}

pub struct AnalysisData {
	pub score: i16,
	pub depth: u8,
	pub time: Duration,
	pub best_move: Move,
	pub nodes: u64,
}

pub enum EngineMessage {
	AnalysisData(AnalysisData),
	BestMove(Move),
	Evaluation(Evaluation),
	Perft(MoveDivision),
	ReadyOk,
	Info(String),
	Error(String),
}

pub struct GladiusEngine {
	opts: EngineOpts,
	current_position: Position,
	position_history: Vec<u64>,
	output_channel: Sender<EngineMessage>,
	transposition_table: TranspositionTable,
	stop: Arc<AtomicBool>,
}

impl GladiusEngine {
	pub fn new(opts: EngineOpts, output_channel: Sender<EngineMessage>) -> Self {
		GladiusEngine {
			current_position: Position::default(),
			position_history: Vec::new(),
			transposition_table: TranspositionTable::new(opts.table_size),
			stop: Arc::new(AtomicBool::default()),
			opts,
			output_channel,
		}
	}

	fn send_info_message<T: ToString>(&self, message: T) {
		self.output_channel
			.send(EngineMessage::Info(message.to_string()))
			.expect("Output channel unexpected closed!")
	}

	fn send_error_message<T: ToString>(&self, message: T) {
		self.output_channel
			.send(EngineMessage::Error(message.to_string()))
			.expect("Output channel unexpected closed!")
	}

	fn time_limit(&self, search_options: &SearchParameters) -> Duration {
		let time_remaining = match self.current_position.metadata.to_move() {
			Color::Black => search_options.btime,
			Color::White => search_options.wtime,
		};

		time_remaining.map_or(self.opts.max_move_time, |total_time_remaining_millis| {
			let expected_moves_remaining = AVERAGE_MOVES_PER_GAME
				.saturating_sub(self.current_position.full_move_clock as u64)
				.max(MIN_MOVES_REMAINING);
			let millis_for_move = total_time_remaining_millis as u64 / expected_moves_remaining;
			Duration::from_millis(millis_for_move)
				.saturating_sub(self.opts.move_overhead)
				.min(self.opts.max_move_time)
		})
	}

	pub fn show_fen(&self) -> String {
		self.current_position.to_fen()
	}
}

impl Engine for GladiusEngine {
	fn setup_pos(&mut self, starting_position: Position, move_history: Vec<Move>) {
		if self.opts.debug {
			self.send_info_message("set position ack")
		}

		let original_position = self.current_position.to_owned();
		let original_position_history = self.position_history.clone();

		self.position_history.clear();
		self.position_history.push(starting_position.zobrist_hash);
		self.current_position = starting_position;

		for m in &move_history {
			self.current_position = match catch_unwind(|| self.current_position.apply_move(m)) {
				Ok(position) => position,
				Err(e) => {
					self.send_error_message(format!("failed to update the position: {e:?}"));
					self.position_history = original_position_history;
					self.current_position = original_position;
					return;
				}
			};
			self.position_history
				.push(self.current_position.zobrist_hash);
		}
	}

	fn new_game(&mut self) {
		if self.opts.debug {
			self.send_info_message("new game ack")
		}
		self.setup_pos(Position::default(), Vec::new());
	}

	fn display(&self) -> String {
		self.current_position.as_display_string()
	}

	fn perft(&self, depth: u8) {
		if self.opts.debug {
			self.send_info_message(format!("perft ack depth {depth}"));
		}
		let cloned_position = self.current_position.clone();
		let tx = self.output_channel.clone();
		std::thread::spawn(move || {
			tx.send(EngineMessage::Perft(divide(cloned_position, depth)))
				.unwrap();
		});
	}

	fn stop(&self) {
		if self.opts.debug {
			self.send_info_message("stop ack")
		}
		self.stop.store(true, Ordering::Relaxed);
	}

	fn go(&self, parameters: SearchParameters) {
		if self.opts.debug {
			self.send_info_message(format!("go ack parameters {parameters:?}"))
		}
		if !self.stop.load(Ordering::Relaxed) {
			self.send_error_message(
				"a search is already running. Stop the current search before starting a new one",
			);
			return;
		}
		self.stop.store(false, Ordering::Relaxed);

		let starting_position = self.current_position.clone();
		let mut move_history = self.position_history.clone();
		let stop_flag = self.stop.clone();
		let search_tx = self.output_channel.clone();
		let mut transposition_table = self.transposition_table.clone();
		let analyze_mode = self.opts.analyze_mode;
		let debug = self.opts.debug;

		let depth_limit = parameters.depth.unwrap_or(std::u8::MAX);
		let infinite = parameters.infinite;
		let mut killers_table = KillerTable::default();

		let time_limit = match parameters.move_time {
			Some(time) => time,
			None => self.time_limit(&parameters),
		};

		// Create a task to stop the search after the time limit has elapsed.
		// We'll only use 80% of the time as a safety margin to make sure we
		// have a response before the timeout.
		if !infinite {
			let timeout_stop_flag = self.stop.clone();
			let timeout_tx = self.output_channel.clone();
			std::thread::spawn(move || {
				let tic = std::time::Instant::now();
				std::thread::sleep(time_limit);
				if debug {
					let elapsed = tic.elapsed();
					timeout_tx
						.send(EngineMessage::Info(format!("search timeout {elapsed:?}")))
						.expect("Engine messaging channel was prematurely closed!");
				}
				timeout_stop_flag.store(true, Ordering::Relaxed);
			});
		}

		std::thread::spawn(move || {
			let mut depth = 1u8;
			let mut best_move = None;
			let mut current_score = MIN_SCORE;

			while !stop_flag.load(Ordering::Relaxed) && (infinite || depth <= depth_limit) {
				if analyze_mode {
					search_tx
						.send(EngineMessage::Info(format!("depth {depth}")))
						.expect("Engine messaging channel was prematurely closed!");
				}

				let tic = std::time::Instant::now();
				let search_result = search(
					&starting_position,
					&mut move_history,
					&mut transposition_table,
					&mut killers_table,
					search::SearchParameters::new_with_alpha(depth, current_score),
					stop_flag.clone(),
				);

				best_move = search_result.best_move.or(best_move);

				if analyze_mode {
					search_tx
						.send(EngineMessage::AnalysisData(AnalysisData {
							score: search_result.score,
							depth,
							time: tic.elapsed(),
							best_move: best_move.unwrap_or(Move::null_move()),
							nodes: search_result.nodes_explored,
						}))
						.expect("Engine messaging channel was prematurely closed!");
				}

				current_score = search_result.score;
				depth += 1;
			}

			let message = match best_move {
				Some(best_move) => EngineMessage::BestMove(best_move),
				None => EngineMessage::Error(
					"Unable to determine a best move for the current position".to_owned(),
				),
			};

			stop_flag.store(true, Ordering::Relaxed);
			search_tx
				.send(message)
				.expect("Engine messaging channel was prematurely closed!");
		});
	}

	fn ready(&self) {
		if self.opts.debug {
			self.send_info_message("ready check ack")
		}
		self.output_channel.send(EngineMessage::ReadyOk).unwrap();
	}

	fn set_opt(&mut self, option: EngineOption) {
		if self.opts.debug {
			self.send_info_message(format!("set option ack option {option:?}"));
		}

		match option {
			EngineOption::Debug(is_enabled) => self.opts.debug = is_enabled,
			EngineOption::TableSize(size) => {
				self.opts.table_size = size;
				self.transposition_table.resize(size);
			}
			EngineOption::AnalyzeMode(is_enabled) => self.opts.analyze_mode = is_enabled,
			EngineOption::MoveOverhead(overhead) => self.opts.move_overhead = overhead,
			EngineOption::Threads(count) => self.opts.threads = count,
			EngineOption::MaxMoveTime(move_time) => self.opts.max_move_time = move_time,
		}
	}

	fn evaluate(&self) {
		self.output_channel
			.send(EngineMessage::Evaluation(evaluate_position_verbose(
				&self.current_position,
			)))
			.expect("Engine output channel closed unexpectedly!");
	}
}
