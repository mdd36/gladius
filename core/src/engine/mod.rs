use crate::{
	position::{
		moves::{divide, Move, MoveDivision},
		Position,
	},
	search::{self, search, transposition::TranspositionTable, move_ordering::MoveOrderer},
};
use std::{
	sync::{
		atomic::{AtomicBool, Ordering},
		mpsc::Sender,
		Arc,
	},
	time::Duration,
};

pub struct SearchParameters {
	wtime: Option<u32>,
	btime: Option<u32>,
	winc: Option<u32>,
	binc: Option<u32>,
	depth: Option<u8>,
	move_time: Option<Duration>,
	infinite: Option<bool>,
}

pub trait Engine {
	fn setup_pos(&mut self, position: Position, move_history: Vec<Move>);

	fn new_game(&mut self);

	fn set_debug(&mut self, enabled: bool);

	fn display(&self) -> String;

	fn perft(&self, depth: u8);

	fn stop(&self);

	fn go(&self, parameters: SearchParameters);

	fn ready(&self);
}

/// Various flags and values that affect engine behavior.
pub struct EngineOpts {
	pub debug: bool,
	pub table_size: usize,
}

impl Default for EngineOpts {
	fn default() -> Self {
		Self {
			debug: false,
			table_size: 4 * 1024 * 1024, // 4MB
		}
	}
}

pub enum EngineMessage {
	BestMove(Move),
	Perft(MoveDivision),
	ReadyOk,
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
}

impl Engine for GladiusEngine {
	fn setup_pos(&mut self, starting_position: Position, move_history: Vec<Move>) {
		self.position_history.clear();
		self.position_history.push(starting_position.zobrist_hash);
		self.current_position = starting_position;

		for m in &move_history {
			self.current_position = self.current_position.apply_move(m);
			self.position_history.push(self.current_position.zobrist_hash);						
		}
	}

	fn new_game(&mut self) {
		self.setup_pos(Position::default(), Vec::new());
	}

	fn set_debug(&mut self, enabled: bool) {
		self.opts.debug = enabled;
	}

	fn display(&self) -> String {
		self.current_position.as_display_string()
	}

	fn perft(&self, depth: u8) {
		let cloned_position = self.current_position.clone();
		let tx = self.output_channel.clone();
		std::thread::spawn(move || {
			tx.send(EngineMessage::Perft(divide(cloned_position, depth)))
				.unwrap();
		});
	}

	fn stop(&self) {
		self.stop.store(true, Ordering::Relaxed);
	}

	fn go(&self, parameters: SearchParameters) {
		if !self.stop.load(Ordering::Relaxed) {
			self.output_channel.send(EngineMessage::Error(
				"A search is already running. Stop the current search before starting a new one"
					.to_owned(),
			)).expect("Engine message channel was prematurely closed!");
			return;
		}

		let starting_position = self.current_position.clone();
		let mut move_history = self.position_history.clone();
		let stop_flag = self.stop.clone();
		let search_tx = self.output_channel.clone();
		let mut transposition_table = self.transposition_table.clone();

		let depth_limit = parameters.depth.unwrap_or(std::u8::MAX);
		let infinite = parameters.infinite.unwrap_or(false);

		// Create a task to stop the search after the time limit has elapsed.
		// We'll only use 80% of the time as a safety margin to make sure we
		// have a response before the timeout.
		if let Some(time_limit) = parameters.move_time {
			if !infinite {
				let timeout_stop_flag = self.stop.clone();
				std::thread::spawn(move || {
					std::thread::sleep(time_limit.mul_f64(0.8));
					timeout_stop_flag.store(true, Ordering::Relaxed);
				});
			}
		}

		std::thread::spawn(move || {
			let mut depth = 1u8;
			let mut best_move = None;
			let mut move_orderer = MoveOrderer::new(depth_limit);

			while !stop_flag.load(Ordering::Relaxed) && (infinite || depth <= depth_limit) {
				let search_result = search::<false>(
					&starting_position,
					&mut move_history,
					&mut transposition_table,
					&mut move_orderer,
					search::SearchParameters::new(depth),
					stop_flag.clone(),
				);
				best_move = search_result.best_move;
				depth += 1;
			}

			let message = match best_move {
				Some(best_move) => EngineMessage::BestMove(best_move),
				None => EngineMessage::Error(
					"Unable to determine a best move for the current position".to_owned(),
				),
			};

			stop_flag.store(false, Ordering::Relaxed);
			search_tx
				.send(message)
				.expect("Engine messaging channel was prematurely closed!");
		});
	}

	fn ready(&self) {
		self.output_channel.send(EngineMessage::ReadyOk).unwrap();
	}
}
