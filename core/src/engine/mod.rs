use crate::position::{Position, moves::{MoveDivision, divide, Move}};
use std::sync::{mpsc::{Sender, Receiver, channel}, RwLock, Arc, atomic::AtomicBool};

pub struct SearchParameters {
	search_moves: Option<Vec<Move>>,
	wtime: Option<u32>,
	btime: Option<u32>,
	winc: Option<u32>,
	binc: Option<u32>,
	depth: Option<u8>,
	nodes: Option<u32>,
	move_time: Option<u32>,
	infinite: Option<bool>,
}

pub trait Engine {
	fn setup_pos(&mut self, position: Position);

	fn best_move(&self);

	fn new_game(&mut self);

	fn set_debug(&mut self, enabled: bool);

	fn display(&self) -> String;

	fn perft(&self, depth: u8);

	fn stop(&self);

	fn go(&self, parameters: SearchParameters);

	fn ready(&self);
}

/// Various flags and values that affect engine behavior.
///
pub struct EngineOpts(u32);

impl Default for EngineOpts {
	fn default() -> Self {
		Self(Default::default())
	}
}

impl EngineOpts {
	fn set_debug(&mut self, enabled: bool) {
		*self = Self(self.0 | enabled as u32);
	}

	#[inline]
	fn debug_enabled(&self) -> bool {
		self.0 & 1 != 0
	}
}

pub enum EngineMessage {
	BestMove(Move),
	Perft(MoveDivision),
	ReadyOk,
}

pub struct GladiusEngine {
	opts: EngineOpts,
	position: Arc<RwLock<Position>>,
	output_channel: Sender<EngineMessage>,
	stop: Arc<AtomicBool>,
}

impl GladiusEngine {
	pub fn new(output_channel: Sender<EngineMessage>) -> Self {
		GladiusEngine { 
			position: Arc::new(RwLock::new(Position::default())),
			opts: EngineOpts::default(),
			output_channel,
			stop: Arc::new(AtomicBool::default()),
		}
	}
}

impl Engine for GladiusEngine {
	fn setup_pos(&mut self, position: Position) {
		*self.position.write().unwrap() = position;
	}

	fn best_move(&self) {
		todo!()
	}

	fn new_game(&mut self) {
		self.setup_pos(Position::default());
	}

	fn set_debug(&mut self, enabled: bool) {
		self.opts.set_debug(enabled);
	}

	fn display(&self) -> String {
		self.position.read().unwrap().as_display_string()
	}

	fn perft(&self, depth: u8) {
		let cloned_position = self.position.read().unwrap().clone();
		let tx = self.output_channel.clone();
		std::thread::spawn(move || {
			tx.send(EngineMessage::Perft(divide(cloned_position, depth))).unwrap();
		});
	}

	fn stop(&self) {
		self.stop.store(true, std::sync::atomic::Ordering::Relaxed);
	}

	fn go(&self, parameters: SearchParameters) {
		
	}

	fn ready(&self) {
		self.output_channel.send(EngineMessage::ReadyOk).unwrap();
	}
}
