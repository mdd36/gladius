use crate::position::Position;

pub trait Engine {
	fn setup_pos(&mut self, position: Position);

	fn best_move(&self);

	fn new_game(&mut self);

	fn set_debug(&mut self, enabled: bool);
}

/// Various flags and values that affect engine behavior.
///
pub struct EngineOpts(u32);

impl EngineOpts {
	#[inline]
	fn set_debug(&mut self, enabled: bool) {
		*self = Self(self.0 | enabled as u32);
	}

	#[inline]
	fn debug_enabled(&self) -> bool {
		self.0 & 1 != 0
	}
}

pub struct GladiusEngine {
	opts: EngineOpts,
	position: Position,
}

impl Engine for GladiusEngine {
	fn setup_pos(&mut self, position: Position) {
		self.position = position;
	}

	fn best_move(&self) {
		todo!()
	}

	fn new_game(&mut self) {
		self.position = Position::default();
	}

	fn set_debug(&mut self, enabled: bool) {
		self.opts.set_debug(enabled);
	}
}
