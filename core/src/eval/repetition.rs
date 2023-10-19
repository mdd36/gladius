use crate::position::Position;

/// A wrapper to handle managing position history and checking for
/// repetitions.
#[derive(Clone, Default)]
pub struct PositionHistory(Vec<u64>);

impl PositionHistory {
	/// Add a new position to the history.
	pub fn add_position(&mut self, position: &Position) {
		self.0.push(position.hash());
	}

	/// Remove the last visited position from the history
	pub fn pop(&mut self) {
		self.0.pop();
	}

	/// Remove all entries from the history
	pub fn clear(&mut self) {
		self.0.clear()
	}

	/// Count the number of repetitions of a given position.
	pub fn repetitions(&self, position: &Position) -> u8 {
		// Only need to scan from the last irreversible move,
		// which is a pawn push or capture. The first repetition
		// can only come on the 3rd ply from the irreversible move
		self.0
			.iter()
			.rev()
			.take(position.half_move_clock() as usize + 1)
			.filter(|&&hash| hash == position.hash())
			.count() as u8
	}
}

#[cfg(test)]
pub mod test {

	use crate::position::moves::Move;
	use pretty_assertions::assert_eq;

	use super::*;

	#[test]
	fn position_history_test() {
		let mut position_history = PositionHistory::default();

		let mut position = Position::from_fen(
			"r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
		)
		.unwrap();
		assert_eq!(0, position_history.repetitions(&position));
		position_history.add_position(&position);

		for cycle in 0u8..2 {
			position = position.apply_move(&Move::from_uci_str("f3f5", &position));
			assert_eq!(cycle, position_history.repetitions(&position));
			position_history.add_position(&position);

			position = position.apply_move(&Move::from_uci_str("e7c5", &position));
			assert_eq!(cycle, position_history.repetitions(&position));
			position_history.add_position(&position);

			position = position.apply_move(&Move::from_uci_str("f5f3", &position));
			assert_eq!(cycle, position_history.repetitions(&position));
			position_history.add_position(&position);

			position = position.apply_move(&Move::from_uci_str("c5e7", &position));
			assert_eq!(cycle + 1, position_history.repetitions(&position));
			position_history.add_position(&position);
		}

		position = position.apply_move(&Move::from_uci_str("g2g4", &position));
		assert_eq!(0, position_history.repetitions(&position));
		position_history.add_position(&position);
	}
}
