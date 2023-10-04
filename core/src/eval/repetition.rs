/// Detect a threefold repetition, which will result in a draw.
///
/// This code is adapted from a description on the
/// [Chess Programming Wiki](https://www.chessprogramming.org/Repetitions#List_of_Keys)
pub fn is_threefold_repetition(
	ply_from_irreversible: u8,
	ply_from_root: u8,
	move_history: &Vec<u64>,
	new_position: u64,
) -> bool {
	if ply_from_irreversible > 5 {
		let start = move_history.len() - ply_from_irreversible as usize;
		let repetition_count = move_history[start..]
			.iter()
			.filter(|position| new_position.eq(&position))
			.count();
		// Either we've found a definite 3 fold repetition, or we're guessing that
		// we'd find one a little further along and are stopping early to speed
		// up the search.
		repetition_count == 3 || (repetition_count == 2 && ply_from_root > 4)
	} else {
		false
	}
}
