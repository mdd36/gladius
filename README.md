
     _____ _           _ _
    |  __ \ |         | (_)
    | |  \/ | __ _  __| |_ _   _ ___
    | | __| |/ _` |/ _` | | | | / __|
    | |_\ \ | (_| | (_| | | |_| \__ \
     \____/_|\__,_|\__,_|_|\__,_|___/

<div align=center>

[![Build and Test](https://github.com/mdd36/gladius/actions/workflows/build-and-test.yaml/badge.svg)](https://github.com/mdd36/gladius/actions/workflows/build-and-test.yaml)

[![lichess-bullet](https://lichess-shield.vercel.app/api?username=gladius_bot&format=bullet)](https://lichess.org/@/gladius_bot/perf/bullet)
[![lichess-blitz](https://lichess-shield.vercel.app/api?username=gladius_bot&format=blitz)](https://lichess.org/@/gladius_bot/perf/blitz)
[![lichess-rapid](https://lichess-shield.vercel.app/api?username=gladius_bot&format=rapid)](https://lichess.org/@/gladius_bot/perf/rapid)
</div>

A chess engine built in Rust. By default, the core engine comes bundled with a [Universal Chess Interface](https://en.wikipedia.org/wiki/Universal_Chess_Interface) (UCI) front end to support CLI and bot play. Connected to the [gladius_bot](https://lichess.org/@/gladius_bot) Lichess account, where it's currently accepting human challenges.

## Structure
This repo holds a Cargo workspace with two crates, `gladius_core` and `gladius`. 

`gladius_core` manages all of the actual logic for playing chess including move generation, position evaluation, and move selection, and exposes this information through an implementation of the `Engine` trait. It's possible to use `gladius_core` as library in other projects if you want to change the frontend or just need a chess engine for some reason. Due to the extreme time constraints in chess programming, the core often favors performance over abstraction. 

`gladius` is a CLI program to interact with the engine through the UCI, along with some additional commands to make CLI play a little easier. Where possible, I've used the same syntax as [stockfish](https://github.com/official-stockfish/Stockfish/) when deviating from the UCI.

## How does it work?
*Note: this is a high level summary, and much of the nitty-gritty is omitted. You can look at the generated docs for a more granular view*

The engine uses [bitboards](https://www.chessprogramming.org/Bitboards) to represent positions internally, and these bitboards have a new type wrapper over them. A position holds one bitboard for each color and piece, plus some metadata (en passant square, which side is moving, etc) that gets packed into an unsigned short. Move generation uses precomputed movement tables for non-sliding pieces, and [magic bitboards](https://www.chessprogramming.org/Magic_Bitboards) for the sliding piece attacks. The magics are generated outside of the program and imported into `magics.rs`. During search, the moves are dynamically ordered over serval phases: first, it looks at the move from the transposition table, then captures ranked by their static exchange value, then any [killer moves](https://www.chessprogramming.org/Killer_Heuristic), and finally all other moves, again ordered by their predicted goodness.

The engine itself performs searches on a separate thread so that it remains responsive while thinking. Currently, only one search can be run at a time and that search doesn't use multiple threads. The engine communicates with its caller through a [messaging channel](https://doc.rust-lang.org/std/sync/mpsc/index.html), which lets it report back information, errors, or the best move asynchronously.

## What's next?
In no particular order:

- Better evaluation, especially of king safety
- Search is pretty slow, so more performance testing
- Add support for more UCI features like ELO limitation
- Add our own opening book
- Make search multithreaded

If you've played Gladius and feel like it's doing something poorly, open an issue so I can try to make it better!

## What's with the name?

[Gladius](https://en.wikipedia.org/wiki/Gladius) is the Latin name for the swords of Roman soldiers.

## Acknowledgements
- The [Chess Programming Wiki](https://www.chessprogramming.org), which has been an invaluable reference at every stage
- [Sebastian Lague](https://www.youtube.com/@SebastianLague), whose videos on chess programming made me start this project
