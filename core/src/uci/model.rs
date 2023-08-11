use std::rc::Rc;

use crate::position::{Move, Position};

pub enum UciCommand {
  UCI,
  Debug(bool),
  IsReady,
  SetOption { name: String, value: Option<String> },
  Register,
  NewGame,
  Position { start_position: Position, moves: Option<Rc<[Move]>> },
  Go { 
    search_moves: Option<Rc<[Move]>>,
    wtime: Option<u32>,
    btime: Option<u32>,
    winc: Option<u32>,
    binc: Option<u32>,
    depth: Option<u8>,
    nodes: Option<u32>,
    move_time: Option<u32>,
    infinite: Option<bool>
  },
  Stop,
  Quit,
  Help
}

impl std::fmt::Debug for UciCommand {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::UCI => write!(f, "UCI"),
      Self::Debug(arg0) => write!(f, "Debug"),
      Self::IsReady => write!(f, "IsReady"),
      Self::SetOption { .. } => write!(f, "SetOption"),
      Self::Register => write!(f, "Register"),
      Self::NewGame => write!(f, "NewGame"),
      Self::Position { .. } => write!(f, "Position"),
      Self::Go { .. } => write!(f, "Go"),
      Self::Stop => write!(f, "Stop"),
      Self::Quit => write!(f, "Quit"),
      Self::Help => write!(f, "Help"),
    }
  }
}

pub enum UciResponse {
  Id(String, String),
  UCIOk,
  ReadyOk,
  BestMove(String),
  CopyProtection(String),
  Registration(),
  Info(),
  Option()
}

pub enum EngineOptions {
  Hash(u32),
  NalimovPath(String),
  NalimovCache(String),
  Ponder(bool),
  OwnBook(bool),
  MultiPV(u32),
  UCIShowCurrentLine(bool),
  UCIShowRefutations(bool),
  UCILimitStrength(bool),
  UCIElo(u32),
  UCIAnalyzeMode(bool),
  UCIOpponent(String),
  UCIEngineAbout(String),
  UCIShredderbasesPath(String),
}