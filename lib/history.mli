(** Representation of the history necessary for forming a leaderboard
    This module handles the history of previous played games. [game_record] is the type representing a previous game,
    [personal_stats] represents the data for a single player, and [leaderboard_entry] represents the data needed for a single entry in a
    leaderboard.
*)

type game_record  = {
  turns : int;
  solved : bool;
  player : string;
  answer : string;
}
(** The type of a past game *)

type personal_stats = {
  turns_data : int list;
  avg_num_turns : float;
  max_turns: int
}
(** The type representing game data for a given player. [turns_data] is a list where 
    each index [i] has a value [j] representing that a player has played [j] games that took [i + 1] turns to complete. *)

type leaderboard_entry = {
  player: string;
  average_turns: float;
  solved_games: int
}
(** The type representing a single entry on the leaderboard*)



val write_to_json:  Yojson.Basic.t -> State.t -> string -> string -> unit
(** [write_to_json json game init fn]  writes to json file with name [fn], the data from [json] updated with a new game [game] played by a player with initial [init]*)


val read_from_json:  Yojson.Basic.t -> game_record list
(** [read_from_json j], is a list of the previously played games in json data [j]*)


val get_history_turns: game_record list -> string -> int list
(** [get_history_turns d p] gives a list where each index i in that list has a value corresponding to the number of games that took i + 1 turns to complete  *)


val get_player_games: string -> game_record list -> game_record list
(** [get_player_games p game_list] returns the list of games that player p played*)

val get_games_by_length: int -> game_record list -> game_record list 
(** [get_games_by_length len lst] is the filtered game_record list containing games where each word is of length len *)

val games_to_string: game_record list -> string 
(** [print_games games] returns a formatted string of all games in the passed in games list *)


val get_turns_by_game_length: game_record list -> string -> int -> int list 
(** [get_turns_by_game_length g p l] returns a list in the form of get_history_turns, where each game playerd is of length l*)

val get_personal_stats: game_record list -> string -> personal_stats
(** [get_personal_stats lst player] returns a record with fields for the turns data produced by get_history_turns as well as a field for the average number of turns *)


val get_leaderboard_data: game_record list -> leaderboard_entry list
(** [get_leaderboard_data g] produces a list of leaderboard data*)
