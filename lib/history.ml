type game_record = {
  turns : int;
  solved : bool;
  player : string;
  answer : string;
}

type t = game_record list

let write_to_json (json : Yojson.Basic.t) (st : State.t) (initials : string)
    (file_name : string) =
  let num_guesses = List.length (State.get_guesses st) + 1 in
  let answer = State.get_answer st in
  let solved = State.get_solved st in
  let new_game =
    `Assoc
      [
        ("turns", `Int num_guesses);
        ("answer", `String answer);
        ("solved", `Bool solved);
        ("player", `String initials);
      ]
  in
  let _, values =
    match json with
    | `Assoc [ ("games", `List values) ] -> ("games", values)
    | _ -> failwith "Invalid JSON format"
  in
  let new_json = `Assoc [ ("games", `List ([ new_game ] @ values)) ] in

  let str_rep = Yojson.Basic.to_string new_json in

  Yojson.Basic.to_file
    ("data" ^ Filename.dir_sep ^ file_name)
    (Yojson.Basic.from_string str_rep)

let json_object_to_ocaml_record json_obj =
  let open Yojson.Basic.Util in
  let turns' = json_obj |> member "turns" |> to_int in
  let answer' = json_obj |> member "answer" |> to_string in
  let solved' = json_obj |> member "solved" |> to_bool in
  let player' = json_obj |> member "player" |> to_string in
  let record : game_record =
    { turns = turns'; answer = answer'; solved = solved'; player = player' }
  in
  record

let read_from_json json =
  let open Yojson.Basic.Util in
  json |> member "games" |> convert_each json_object_to_ocaml_record

let get_player_games (player : string) (games_list : t) =
  List.filter
    (fun game ->
      String.lowercase_ascii game.player = String.lowercase_ascii player)
    games_list

let get_turns_helper filtered_games =
  let history_arr = Array.make 6 0 in
  List.iter
    (fun x ->
      let current_num_turns = x.turns in
      history_arr.(current_num_turns - 1) <-
        history_arr.(current_num_turns - 1) + 1)
    filtered_games;
  Array.to_list history_arr

let get_history_turns categorized_history player =
  let player_games = get_player_games player categorized_history in
  get_turns_helper player_games

let get_turns_by_game_length game_list player length =
  let filtered_games =
    List.filter
      (fun g -> g.player = player && String.length g.answer = length)
      game_list
  in
  get_turns_helper filtered_games

let game_to_string acc (record : game_record) =
  "{" ^ "turns: " ^ string_of_int record.turns ^ " player: " ^ record.player
  ^ "}," ^ acc

let games_to_string games_list =
  let str_to_print = List.fold_left game_to_string "[" games_list in
  str_to_print

let get_games_by_length len games_list =
  List.filter (fun g -> String.length g.answer = len) games_list

type personal_stats = {
  turns_data : int list;
  avg_num_turns : float;
  max_turns : int;
}

let get_personal_stats game_list player_name =
  let player_games = get_player_games player_name game_list in
  let turns_data = get_history_turns game_list player_name in
  let total_turns =
    List.fold_left (fun acc curr -> acc + curr.turns) 0 player_games
  in
  if total_turns = 0 then { turns_data; avg_num_turns = 0.; max_turns = 0 }
  else
    let avg_num_turns =
      float_of_int total_turns /. float_of_int (List.length player_games)
    in
    let max_turns =
      List.fold_left
        (fun acc curr -> if curr.turns > acc then curr.turns else acc)
        0 player_games
    in
    { turns_data; avg_num_turns; max_turns }

module Map = Map.Make (String)

type temp_player_record = {
  mutable completed_count : int;
  mutable turns_count : int;
  mutable games_count : int;
}

type leaderboard_entry = {
  player : string;
  average_turns : float;
  solved_games : int;
}

let rec get_leaderboard_data_helper (games : game_record list)
    (map : temp_player_record Map.t) =
  match games with
  | [] -> map
  | h :: t ->
      let completed_game = h.solved in
      let turns = h.turns in
      let player_name = h.player in
      if Map.mem player_name map then (
        let current_player_data = Map.find h.player map in
        current_player_data.games_count <- current_player_data.games_count + 1;
        current_player_data.turns_count <-
          current_player_data.turns_count + turns;
        if completed_game then
          current_player_data.completed_count <-
            current_player_data.completed_count + 1
        else ();
        get_leaderboard_data_helper t map)
      else if completed_game then
        let (new_player : temp_player_record) =
          { completed_count = 1; turns_count = turns; games_count = 1 }
        in
        get_leaderboard_data_helper t (Map.add player_name new_player map)
      else
        let new_player : temp_player_record =
          { completed_count = 0; turns_count = turns; games_count = 1 }
        in
        get_leaderboard_data_helper t (Map.add player_name new_player map)

let get_leaderboard_data (games : game_record list) =
  let player_map : temp_player_record Map.t =
    get_leaderboard_data_helper games Map.empty
  in
  let (acc : leaderboard_entry list) = [] in
  let leaderboard_entries =
    Map.fold
      (fun key v acc ->
        let (new_entry : leaderboard_entry) =
          {
            average_turns =
              float_of_int v.turns_count /. float_of_int v.games_count;
            solved_games = v.completed_count;
            player = key;
          }
        in
        new_entry :: acc)
      player_map acc
  in
  List.sort
    (fun x y -> int_of_float ((x.average_turns -. y.average_turns) *. 10.))
    leaderboard_entries
