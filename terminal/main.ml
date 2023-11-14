open Wordle.Gameplay

let data_dir_prefix = "data" ^ Filename.dir_sep

let word_list_unparsed len =
  Yojson.Basic.from_file (data_dir_prefix ^ len ^ "-letter-words.json")

let num_guesses = 5
let rec repeat n s = if n = 0 then "" else s ^ repeat (n - 1) s
let data_dir_prefix = "data" ^ Filename.dir_sep

let game_history_json =
  Yojson.Basic.from_file (data_dir_prefix ^ "game-history.json")

let convert_to_percent_string (value : float) =
  let percent = Float.round (value *. 100.) in
  string_of_int (int_of_float percent) ^ " %"

let round_turns avg_turns =
  let turns_string = string_of_float avg_turns in
  if String.length turns_string = 2 then turns_string ^ "00"
  else if String.length turns_string = 3 then turns_string ^ "0"
  else String.sub turns_string 0 4

let rec create_padding n = if n = 0 then "" else " " ^ create_padding (n - 1)

let pad_player_row (player_name : string) =
  player_name ^ create_padding (17 - String.length player_name)

let rec leaderboard_loop (st : Wordle.State.t) lst (initials : string)
    game_history =
  let full_history_lst = Wordle.History.read_from_json game_history in
  let player_games =
    Wordle.History.get_player_games initials full_history_lst
  in
  let total_player_games = List.length player_games in
  let total_player_games_flt = float_of_int total_player_games in
  (* average turns player with [initials] uses in his or her games *)
  let rec get_number_solved (lst : Wordle.History.game_record list) =
    match lst with
    | [] -> 0
    | h :: t -> (if h.solved = true then 1 else 0) + get_number_solved t
  in
  let percent_player_correct =
    float_of_int (get_number_solved player_games) /. total_player_games_flt
  in
  let leaderboard_data = Wordle.History.get_leaderboard_data full_history_lst in
  let personal_stats =
    Wordle.History.get_personal_stats full_history_lst initials
  in
  let rec leaderboard_printer (lst : Wordle.History.leaderboard_entry list) =
    match lst with
    | [] -> print_endline ""
    | h :: t ->
        print_string (pad_player_row h.player);
        print_int h.solved_games;
        print_string "                  ";
        print_string (round_turns h.average_turns);
        print_endline "";
        leaderboard_printer t
  in
  print_endline
    "Do you want to see your personal stats, the leaderboard, or quit? (personal\n\
    \     stats/leaderboard/quit)";
  let rec turn_printer = function
    | [] -> print_string ""
    | h :: t ->
        print_int h;
        turn_printer t
  in
  match String.lowercase_ascii (read_line ()) with
  | exception End_of_file -> failwith "error"
  | input ->
      if input = "leaderboard" then (
        print_endline "player        solved games       average turns";

        leaderboard_printer leaderboard_data)
      else if input = "personal stats" then (
        print_endline "";
        print_string "Number of Games Played: ";
        turn_printer personal_stats.turns_data;
        print_string
          ("\nMax Turns: " ^ string_of_int personal_stats.max_turns ^ "\n");
        print_string
          ("Average Turns Taken: "
          ^ string_of_float personal_stats.avg_num_turns
          ^ "\n");
        print_string
          ("Win Percentage: "
          ^ convert_to_percent_string percent_player_correct
          ^ "\n\n"))
      else if input = "quit" then exit 0
      else print_endline "leaderboard, personal stats, or quit must be inputted";
      leaderboard_loop st lst initials game_history

let rec leaderboard_access (st : Wordle.State.t) lst (initials : string)
    game_history =
  print_endline "Do you want to take a look at game statistics? (yes/n)";
  match String.lowercase_ascii (read_line ()) with
  | exception End_of_file -> failwith "error"
  | input ->
      if input = "yes" then leaderboard_loop st lst initials game_history
      else if input = "n" then exit 0
      else print_endline "Please answer yes/n";
      leaderboard_access st lst initials game_history

let rec parse_guess (st : Wordle.State.t) lst : Wordle.State.t =
  let turns_left = Wordle.State.get_turns_left st in
  print_endline
    ("Please enter your current guess "
    ^ repeat (String.length (Wordle.State.get_answer st)) "_ "
    ^ " (" ^ string_of_int turns_left
    ^ if turns_left = 1 then " turn remaining)" else " turns remaining)");
  Wordle.Printing.print_letters (Wordle.State.get_letters st);
  print_string "\n";
  match String.lowercase_ascii (read_line ()) with
  | exception End_of_file -> failwith "Error"
  | input -> (
      if check_legality input lst = false then (
        print_endline
          (input ^ " is not in the word list, please enter a new word \n");
        parse_guess st lst)
      else
        try
          match Wordle.State.add_guess st input with
          | new_state -> new_state
        with
        | Wordle.State.InvalidCharacter s ->
            let () = print_endline s in
            parse_guess st lst
        | Wordle.State.IncorrectLength s ->
            let () = print_endline s in
            parse_guess st lst
        | Wordle.State.NoMoreTurns s -> (
            let () = print_endline s in
            let () =
              print_endline
                ("Oh no you lost :( The correct word was "
               ^ Wordle.State.get_answer st)
            in
            print_string "Enter Your Initials: \n";
            match String.lowercase_ascii (read_line ()) with
            | exception End_of_file -> failwith "Error"
            | input ->
                Wordle.History.write_to_json game_history_json st input
                  "game-history.json";
                let new_history =
                  Yojson.Basic.from_file (data_dir_prefix ^ "game-history.json")
                in
                leaderboard_access st lst input new_history))

let rec game_loop (st : Wordle.State.t) lst =
  let next_state = parse_guess st lst in
  let guesses = Wordle.State.get_guesses next_state in
  Wordle.Printing.print_guess (List.hd (List.rev guesses));
  print_string "\n";
  if Wordle.State.get_solved next_state then (
    print_string "You Won! \n";
    print_string "Enter Your Initials: \n";
    match String.lowercase_ascii (read_line ()) with
    | exception End_of_file -> failwith "Error"
    | input ->
        Wordle.History.write_to_json game_history_json next_state input
          "game-history.json";
        let new_history =
          Yojson.Basic.from_file (data_dir_prefix ^ "game-history.json")
        in
        leaderboard_access next_state lst input new_history)
  else print_endline "";
  game_loop next_state lst

let rec length_chooser () =
  ANSITerminal.print_string [ ANSITerminal.black ]
    "\n\nWhat word length between 4 and 8 letters do you want to guess? \n";
  match String.lowercase_ascii (read_line ()) with
  | exception End_of_file -> failwith "Error"
  | input ->
      if
        Char.code input.[0] < 52
        || Char.code input.[0] > 56
        || String.length input > 1
      then (
        print_string
          "Only words of lengths between 4 and 8 letters can be generated. \
           Please enter a number between 4 and 8.";
        length_chooser ())
      else from_json (word_list_unparsed input)

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Wordle\n";
  let word_list = length_chooser () in
  ANSITerminal.print_string [ ANSITerminal.black ]
    "\n\nI am generating a random word for the answer \n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nOk Got It! Let's start the game. \n";
  let new_game_state =
    let solution = generate_soln word_list in
    Wordle.State.new_game num_guesses solution
  in
  game_loop new_game_state word_list

let () = main ()
