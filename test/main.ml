(*Testing Approach: We designed our project so that there are two ways to play a
  game of wordle. Either you can play on the terminal, or through a browser.
  Both versions of the game rely upon the same modules for functionality such as
  adding guesses, encoding them with colors, generating leaderboard data, and
  fetching leaderboard data. As such, we have used OUnit to test the various
  functions in our modules, and have manually tested aspects of the UI. The only
  aspect of the modules not tested with OUnit are the file output operations,
  which are tested manually alongside the UI. Test cases are for the most part
  black-box, as we test various aspects of what a game would look like without
  regards to the module's internal code. Many of our test cases revolve around
  testing boundary cases. There is a small amount of randomized testing in terms
  of ensuring that randomly generated answers are valid. Our testing plan
  demonstrates the correctness of the code because we have covered a large
  variety of different conditions for all the different core functionality, and
  have ensured that the UI works when playing games, viewing leaderboard data,
  and saving games played. *)

open OUnit2
open Wordle

let data_dir_prefix = "data" ^ Filename.dir_sep

let history_data_1 =
  let json_data =
    Yojson.Basic.from_file (data_dir_prefix ^ "history_test.json")
  in
  History.read_from_json json_data

let history_data_2 =
  let json_data =
    Yojson.Basic.from_file (data_dir_prefix ^ "history_test2.json")
  in
  History.read_from_json json_data

let history_data_3 =
  let json_data =
    Yojson.Basic.from_file (data_dir_prefix ^ "history_test3.json")
  in
  History.read_from_json json_data

let history_data_4 =
  let json_data =
    Yojson.Basic.from_file (data_dir_prefix ^ "history_test4.json")
  in
  History.read_from_json json_data

(* constructs a word list of word_length size*)
let create_word_list word_length =
  let unparsed_word_list =
    Yojson.Basic.from_file
      (data_dir_prefix ^ string_of_int word_length
     ^ "-letter-words-no-swears.json")
  in
  let parsed_word_list = Gameplay.from_json unparsed_word_list in
  parsed_word_list

let check_legality_test (name : string) (data : Wordle.Gameplay.t)
    (word : string) (expected_output : bool) =
  name >:: fun _ ->
  assert_equal (Wordle.Gameplay.check_legality word data) expected_output

let get_turns_left_test (name : string) (expected_output : int)
    (state : Wordle.State.t) =
  name >:: fun _ ->
  assert_equal expected_output (Wordle.State.get_turns_left state)

let get_guesses_test (name : string)
    (expected_output : (char * string) list list) (state : Wordle.State.t) =
  name >:: fun _ ->
  assert_equal expected_output (Wordle.State.get_guesses state)

let invalid_character_test (name : string) (input : string) =
  name >:: fun _ ->
  assert_raises
    (Wordle.State.InvalidCharacter "Input Contains Non-Alphabetical Characters")
    (fun () -> Wordle.State.add_guess (Wordle.State.new_game 5 "house") input)

let incorrect_length_test (name : string) (input : string) =
  name >:: fun _ ->
  assert_raises (Wordle.State.IncorrectLength "Input Is Not A Valid Length")
    (fun () -> Wordle.State.add_guess (Wordle.State.new_game 5 "house") input)

let no_more_turns_test (name : string) =
  name >:: fun _ ->
  assert_raises (Wordle.State.NoMoreTurns "Game Over") (fun () ->
      let state = Wordle.State.new_game 1 "words" in
      let state' = Wordle.State.add_guess state "tests" in
      Wordle.State.add_guess state' "tests")

(** [get_answer_test name st expected_output] constructs an OUnit test named
    [name] that asserts the equality of [expected_output] with [get_answer st] *)
let get_answer_test (name : string) (st : Wordle.State.t)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output (Wordle.State.get_answer st) ~printer:Fun.id

(** [get_solved_test name st expected_output] constructs an OUnit test named
    [name] that asserts the equality of [expected_output] with [get_solved st] *)
let get_solved_test (name : string) (st : Wordle.State.t)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (Wordle.State.get_solved st)
    ~printer:string_of_bool

let word_list = create_word_list 5

let rec check_legality_tests tests =
  match tests with
  | [] -> true
  | h :: t ->
      (not (Wordle.Gameplay.check_legality h word_list))
      && check_legality_tests t

let rec check_soln_tests n =
  if n = 0 then false
  else
    let generated_soln = Wordle.Gameplay.generate_soln word_list in
    Wordle.Gameplay.check_legality generated_soln word_list
    && check_soln_tests (n - 1)

let game_play_tests =
  [
    check_legality_test "A word not in the word list is illegal" word_list
      "qwerf" false;
    check_legality_test "A word in the word list " word_list "match" true;
    ( "a long list of invalid words are deemed illegal" >:: fun _ ->
      let invalid_words = [ "zzdfd"; "fijes"; "FDADF"; "POHSTS" ] in
      let all_invalid = check_legality_tests invalid_words in
      assert_equal all_invalid true );
    ( "a list of generated guesses are all considered legal " >:: fun _ ->
      let ten_guesses_valid = check_soln_tests 10 in
      assert_equal ten_guesses_valid ten_guesses_valid );
  ]

(* adds a guess to the game_state, precondition: guess is valid*)
let apply_move game_state word = Wordle.State.add_guess game_state word

let rec apply_moves (game_state : Wordle.State.t) (word_list : string list) =
  match word_list with
  | [] -> game_state
  | h :: t -> apply_moves (apply_move game_state h) t

let create_game_state (guess_list : string list) (answer : string) =
  let initial_game_state = Wordle.State.new_game 6 answer in
  let updated_game_state = apply_moves initial_game_state guess_list in
  updated_game_state

let rec guess_printer_helper (guess : (char * string) list) =
  match guess with
  | [] -> ""
  | h :: t ->
      let letter_string = String.make 1 (fst h) in
      let color = snd h in
      "(" ^ letter_string ^ ", " ^ color ^ "); " ^ guess_printer_helper t

let rec guesses_printer (guess_list : (char * string) list list) =
  match guess_list with
  | [] -> ""
  | h :: t ->
      let str_rep = "[ " ^ guess_printer_helper h ^ " ]; " in
      str_rep ^ "; " ^ guesses_printer t

let generate_keyboard () =
  let keyboard_ref = ref [] in
  for i = 97 to 122 do
    let new_entry = (Char.chr i, "gray") in
    keyboard_ref := new_entry :: !keyboard_ref
  done;
  !keyboard_ref

let rec keyboard_printer keyboard =
  match keyboard with
  | [] -> ""
  | h :: t ->
      "(" ^ String.make 1 (fst h) ^ "," ^ snd h ^ "); " ^ keyboard_printer t

let map_keyboard (letters : (char * string) list)
    (keyboard : (char * string) list) =
  let new_keyboard =
    List.map
      (fun (lett, color) ->
        let lett_option : (char * string) option =
          List.find_opt (fun entry -> fst entry = lett) letters
        in
        match lett_option with
        | None -> (lett, color)
        | Some v -> (lett, snd v))
      keyboard
  in
  new_keyboard

let game_state_tests =
  [
    get_turns_left_test "Number of turns in a game that just started" 5
      (Wordle.State.new_game 5 "house");
    ( "there are 5 turns remaining in a game with one guess" >:: fun _ ->
      let game_state = create_game_state [ "hello" ] "goods" in
      let turns_remaining = State.get_turns_left game_state in
      assert_equal turns_remaining 5 );
    ( "get answer is correct after multiple guesses have been added" >:: fun _ ->
      let new_game =
        create_game_state [ "hello"; "hello"; "hello"; "hello" ] "goody"
      in
      assert_equal (State.get_answer new_game) "goody" );
    ( "the keyboard is correct on game initialization" >:: fun _ ->
      let empty_keyboard = generate_keyboard () in
      let new_game = create_game_state [] "hello" in
      let actual_keyboard = State.get_letters new_game in
      assert_equal (List.rev empty_keyboard) actual_keyboard
        ~printer:keyboard_printer );
    ( "the keyboard is correct after one correct guess" >:: fun _ ->
      let empty_keyboard = generate_keyboard () in
      let new_game = create_game_state [ "hello" ] "hello" in
      let mapped_keyboard =
        map_keyboard
          [
            ('h', "green");
            ('e', "green");
            ('l', "green");
            ('l', "green");
            ('o', "green");
          ]
          empty_keyboard
      in
      let actual_keyboard = State.get_letters new_game in
      assert_equal (List.rev mapped_keyboard) actual_keyboard
        ~printer:keyboard_printer );
    ( "the keyboard is correct after one incorrect guess" >:: fun _ ->
      let empty_keyboard = generate_keyboard () in
      let new_game = create_game_state [ "hello" ] "crass" in
      let mapped_keyboard =
        map_keyboard
          [
            ('h', "red"); ('e', "red"); ('l', "red"); ('l', "red"); ('o', "red");
          ]
          empty_keyboard
      in
      let actual_keyboard = State.get_letters new_game in
      assert_equal (List.rev mapped_keyboard) actual_keyboard
        ~printer:keyboard_printer );
    ( "the keyboard is correct after one incorrect guess with yellows"
    >:: fun _ ->
      let empty_keyboard = generate_keyboard () in
      let new_game = create_game_state [ "hello" ] "gleds" in
      let mapped_keyboard =
        map_keyboard
          [
            ('h', "red");
            ('e', "yellow");
            ('l', "yellow");
            ('l', "yellow");
            ('o', "red");
          ]
          empty_keyboard
      in
      let actual_keyboard = State.get_letters new_game in
      assert_equal (List.rev mapped_keyboard) actual_keyboard
        ~printer:keyboard_printer );
    get_turns_left_test "Number of turns in a game after 1 guess was made" 4
      (Wordle.State.add_guess (Wordle.State.new_game 5 "house") "sound");
    ( "get turns returns 0 when there are no turns remaining" >:: fun _ ->
      let new_game =
        create_game_state
          [ "hello"; "hello"; "hello"; "hello"; "hello"; "hello" ]
          "fused"
      in
      let turns_remaining = State.get_turns_left new_game in
      assert_equal turns_remaining 0 );
    get_guesses_test "Guesses list for a game that just started" []
      (Wordle.State.new_game 5 "house");
    get_guesses_test "Guesses list for a game with 1 guess"
      [
        [
          ('s', "yellow");
          ('o', "green");
          ('u', "green");
          ('n', "red");
          ('d', "red");
        ];
      ]
      (Wordle.State.add_guess (Wordle.State.new_game 5 "house") "sound");
    invalid_character_test "Invalid character in input" "soun!";
    incorrect_length_test "Incorrect length of input" "abcd";
    no_more_turns_test "No more turns available";
    get_answer_test {|get_answer of state with solution "sound" is "sound"|}
      (Wordle.State.new_game 5 "sound")
      "sound";
    get_solved_test {|get_solved of new game is false|}
      (Wordle.State.new_game 5 "sound")
      false;
    get_solved_test {|get_solved of solved game is true|}
      (Wordle.State.add_guess (Wordle.State.new_game 5 "house") "house")
      true;
    ( "Guess list for a game with 2 guesses" >:: fun _ ->
      let game = create_game_state [ "hello"; "world" ] "world" in
      let expected_guess_list =
        [
          [
            ('h', "red");
            ('e', "red");
            ('l', "yellow");
            ('l', "green");
            ('o', "yellow");
          ];
          [
            ('w', "green");
            ('o', "green");
            ('r', "green");
            ('l', "green");
            ('d', "green");
          ];
        ]
      in
      let actual_guess_list = State.get_guesses game in
      assert_equal expected_guess_list actual_guess_list
        ~printer:guesses_printer );
    ( "Guess list where every letter is red" >:: fun _ ->
      let game = create_game_state [ "hello" ] "zaids" in
      let expected_guess_list =
        [
          [
            ('h', "red"); ('e', "red"); ('l', "red"); ('l', "red"); ('o', "red");
          ];
        ]
      in
      let actual_guess_list = State.get_guesses game in
      assert_equal expected_guess_list actual_guess_list
        ~printer:guesses_printer );
    ( "Guess list where every letter is yellow" >:: fun _ ->
      let game = create_game_state [ "faint" ] "afnti" in
      let expected_guess_list =
        [
          [
            ('f', "yellow");
            ('a', "yellow");
            ('i', "yellow");
            ('n', "yellow");
            ('t', "yellow");
          ];
        ]
      in
      let actual_guess_list = State.get_guesses game in
      assert_equal actual_guess_list expected_guess_list
        ~printer:guesses_printer );
    ( "Guess list with one guess that is all green" >:: fun _ ->
      let game = create_game_state [ "hello" ] "hello" in
      let expected_guess_list =
        [
          [
            ('h', "green");
            ('e', "green");
            ('l', "green");
            ('l', "green");
            ('o', "green");
          ];
        ]
      in
      let actual_guess_list = State.get_guesses game in
      assert_equal expected_guess_list actual_guess_list );
    ( "Guess list is correct given 3 guesses" >:: fun _ ->
      let game = create_game_state [ "trace"; "trade"; "tried" ] "train" in
      let expected_guess_list =
        [
          [
            ('t', "green");
            ('r', "green");
            ('a', "green");
            ('c', "red");
            ('e', "red");
          ];
          [
            ('t', "green");
            ('r', "green");
            ('a', "green");
            ('d', "red");
            ('e', "red");
          ];
          [
            ('t', "green");
            ('r', "green");
            ('i', "yellow");
            ('e', "red");
            ('d', "red");
          ];
        ]
      in
      let actual_guess_list = State.get_guesses game in
      assert_equal actual_guess_list expected_guess_list
        ~printer:guesses_printer );
  ]

let rec turns_printer (turns_list : int list) =
  match turns_list with
  | [] -> "[]"
  | h :: t -> string_of_int h ^ " :: " ^ turns_printer t

let history_tests =
  [
    ( "History satisfies various properties" >:: fun _ ->
      let saved_games = History.get_player_games "Eli" history_data_1 in
      let length_correct = List.length saved_games = 3 in
      let turns_correct =
        List.for_all (fun x -> x.History.turns = 4) saved_games
      in
      let solved_correct =
        List.for_all (fun x -> x.History.solved = false) saved_games
      in
      assert_equal (length_correct && turns_correct && solved_correct) true );
    ( "History handles games of different length correct" >:: fun _ ->
      let schmeli_games = History.get_player_games "Schmeli" history_data_1 in
      let six_letter_games = History.get_games_by_length 6 schmeli_games in
      let seven_letter_games = History.get_games_by_length 7 schmeli_games in
      let eight_letter_games = History.get_games_by_length 8 schmeli_games in
      let list_lengths_correct =
        List.length six_letter_games = 1
        && List.length seven_letter_games = 1
        && List.length eight_letter_games = 1
      in
      assert_equal list_lengths_correct true );
    ( "get_history_turns returns accurate turns data" >:: fun _ ->
      let turns_list = History.get_history_turns history_data_2 "Eli" in
      assert_equal ~printer:turns_printer [ 1; 2; 1; 2; 2; 1 ] turns_list );
    ( "get_history_turns handles possiblity of zero in list correctly"
    >:: fun _ ->
      let turns_list = History.get_history_turns history_data_2 "Elijah" in
      assert_equal ~printer:turns_printer [ 1; 0; 0; 0; 0; 1 ] turns_list );
    ( "get_history_turns is not case sensitive" >:: fun _ ->
      let turns_list = History.get_history_turns history_data_2 "elijah" in
      assert_equal ~printer:turns_printer [ 1; 0; 0; 0; 0; 1 ] turns_list );
    ( "get_player_games returns accurate data" >:: fun _ ->
      let player_games = History.get_player_games "Elijah" history_data_2 in
      assert_equal 2 (List.length player_games) );
    ( "get_player_games returns an accurate record list" >:: fun _ ->
      let player_games = History.get_player_games "Ellie" history_data_2 in
      let expected_records : History.game_record list =
        [ { turns = 6; solved = false; player = "Ellie"; answer = "roman" } ]
      in
      assert_equal expected_records player_games
        ~printer:History.games_to_string );
    ( "get_player_games returns an empty list when a player hasn't played any \
       games"
    >:: fun _ ->
      let player_games = History.get_player_games "fake" history_data_3 in
      assert_equal (List.length player_games) 0 );
    ( "get_player_games is case insensitive" >:: fun _ ->
      let player_games = History.get_player_games "elijah" history_data_2 in
      assert_equal 2 (List.length player_games) );
    ( "get_player_games returns an empty list when a player has played no games"
    >:: fun _ ->
      let player_games = History.get_player_games "Schmeli" history_data_2 in
      assert_equal (List.length player_games) 0 );
    ( "get_personal_stats returns accurate stats when a player has played no \
       games"
    >:: fun _ ->
      let player_stats = History.get_personal_stats history_data_2 "Schmeli" in
      let expected_stats : History.personal_stats =
        { turns_data = [ 0; 0; 0; 0; 0; 0 ]; avg_num_turns = 0.; max_turns = 0 }
      in
      assert_equal player_stats.avg_num_turns expected_stats.avg_num_turns
        ~printer:string_of_float );
    ( "get_personal_stats returns accurate stats for when a player has played \
       a single game"
    >:: fun _ ->
      let player_stats = History.get_personal_stats history_data_2 "Ellie" in
      let expected_stats : History.personal_stats =
        { turns_data = [ 0; 0; 0; 0; 0; 1 ]; avg_num_turns = 6.; max_turns = 6 }
      in
      assert_equal expected_stats.avg_num_turns player_stats.avg_num_turns
        ~printer:string_of_float );
    ( "get_personal_stats returns accurate data when multiple games have been \
       played"
    >:: fun _ ->
      let player_stats = History.get_personal_stats history_data_2 "Elijah" in
      let expected_stats : History.personal_stats =
        {
          turns_data = [ 1; 0; 0; 0; 0; 1 ];
          avg_num_turns = 3.5;
          max_turns = 6;
        }
      in
      assert_equal expected_stats player_stats );
    ( "leaderboard with one entry" >:: fun _ ->
      let leaderboard_data = History.get_leaderboard_data history_data_3 in
      let expected_leaderboard : History.leaderboard_entry list =
        [ { player = "Ellie"; average_turns = 6.; solved_games = 0 } ]
      in
      assert_equal expected_leaderboard leaderboard_data );
    ( "leaderboard with two entries" >:: fun _ ->
      let leaderboard_data = History.get_leaderboard_data history_data_4 in
      let expected_leaderboard : History.leaderboard_entry list =
        [
          { player = "Ellie"; average_turns = 5.; solved_games = 1 };
          { player = "Eli"; average_turns = 6.; solved_games = 1 };
        ]
      in
      assert_equal leaderboard_data expected_leaderboard );
  ]

let game_list : History.game_record list =
  [
    { turns = 4; solved = true; player = "AB"; answer = "TESTS" };
    { turns = 5; solved = true; player = "CD"; answer = "HEART" };
  ]

let games_list : History.game_record list =
  [
    { turns = 4; solved = false; player = "AB"; answer = "TESTS" };
    { turns = 6; solved = false; player = "EF"; answer = "WORLD" };
    { turns = 5; solved = true; player = "GH"; answer = "APPLE" };
  ]

let ab_games : History.game_record list =
  [ { turns = 4; solved = false; player = "AB"; answer = "TESTS" } ]

let ef_games : History.game_record list =
  [ { turns = 6; solved = false; player = "EF"; answer = "WORLD" } ]

let gh_games : History.game_record list =
  [ { turns = 5; solved = true; player = "GH"; answer = "APPLE" } ]

let get_player_games_test (name : string) (player : string)
    (games_list : History.game_record list)
    (expected_output : History.game_record list) =
  name >:: fun _ ->
  assert_equal
    (Wordle.History.get_player_games player games_list)
    expected_output

let get_turns_by_game_length_test (name : string)
    (game_list : History.game_record list) (player : string) (length : int)
    (expected_output : int list) =
  name >:: fun _ ->
  assert_equal
    (Wordle.History.get_turns_by_game_length game_list player length)
    expected_output ~printer:turns_printer

let get_history_turns_test (name : string)
    (categorized_history : History.game_record list) (player : string)
    (expected_output : int list) =
  name >:: fun _ ->
  assert_equal
    (Wordle.History.get_history_turns categorized_history player)
    expected_output ~printer:turns_printer

let get_games_by_length_test (name : string) (len : int)
    (games_list : History.game_record list)
    (expected_output : History.game_record list) =
  name >:: fun _ ->
  assert_equal
    (Wordle.History.get_games_by_length len games_list)
    expected_output

let additional_tests =
  [
    get_player_games_test "test_get_player_games" "AB" games_list ab_games;
    get_player_games_test "get_player_games EF" "EF" games_list ef_games;
    get_player_games_test "get_player_games GH" "GH" games_list gh_games;
    get_turns_by_game_length_test "test_get_turns_by_game_length" game_list "AB"
      5 [ 0; 0; 0; 1; 0; 0 ];
    get_turns_by_game_length_test "test_get_turns_by_game_length" game_list "CD"
      5 [ 0; 0; 0; 0; 1; 0 ];
    get_games_by_length_test "test_get_games_by_length" 5 games_list games_list;
    get_games_by_length_test "test_get_games_by_length" 5 game_list game_list;
    get_history_turns_test "test_get_history_turns" game_list "AB"
      [ 0; 0; 0; 1; 0; 0 ];
    get_history_turns_test "test_get_history_turns" game_list "CD"
      [ 0; 0; 0; 0; 1; 0 ];
  ]

let suite =
  "test suite for Wordle"
  >::: List.flatten
         [ game_play_tests; game_state_tests; history_tests; additional_tests ]

let _ = run_test_tt_main suite
