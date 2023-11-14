module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js
module Dom_Html = Js_of_ocaml.Dom_html
module Dom_events = Js_of_ocaml.Dom_events
module Dom = Js_of_ocaml.Dom
open Wordle.Gameplay

let five_letter_words_json =
  Js.to_string (Js.Unsafe.eval_string " JSON.stringify(fiveLetters)")

let json_data = Yojson.Basic.from_string five_letter_words_json
let history_json_string = Js.to_string (Js.Unsafe.eval_string "gameHistory")
let history_json = Yojson.Basic.from_string history_json_string
let history_records = Wordle.History.read_from_json history_json
let leaderboard_records = Wordle.History.get_leaderboard_data history_records
let word_list = ref (from_json json_data)
let solution = ref (Wordle.Gameplay.generate_soln !word_list)
let current_letter = ref 1
let current_word_length = ref (String.length !solution)
let current_word = ref ""
let saved_player = ref ""
let initial_game_state = Wordle.State.new_game 6 !solution
let current_game = ref initial_game_state
let document = Html.window##.document

let get_dom_element_by_id query_string =
  Js.Opt.get
    (document##getElementById (Js.string query_string))
    (fun () -> assert false)

let get_dom_element_by_class query_string =
  Js.Opt.get
    (document##querySelector (Js.string ("." ^ query_string)))
    (fun () -> assert false)

let pop_current_word () =
  let current_word_val = !current_word in
  let word_length = String.length current_word_val in
  if word_length > 1 then
    let new_string = String.sub current_word_val 0 (word_length - 1) in
    current_word := new_string
  else current_word := ""

let update_letter el text = el##.innerText := Js.string text
let key_code_to_letter (code : int) = String.make 1 (char_of_int code)

let play_turn (letter : string) =
  let completed_word = String.length !current_word = !current_word_length in
  let game_over = Wordle.State.game_over !current_game in
  if not (game_over || completed_word) then (
    let query_string = "letter-" ^ string_of_int !current_letter in
    let dom_element = get_dom_element_by_id query_string in
    update_letter dom_element letter;
    current_letter := !current_letter + 1;
    current_word := !current_word ^ letter)
  else ()

let color_letters (letter_colors : string list) =
  let start = !current_letter - !current_word_length in
  let idx = ref 0 in
  for i = start to start + (!current_word_length - 1) do
    ignore i;
    let color = List.nth letter_colors !idx in
    let query_string = "letter-" ^ string_of_int i in
    let dom_element = get_dom_element_by_id query_string in
    let class_list = dom_element##.classList in
    class_list##add (Js.string color);
    idx := !idx + 1
  done

let update_keyboard_letter ((letter, color) : char * string) =
  let query_string = "keyboard-" ^ String.make 1 letter in
  let dom_letter = get_dom_element_by_id query_string in
  let class_list = dom_letter##.classList in
  class_list##remove (Js.string "red");
  class_list##remove (Js.string "yellow");
  class_list##remove (Js.string "green");
  class_list##add (Js.string color)

let rec render_keyboard (keyboard : (char * string) list) =
  match keyboard with
  | [] -> ()
  | h :: t ->
      update_keyboard_letter h;
      render_keyboard t

(* recolors every keyboard button to be gray. Precondition is that ref with
   current game_state has been updated already*)
let remove_keyboard_styling () =
  let game_state = !current_game in
  let blank_keyboard = Wordle.State.get_letters game_state in
  render_keyboard blank_keyboard

(* removes previous game board from dom, so that a new one can be rendered*)
let delete_game_board () =
  let main = get_dom_element_by_id "main" in
  let game_board = get_dom_element_by_id "game-board" in
  let new_game_board = Html.createDiv document in

  new_game_board##.id := Js.string "game-board";
  new_game_board##.className := Js.string "game-board";
  Dom.removeChild main game_board;
  Dom.appendChild main new_game_board

(* will create the grid with word length columns and num_guesses columns*)
let render_game_board rows columns =
  let game_board = get_dom_element_by_id "game-board" in
  let letter_number = ref 1 in
  for i = 0 to rows - 1 do
    ignore i;
    let current_row_div = Html.createDiv document in
    ignore (current_row_div##.className := Js.string "game-row");
    for j = 0 to columns - 1 do
      ignore j;
      let new_letter_div = Html.createDiv document in
      new_letter_div##.className := Js.string "letter-container";
      new_letter_div##.id := Js.string ("letter-" ^ string_of_int !letter_number);
      Dom.appendChild current_row_div new_letter_div;
      letter_number := !letter_number + 1
    done;
    Dom.appendChild game_board current_row_div
  done

let replay_game () =
  let new_solution = Wordle.Gameplay.generate_soln !word_list in
  let new_game_state = Wordle.State.new_game 6 new_solution in
  current_letter := 1;
  current_word := "";
  current_game := new_game_state;
  current_word_length := String.length new_solution;
  delete_game_board ();
  remove_keyboard_styling ();
  render_game_board 6 (String.length new_solution)

let hide_modal () =
  let modal = get_dom_element_by_class "modal" in
  Dom.removeChild (get_dom_element_by_id "root") modal

let add_replay_handler button =
  button##.onclick :=
    Html.handler (fun _ ->
        replay_game ();
        hide_modal ();
        Js._true)

let create_relative_size (relative_height : float) =
  let percent_string = string_of_float relative_height in
  let clipped_percent_string =
    String.sub percent_string 0 (String.length percent_string - 1) ^ "%"
  in
  clipped_percent_string

let create_guess_row (idx : int) (count : int) style =
  let guess_row = Html.createDiv document in
  let number_container = Html.createSpan document in
  let guess_bar = Html.createDiv document in
  let bar_container = Html.createDiv document in
  let count_container = Html.createDiv document in
  count_container##.innerText := Js.string (string_of_int count);
  count_container##.className := Js.string "guess-bar-count";
  bar_container##.className := Js.string "bar-container";
  if count <> 0 then guess_bar##.style##.cssText := Js.string style else ();

  number_container##.innerText := Js.string (string_of_int idx);
  guess_bar##.classList##add (Js.string "guess-bar");
  guess_row##.classList##add (Js.string "guess-row");

  guess_bar##.classList##add (Js.string ("guess-" ^ string_of_int count));
  Dom.appendChild guess_bar count_container;
  Dom.appendChild guess_row number_container;
  Dom.appendChild bar_container guess_bar;
  Dom.appendChild guess_row bar_container;
  guess_row

let create_guess_display guesses =
  let container = Html.createDiv document in
  container##.classList##add (Js.string "guess-display-container");
  let current_game_guess_count =
    6 - Wordle.State.get_turns_left !current_game
  in
  let updated_guesses =
    List.mapi
      (fun idx n -> if idx + 1 = current_game_guess_count then n + 1 else n)
      guesses
  in
  let max_guess =
    List.fold_left (fun acc x -> if x > acc then x else acc) 0 updated_guesses
  in
  for i = 0 to List.length guesses - 1 do
    let count = List.nth updated_guesses i in
    let relative_width = float_of_int count /. float_of_int max_guess *. 100. in
    let min_width_style = "min-width: 1.1rem; " in
    let width_style = "width: " ^ create_relative_size relative_width in
    let css_style = min_width_style ^ width_style in

    let new_row = create_guess_row (i + 1) count css_style in
    Dom.appendChild container new_row
  done;
  container

(* alerts user that they have won / lost*)
let display_end_game_modal game_state =
  let player_won = Wordle.State.get_solved game_state in
  let replay_button_container = Html.createDiv document in
  let modal_div = Html.createDiv document in
  let modal_content = Html.createDiv document in
  let header = Html.createH1 document in
  let statsHeading = Html.createH2 document in
  let replay_button = Html.createButton document in
  let guesses =
    Wordle.History.get_history_turns history_records !saved_player
  in

  let guess_display = create_guess_display guesses in
  replay_button_container##.className := Js.string "replay-button-container";
  replay_button##.innerText := Js.string "Play Again?";
  replay_button##.className := Js.string "replay-button";
  header##.className := Js.string "modal-header";
  statsHeading##.innerText := Js.string "Guess Distribution";

  modal_div##.className := Js.string "modal";
  modal_content##.id := Js.string "modal-content";
  if player_won then header##.innerText := Js.string "You win"
  else header##.innerText := Js.string "You Lose";
  modal_div##.onclick :=
    Html.handler (fun _ ->
        hide_modal ();
        Js._true);
  modal_content##.onclick :=
    Html.handler (fun e ->
        Dom_Html.stopPropagation e;
        Js._true);
  add_replay_handler replay_button;
  Dom.appendChild replay_button_container replay_button;
  Dom.appendChild modal_content header;
  Dom.appendChild modal_content statsHeading;
  Dom.appendChild modal_content guess_display;

  Dom.appendChild modal_content replay_button_container;
  Dom.appendChild modal_div modal_content;
  Dom.appendChild (get_dom_element_by_id "root") modal_div

let handle_enter_key () =
  let current_word_val = !current_word in
  if String.length current_word_val <> !current_word_length then ()
    (* don't do anything*)
  else
    let game_state = !current_game in
    let lower_cased_guess = String.lowercase_ascii !current_word in
    let new_state = Wordle.State.add_guess game_state lower_cased_guess in
    let most_recent_guess =
      List.hd (List.rev (Wordle.State.get_guesses new_state))
    in
    let letter_colors = snd (List.split most_recent_guess) in
    color_letters letter_colors;
    current_word := "";
    current_game := new_state;
    let updated_keyboard = Wordle.State.get_letters new_state in
    render_keyboard updated_keyboard;
    if Wordle.State.game_over !current_game then
      display_end_game_modal !current_game
    else ()

let handle_delete_key (dom_element : Dom_Html.element Js.t) =
  update_letter dom_element "";
  pop_current_word ();
  let current_letter_val = !current_letter in
  if current_letter_val > 1 then current_letter := !current_letter - 1 else ()

let keydown_handler (ev : Dom_Html.keyboardEvent Js.t) =
  let key_code = ev##.keyCode in
  if key_code = 8 && !current_word <> "" && !current_letter <> 1 then
    let current_letter_val = !current_letter in
    let query_string = "letter-" ^ string_of_int (current_letter_val - 1) in
    let dom_element = get_dom_element_by_id query_string in
    handle_delete_key dom_element
  else if key_code = 13 then handle_enter_key ()
  else if key_code < 65 || key_code > 90 then ()
  else play_turn (key_code_to_letter key_code)

let add_keydown_listener () =
  let keydown_handler =
    Html.handler (fun ev ->
        Dom.preventDefault ev;
        ignore (keydown_handler ev);
        Js._true)
  in
  document##.onkeydown := keydown_handler

let letter_string_of_int (num : int) =
  match num with
  | 4 -> "four"
  | 5 -> "five"
  | 6 -> "six"
  | 7 -> "seven"
  | 8 -> "eight"
  | _ -> failwith "Error: unsupported word length"

(* deletes previous gameboard, reset all state, and render new gameboard of
   different size*)
let render_new_game word_length =
  let num_string = letter_string_of_int word_length in
  let json_data_name = num_string ^ "Letters" in
  let json_string =
    Js.to_string
      (Js.Unsafe.eval_string ("JSON.stringify" ^ "(" ^ json_data_name ^ ")"))
  in
  let json_data = Yojson.Basic.from_string json_string in
  word_list := from_json json_data;
  let new_solution = generate_soln !word_list in
  let new_game = Wordle.State.new_game 6 new_solution in
  current_game := new_game;
  current_word := "";
  current_letter := 1;
  current_word_length := String.length new_solution;
  solution := new_solution;
  delete_game_board ();
  remove_keyboard_styling ();
  render_game_board 6 word_length

(* removes styling from old selected button, and adds it to the new selected
   one*)
let change_button_styling (old_button_num : int) (new_button_num : int) =
  let old_button =
    get_dom_element_by_id ("button-" ^ string_of_int old_button_num)
  in
  let new_button =
    get_dom_element_by_id ("button-" ^ string_of_int new_button_num)
  in
  old_button##.classList##remove (Js.string "selected");
  new_button##.classList##add (Js.string "selected")

(* will add the event listeners to the buttons so that onClick , delete's
   current gameboard and renders new one*)

let add_button_listeners () =
  for i = 4 to 8 do
    let button = get_dom_element_by_id ("button-" ^ string_of_int i) in
    button##.onclick :=
      Html.handler (fun _ ->
          change_button_styling !current_word_length i;
          render_new_game i;
          Js._false)
  done

(* code inpsired by documentation example found at
   https://ocsigen.org/js_of_ocaml/latest/api/js_of_ocaml/Js_of_ocaml/Dom_html/index.html#document-objects *)
let update_player () =
  match Dom_Html.getElementById_coerce "input" Dom_Html.CoerceTo.input with
  | None -> failwith "invalid input"
  | Some input ->
      let text_value = input##.value in
      saved_player := Js.to_string text_value

let display_person_modal () =
  let modal_div = Html.createDiv document in
  let modal_content = Html.createDiv document in
  let modal_header = Html.createH1 document in
  let input = Html.createInput document in
  let submit_button = Html.createButton document in
  modal_header##.innerText := Js.string "Please enter your initials:";
  submit_button##.innerText := Js.string "submit";
  submit_button##.className := Js.string "login-button";
  modal_content##.id := Js.string "user-modal-content";
  modal_div##.className := Js.string "modal";
  input##.className := Js.string "input";
  input##.id := Js.string "input";

  modal_div##.onclick :=
    Html.handler (fun _ ->
        hide_modal ();

        Js._true);
  modal_content##.onclick :=
    Html.handler (fun e ->
        Dom_Html.stopPropagation e;
        Js._true);

  submit_button##.onclick :=
    Html.handler (fun _ ->
        update_player ();
        hide_modal ();
        add_keydown_listener ();
        Js._true);

  Dom.appendChild modal_content modal_header;
  Dom.appendChild modal_content input;
  Dom.appendChild modal_content submit_button;
  Dom.appendChild modal_div modal_content;
  Dom.appendChild (get_dom_element_by_id "root") modal_div

(* clears out the game state that was previously saved*)
let clear_game_state () =
  current_letter := 1;
  current_word_length := 5;
  current_word := "";
  current_game := Wordle.State.new_game 6 "horse"

let clear_main_display () =
  let body = get_dom_element_by_id "body" in
  let root = get_dom_element_by_id "root" in
  let new_root = Html.createDiv document in
  new_root##.id := Js.string "root";
  Dom.removeChild body root;
  Dom.appendChild body new_root

let rec create_game_bars (turns_data : int list) (max_turns : float)
    (container : Dom_Html.divElement Js.t) =
  match turns_data with
  | [] -> ()
  | h :: t ->
      let relative_height = float_of_int h /. max_turns *. 100. in
      let game_bar = Html.createDiv document in

      let relative_size_str = create_relative_size relative_height in
      let min_height_style = "min-height: 1rem; " in

      let height_style = "height: " ^ relative_size_str in
      let css_style = min_height_style ^ height_style in
      game_bar##.style##.cssText := Js.string css_style;
      game_bar##.classList##add (Js.string "personal-stats-bar");

      Dom.appendChild container game_bar;
      create_game_bars t max_turns container

let create_personal_stats_display
    (personal_stats : Wordle.History.personal_stats) =
  let personal_stats_display = Html.createDiv document in
  let sidebar = Html.createDiv document in
  let games_label = Html.createDiv document in
  games_label##.innerText := Js.string "Percent of Max";

  let side_labels = Html.createDiv document in
  let zero_percent_label = Html.createDiv document in
  let hundred_percent_label = Html.createDiv document in
  zero_percent_label##.innerText := Js.string "0";
  hundred_percent_label##.innerText := Js.string "100";
  zero_percent_label##.className := Js.string "zero-percent-label";
  hundred_percent_label##.className := Js.string "hundred-percent-label";

  side_labels##.className := Js.string "percent-labels";

  let game_chart = Html.createDiv document in
  let labels = Html.createDiv document in
  let turns_text_container = Html.createDiv document in
  let turns_text_node = Html.createH2 document in
  turns_text_container##.className := Js.string "game-chart-turns";
  sidebar##.className := Js.string "game-chart-sidebar";
  turns_text_node##.innerText := Js.string "Turns";
  let relative_label = Html.createDiv document in
  relative_label##.className := Js.string "relative-label";
  relative_label##.innerText := Js.string "1";
  let max_turns =
    List.fold_left
      (fun x acc -> if x > acc then x else acc)
      0 personal_stats.turns_data
  in
  create_game_bars personal_stats.turns_data (float_of_int max_turns) game_chart;
  game_chart##.className := Js.string "game-chart";
  personal_stats_display##.className := Js.string "personal-stats-display";
  labels##.className := Js.string "labels";

  Dom.appendChild relative_label sidebar;
  Dom.appendChild labels relative_label;

  for i = 2 to 6 do
    let new_label = Html.createDiv document in
    new_label##.className := Js.string "label";
    new_label##.innerText := Js.string (string_of_int i);
    Dom.appendChild labels new_label
  done;

  Dom.appendChild side_labels zero_percent_label;
  Dom.appendChild side_labels hundred_percent_label;
  Dom.appendChild sidebar games_label;
  Dom.appendChild sidebar side_labels;

  Dom.appendChild turns_text_container turns_text_node;
  Dom.appendChild personal_stats_display game_chart;
  Dom.appendChild personal_stats_display labels;
  Dom.appendChild personal_stats_display turns_text_node;
  personal_stats_display

let display_personal_stats (personal_stats : Wordle.History.personal_stats) =
  clear_main_display ();
  let root = get_dom_element_by_id "root" in
  let container = Html.createDiv document in
  let heading = Html.createH1 document in
  heading##.innerText := Js.string ("Hello " ^ !saved_player);
  heading##.className := Js.string "text-center";
  let personal_stats_display = create_personal_stats_display personal_stats in
  Dom.appendChild container heading;
  Dom.appendChild container personal_stats_display;
  Dom.appendChild root container

let create_game_buttons () =
  let header = Html.createH2 document in
  let section_container = Html.createDiv document in
  header##.innerText := Js.string "Word Length";
  let button_container = Html.createDiv document in
  button_container##.className := Js.string "button-container";
  button_container##.id := Js.string "button-container";
  for i = 4 to 8 do
    let new_button = Html.createDiv document in
    new_button##.className := Js.string "new-word-length-button";
    new_button##.id := Js.string ("button" ^ "-" ^ string_of_int i);
    new_button##.innerText := Js.string (string_of_int i);
    if i = 5 then new_button##.classList##add (Js.string "selected") else ();
    Dom.appendChild button_container new_button
  done;
  Dom.appendChild section_container header;
  Dom.appendChild section_container button_container;
  section_container

let rec create_keyboard_row_helper (letters : string list)
    (row : Dom_Html.divElement Js.t) =
  match letters with
  | [] -> row
  | h :: t ->
      let keyboard_button = Html.createButton document in
      keyboard_button##.className := Js.string "new-keyboard-button";
      keyboard_button##.id := Js.string ("keyboard-" ^ h);
      keyboard_button##.innerText := Js.string h;
      Dom.appendChild row keyboard_button;
      create_keyboard_row_helper t row

let create_keyboard_row (letters : string list) (is_third : bool) =
  let initial_div = Html.createDiv document in
  if is_third then (
    let enter_button = Html.createButton document in
    enter_button##.className := Js.string "new-enter-button";
    enter_button##.innerText := Js.string "Enter";
    Dom.appendChild initial_div enter_button)
  else ();

  let keyboard_row = create_keyboard_row_helper letters initial_div in
  keyboard_row##.className := Js.string "keyboard-row";
  if is_third then (
    let delete_button = Html.createButton document in
    delete_button##.className := Js.string "delete-button";
    delete_button##.innerText := Js.string "Del";
    Dom.appendChild keyboard_row delete_button)
  else ();

  keyboard_row

let create_new_keyboard () =
  let keyboard = Html.createDiv document in
  keyboard##.className := Js.string "keyboard";
  keyboard##.id := Js.string "keyboard";
  let letters_1 = [ "q"; "w"; "e"; "r"; "t"; "y"; "u"; "i"; "o"; "p" ] in
  let letters_2 = [ "a"; "s"; "d"; "f"; "g"; "h"; "j"; "k"; "l" ] in
  let letters_3 = [ "z"; "x"; "c"; "v"; "b"; "n"; "m" ] in
  let row_1 = create_keyboard_row letters_1 false in
  let row_2 = create_keyboard_row letters_2 false in
  let row_3 = create_keyboard_row letters_3 true in
  row_2##.classList##add (Js.string "keyboard-center");
  Dom.appendChild keyboard row_1;
  Dom.appendChild keyboard row_2;
  Dom.appendChild keyboard row_3;
  keyboard

(* renders the initial game page *)
let render_game_page () =
  add_keydown_listener ();
  clear_main_display ();
  clear_game_state ();
  let new_keyboard = create_new_keyboard () in
  let game_container = Html.createDiv document in

  game_container##.id := Js.string "game-board";
  game_container##.className := Js.string "game-board";

  let game_buttons = create_game_buttons () in
  let main = Html.createDiv document in
  let root = get_dom_element_by_id "root" in

  main##.id := Js.string "main";

  Dom.appendChild main new_keyboard;
  Dom.appendChild main game_container;
  Dom.appendChild (get_dom_element_by_id "root") game_buttons;

  Dom.appendChild root main;
  render_new_game 5

let remove_keydown_listener () = document##.onkeydown := Dom_Html.no_handler

(* renders the personal leadboard stats*)
let render_personal_stats () =
  remove_keydown_listener ();
  let personal_stats =
    Wordle.History.get_personal_stats history_records !saved_player
  in
  clear_game_state ();
  display_personal_stats personal_stats

let get_percent_string (num : float) =
  let str = string_of_float num in
  if String.length str = 2 then String.sub str 0 2 ^ "00"
  else if String.length str = 3 then String.sub str 0 3 ^ "0"
  else String.sub str 0 4

let rec create_leaderboard_content_helper
    (leaderboard_records : Wordle.History.leaderboard_entry list)
    (container : Dom_Html.tableElement Js.t) (odd : bool) (idx : int) =
  match leaderboard_records with
  | [] -> ()
  | h :: t ->
      let new_entry = Html.createTr document in
      let num_data = Html.createTd document in
      let name_data = Html.createTd document in
      let average_turns_data = Html.createTd document in
      let solved_data = Html.createTd document in
      num_data##.innerText := Js.string (string_of_int idx ^ ".");
      name_data##.innerText := Js.string h.player;
      solved_data##.innerText := Js.string (string_of_int h.solved_games);

      average_turns_data##.innerText
      := Js.string (get_percent_string h.average_turns);

      Dom.appendChild new_entry num_data;
      Dom.appendChild new_entry name_data;
      Dom.appendChild new_entry average_turns_data;
      Dom.appendChild new_entry solved_data;
      Dom.appendChild container new_entry;
      create_leaderboard_content_helper t container (not odd) (idx + 1)

let create_leaderboard_content () =
  let leaderboard_content = Html.createTable document in
  let top_row = Html.createTr document in
  let num_head = Html.createTh document in
  let name_head = Html.createTh document in
  let turns_head = Html.createTh document in
  let solved_head = Html.createTh document in
  name_head##.innerText := Js.string "Player";
  turns_head##.innerText := Js.string "Average Turns";
  solved_head##.innerText := Js.string "Games Solved";
  Dom.appendChild top_row num_head;
  Dom.appendChild top_row name_head;
  Dom.appendChild top_row turns_head;
  Dom.appendChild top_row solved_head;
  Dom.appendChild leaderboard_content top_row;
  create_leaderboard_content_helper leaderboard_records leaderboard_content true
    1;
  leaderboard_content

let render_leaderboard () =
  clear_game_state ();
  remove_keydown_listener ();
  clear_main_display ();
  let h1 = Html.createH1 document in
  h1##.innerText := Js.string "Leaderboard";
  let root = get_dom_element_by_id "root" in
  let leadboard_container = Html.createDiv document in
  leadboard_container##.className := Js.string "leaderboard-container";
  let leaderboard_content = create_leaderboard_content () in
  Dom.appendChild leadboard_container h1;
  Dom.appendChild leadboard_container leaderboard_content;
  Dom.appendChild root leadboard_container

let add_leaderboard_click_handler () =
  let leaderboard_button = get_dom_element_by_class "display-stats-button" in
  leaderboard_button##.onclick
  := Html.handler (fun _ ->
         ignore (render_leaderboard ());
         Js._true)

let add_title_click_handler () =
  let main_header = get_dom_element_by_class "main-header" in
  main_header##.onclick :=
    Html.handler (fun _ ->
        ignore (render_game_page ());
        add_button_listeners ();
        Js._true)

let add_login_popup_handler () =
  let login_button = get_dom_element_by_class "login-button" in
  login_button##.onclick :=
    Html.handler (fun _ ->
        remove_keydown_listener ();
        if !saved_player = "" then display_person_modal ()
        else render_personal_stats ();
        Js._true)

let start () = add_keydown_listener ()

let _ =
  Html.window##.onload :=
    Html.handler (fun _ ->
        ignore (start ());
        add_button_listeners ();
        add_login_popup_handler ();
        add_title_click_handler ();
        add_leaderboard_click_handler ();
        render_game_board 6 5;
        Js._false)
