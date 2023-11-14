exception NoMoreTurns of string
exception InvalidCharacter of string
exception IncorrectLength of string

type t = {
  turns_left : int;
  guesses : (char * string) list list;
  answer : string;
  keyboard : (char * string) array;
  solved : bool;
}

let new_game (turns : int) (solution : string) =
  {
    turns_left = turns;
    guesses = [];
    answer = solution;
    keyboard =
      [|
        ('a', "gray");
        ('b', "gray");
        ('c', "gray");
        ('d', "gray");
        ('e', "gray");
        ('f', "gray");
        ('g', "gray");
        ('h', "gray");
        ('i', "gray");
        ('j', "gray");
        ('k', "gray");
        ('l', "gray");
        ('m', "gray");
        ('n', "gray");
        ('o', "gray");
        ('p', "gray");
        ('q', "gray");
        ('r', "gray");
        ('s', "gray");
        ('t', "gray");
        ('u', "gray");
        ('v', "gray");
        ('w', "gray");
        ('x', "gray");
        ('y', "gray");
        ('z', "gray");
      |];
    solved = false;
  }

let get_turns_left state = state.turns_left
let get_guesses state = state.guesses
let get_answer state = state.answer
let get_solved state = state.solved

let is_alpha guess =
  match guess with
  | 'a' .. 'z' -> true
  | _ -> false

let rec valid_alpha_guess (guess : string) =
  if String.length guess = 0 then true
  else
    let head = String.get guess 0 in
    let tail = String.sub guess 1 (String.length guess - 1) in
    if is_alpha head then valid_alpha_guess tail else false

let list_of_string str = str |> String.to_seq |> List.of_seq

let update_letters (letters : (char * string) array)
    (letter_encoding : char * string) =
  let letter = fst letter_encoding in
  let new_color = snd letter_encoding in
  let idx = Char.code letter - 97 in
  if new_color = "green" then letters.(idx) <- (letter, "green")
  else if new_color = "yellow" && snd letters.(idx) <> "green" then
    letters.(idx) <- (letter, "yellow")
  else if new_color = "red" && snd letters.(idx) = "gray" then
    letters.(idx) <- (letter, "red")

let rec encode_guess_helper (answer : char list) (idx : int) (guess : char list)
    (letters : (char * string) array) =
  match guess with
  | [] -> []
  | h :: t ->
      if List.mem h answer then
        if List.nth answer idx = h then
          let () = update_letters letters (h, "green") in
          (h, "green") :: encode_guess_helper answer (idx + 1) t letters
        else
          let () = update_letters letters (h, "yellow") in
          (h, "yellow") :: encode_guess_helper answer (idx + 1) t letters
      else
        let () = update_letters letters (h, "red") in
        (h, "red") :: encode_guess_helper answer (idx + 1) t letters

let encode_guess (answer : string) (guess : string)
    (letters : (char * string) array) =
  let answer_list = list_of_string answer in
  let guess_list = list_of_string guess in
  encode_guess_helper answer_list 0 guess_list letters

let add_guess (state : t) (new_guess : string) =
  if state.turns_left > 0 then
    if String.length new_guess = String.length state.answer then
      if valid_alpha_guess new_guess then
        let previous_guesses = get_guesses state in
        let new_encoded_guess =
          encode_guess state.answer new_guess state.keyboard
        in
        {
          turns_left = state.turns_left - 1;
          guesses = previous_guesses @ [ new_encoded_guess ];
          answer = state.answer;
          keyboard = state.keyboard;
          solved = new_guess = state.answer;
        }
      else raise (InvalidCharacter "Input Contains Non-Alphabetical Characters")
    else raise (IncorrectLength "Input Is Not A Valid Length")
  else raise (NoMoreTurns "Game Over")

let get_letters state = Array.to_list state.keyboard
let game_over state = get_solved state || get_turns_left state = 0
