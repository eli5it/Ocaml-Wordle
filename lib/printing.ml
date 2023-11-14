(** [color_processor color] is the ANSITerminal style of [color]*)
let color_processor (color : string) : ANSITerminal.style =
  match color with
  | "red" -> ANSITerminal.red
  | "green" -> ANSITerminal.green
  | "yellow" -> ANSITerminal.yellow
  | "blue" -> ANSITerminal.blue
  | "gray" -> ANSITerminal.white
  | _ -> ANSITerminal.black

let rec print_guess (word_lst : (char * string) list) : unit =
  match word_lst with
  | [] -> Stdlib.print_string ""
  | h :: t ->
      let color = snd h in
      let letter = String.make 1 (fst h) in
      ANSITerminal.print_string [ color_processor color ] letter;
      print_guess t

let print_letters (letters : (char * string) list) : unit =
  let () = print_newline () in
  List.iter
    (fun (ch, str) ->
      ANSITerminal.print_string [ color_processor str ] (String.make 1 ch))
    letters
