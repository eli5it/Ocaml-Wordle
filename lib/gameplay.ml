type t = string list

let check_legality word word_list = List.mem word word_list

let generate_soln word_list =
  let () = Random.self_init () in
  let list_len = List.length word_list in
  let random_int = Random.int (list_len - 1) in
  List.nth word_list random_int

let from_json json =
  let open Yojson.Basic.Util in
  json |> to_list |> List.rev_map (fun x -> to_string x)
