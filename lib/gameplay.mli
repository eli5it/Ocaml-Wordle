(** Module for operating on json files in order to extract word lists for a game of wordle. 
    This module represents a word list used for playing a game of wordle. It handles checking whether a guess is in a word list,
    and generating a random word to serve as a solution for a game
    *)


type t
(** The abstract type representing the word list *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the word list that j represents. Requires: [j] is a valid word
    list*)

val check_legality : string -> t -> bool
(** returns: [check_legality s w ] is a bool indicating whether a string [s] is in the
    word list [w] *)

val generate_soln : t -> string
(** [generate_soln word_list] returns a random word from the word_list located
    at the json file with file path [str]. Requires: [str] is a valid file path
    Raises: Not_Found is str is an invalid file path *)
