(** Representation of a Game of Wordle 
    This module allow for creating a new game of wordle, adding guesses to that game, and encoding added guesses with 
    colors indicating their correctness.

*)



exception NoMoreTurns of string
(** Raised when a game state has 0 turns left, and an invalid action such as adding a guess is attempted. *)

exception InvalidCharacter of string
(** Raised when a non alphabetic character is used in a guess*)


exception IncorrectLength of string
(** Raised when attempting to add a  guess of the wrong length to a game state.*)

type t
(** abstract type representing the game state *)

val new_game : int -> string -> t
(** [new_game t solution] returns an initialized game state for when a user
    first starts a new game, given the game has [t] turns and solution [solution] *)

val get_turns_left : t -> int
(** [get_turns_left t] returns the number of turns left in the game state [t] *)

val get_guesses : t -> (char * string) list list
(** [get_guesses t] returns the list of guesses in the game state [t] *)

val add_guess : t -> string -> t
(** [add_guess state guess] returns an updated state with [guess] added to the
    guess list of [state] *)

val get_letters : t -> (char * string) list
(** [get_letters state] returns the keyboard of [state] *)

val get_answer : t -> string
(** [get_answer st] is the solution to the game state [st] *)

val get_solved : t -> bool
(** [get_solved st] is true if the answer has been guessed, false otherwise *)

val game_over : t -> bool 
(** [game_over st] is true if the game is over, and false otherwise*)