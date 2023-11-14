(** A utility Module for interacting with the terminal
    This module deals with printing user's guesses and a representation of previously used letters.
    *)


val print_guess : (char * string) list -> unit
(** [print_guess word_lst] prints the user's guess with color according to
    [word_lst] *)

val print_letters : (char * string) list -> unit
(** [print_letters ltrs] prints letters A...Z with color coded according to the users
    guesses *)
