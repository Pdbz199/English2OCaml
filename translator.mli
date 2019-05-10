(** [create_final_string str] is a string that takes in a given string
    input, [str] and re-creates the string with separated keywords.*)
val create_final_string: string->string

(** [execute_eval str] is a string that is formed from a given
    string, [str], that represents the user's inputted text to be evaluated.
    It converts their input to a list of keywords using [cleaned_list] and
    calls [eval] to create the final string. *)
val execute_eval: string -> string