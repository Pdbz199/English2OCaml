(** [tokenize_string str] takes in a given string, [str], and returns a
    string list of substrings that were separated by a space character ' '*)
val tokenize_string: string -> string list

(** [separate_parens lst acc] is a string list that takes in a given
    string list [lst] and iterates through each element. If it finds an
    element with an open parenthesis: "(1", it separates the two and appends
    each to a new accumulator list, [acc]. It does something similar with
    closed parenthesis "(" and if the element contains neither, the element
    itself is just passed to the accumulator.*)
val separate_parens: string list -> string list -> string list