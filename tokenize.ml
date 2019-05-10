(** [tokenize_string str] takes in a given string, [str], and returns a
    string list of substrings that were separated by a space character ' '*)
let tokenize_string str =
  String.split_on_char ' ' str

(** [separate_parens lst acc] is a string list that takes in a given
    string list [lst] and iterates through each element. If it finds an
    element with an open parenthesis: "(1", it separates the two and appends
    each to a new accumulator list, [acc]. It does something similar with
    closed parenthesis "(" and if the element contains neither, the element
    itself is just passed to the accumulator.*)
let rec separate_parens lst acc =
  match lst with
  | [] -> (List.rev acc)
  | h::t -> let open_parens = String.index_opt h '(' in
    let close_parens = String.index_opt h ')' in
    if (String.length h > 1) then 
      match open_parens with
      | Some(index) -> separate_parens t ((String.sub h (index+1) 
                                             ((String.length h)-1))::"("::acc)
      | None -> match close_parens with
        | Some(i) -> separate_parens t (")"::(String.sub h (0) 
                                                ((String.length h)-1))::acc)
        | None -> separate_parens t (h::acc)
    else separate_parens t (h::acc)