type keywords =
  | If of string
  | Then of string
  | Else of string
  | Let of string
  | Print of string

exception InvalidInputError

let ifs = ["if"]
let thens = ["then"]
let elses = ["else"; "otherwise"]
let lets = ["set"; "let"; "assign"]
let prints = ["print"; "output"]
let mems = ["contains"]
let appends = ["append"]
let removes = ["remove"; "delete"]
let keyword_lists = [ifs; thens; elses; lets; prints]

let decoration = ["do"; ".";" in"]

let environment = ref []

let keyword_list = List.fold_left (fun acc elt -> List.rev_append elt acc) []
    keyword_lists

(** [replace_keys input_lst output start] is a string with all instances of
    the strings from [input_lst] in [start] replaced with [output].
    This uses Str.global_replace. *)
let rec replace_keys input_lst output start =
  match input_lst with
  | [] -> start
  | h::t -> replace_keys t output
              (Str.global_replace (Str.regexp_string h) output start)

(** [remove_keys input_lst start] is a string with all instances of the 
    strings from [input_lst] in [start] replaced with an empty string.
    This uses replace_keys. *)
let remove_keys input_lst start = replace_keys input_lst "" start

(** [is_num s] is true if [s] can be converted to an int and
    is false otherwise *)
let is_num s =
  try int_of_string (String.trim s) |> ignore; true
  with Failure _ -> false

(** [is_bool s] is true if [s] is either "true" or "false" and
    is false otherwise *)
let is_bool s =
  if s = "true" || s = "false" then true else false

(** [is_lst s] is true if [s] contains '[' and is false otherwise *)
let is_lst s =
  match String.index_opt s '[' with
  | None -> false
  | Some(_) -> true

(** [split_str str] takes in a given string, [str], and returns a
    string list of substrings that were separated by a space character ' '. *)
let split_str str = 
  (List.filter (fun x -> x <> "")
     (String.split_on_char ' ' (remove_keys decoration str)))

(** [complete_text lst keyword_list acc] is a list that is formed by
    taking in a list, [lst], and
    iterating through every element in the list until it hits an
    element that exists in a given [keyword_list]. *)
let rec complete_text lst keyword_list acc = 
  match lst with
  | [] -> List.rev acc
  | h::t -> if List.mem h keyword_list 
    then List.rev acc else (complete_text t keyword_list (h::acc))

(** [str_list_conversion lst acc] takes a given string list and
    combines each element into a full string that's separated by a space. *)
let rec str_list_conversion lst acc =
  match lst with
  | [] -> acc
  | h::t -> str_list_conversion t (acc ^ " " ^ h) 

(** [get_index elt lst acc] is the count from the starting [acc] until [elt]
    is found in [lst].
    Examples:
      get_index 1 [3;2;1] 0 -> 2
      get_index "hi" ["well";"hi";"joe"] (-1) -> 0
*)
let rec get_index elt lst acc =
  match lst with
  | [] -> -1
  | h::t -> if h = elt then acc else get_index elt t (acc + 1)

(** [mem_from_str str] is a string that parses match of "contains" from [str]
    into the proper "List.mem."
    Example: mem_from_str "if x contains 7 then do this" -> "List.mem x 7" *)
let mem_from_str str =
  let split = String.split_on_char ' ' str in
  let varInd = get_index "contains" split (-1) in
  let contained = varInd > -1 in
  if contained then 
    let left = (List.nth split varInd) in 
    let right = (List.nth split (varInd + 2)) in
    "List.mem " ^ right ^ " " ^ left
  else ""

(** [find_keywords lst keyword_list acc] is a list of keywords that is formed
    by iterating through every element in a given [lst] and finding
    given keywords from a [keyword_list]. The values for each type
    are created by calling [complete_text] and [str_list_conversion]
    on the list following finding a keyword. *)
let rec find_keywords lst keyword_list acc = 
  match lst with 
  | [] -> acc
  | h::t -> if List.mem h keyword_list then 
      let combined_string = (str_list_conversion (complete_text t keyword_list
                                                    []) "") in
      if List.mem h ifs then
        find_keywords t keyword_list (If(combined_string)::acc)
      else if List.mem h thens then
        find_keywords t keyword_list (Then(combined_string)::acc)
      else if List.mem h elses then 
        find_keywords t keyword_list (Else(combined_string)::acc)
      else if List.mem h lets then 
        find_keywords t keyword_list (Let(combined_string)::acc)
      else if List.mem h prints then
        find_keywords t keyword_list (Print(combined_string)::acc)
      else []
    else  find_keywords t keyword_list acc

(** [final_string lst acc] is a string that is formed from a given
    keywords list, [lst], and concatenates each string associated
    with that type for each element. *)
let rec final_string lst acc =
  match lst with
  | [] -> acc
  | h::t -> match h with
    | If(s) -> let memStr = mem_from_str s in
      final_string t (
        acc ^ "if" ^ (if String.length memStr > 0 then " " ^ memStr
                      else (replace_keys [" is"; " equals"] " =" s)) ^ "\n")
    | Then(s) -> final_string t (acc ^ "then" ^ s ^ "\n")
    | Else(s) -> final_string t (acc ^ "else" ^ s ^ "\n")
    | Let(s) -> let str = acc ^ "let" ^ (
        replace_keys [","] ";"
          (replace_keys 
             ["= = = ="; "= = ="; "= =";] "=" (
             replace_keys 
               ["to"; "be"; "equal"] "=" s))) ^ " in\n" in
      let altered_str_lst = (String.split_on_char '=' 
                               (String.trim (replace_keys ["let"] "" str))) in
      let varname = (String.trim (List.hd altered_str_lst)) in
      let varval = (List.nth (String.split_on_char ' '
                                (List.nth altered_str_lst 1)) 1) in
      ignore(
        if List.mem_assoc varname !environment
        then environment := (varname, varval)::
                            List.remove_assoc varname !environment
        else environment := (varname, varval)::!environment);
      let strng = if is_num varval || is_bool varval
        then str else 
          let memStr = mem_from_str str in
          if String.length memStr = 0
          then str else "let " ^ varname ^ " = " ^ memStr in
      final_string t (strng)
    | Print(s) -> final_string t (acc ^ (
        let memStr = mem_from_str s in
        if String.length memStr > 0
        then "print_string (if " ^ memStr ^
             " = true then \"true\" else \"false\")"
        else
          let input = (
            match List.assoc_opt (String.trim s) !environment with
            | None -> ""
            | Some(s) -> s) in
          let has_bracket = is_lst input in
          if has_bracket then let toCheck = (remove_keys ["[";";";"]"] input)
            in if is_num toCheck
            then "List.iter (Printf.printf \"%d \") " ^ (String.trim s)
            else "List.iter (Printf.printf \"%s \") " ^ (String.trim s)
          else if is_num s then "print_int" ^ s
          else if (is_bool (String.trim s))
          then ("print_string (string_of_bool" ^ s ^ ")")
          else (
            if List.mem_assoc (String.trim s) !environment 
            then 
              let varname = (String.trim s) in
              let varval = List.assoc varname !environment in
              if is_num varval then "print_int " ^ varname else 
              if is_bool varval then
                ("print_string (string_of_bool " ^ varval ^ ")")
              else "print_string " ^ varname
            else if is_lst s then
              if is_num (remove_keys ["[";";";"]"] s)
              then "List.iter (Printf.printf \"%d \") " ^ (String.trim s)
              else "List.iter (Printf.printf \"%s \")" ^ (String.trim s)
            else "print_string \"" ^ (String.trim s) ^ "\"")) ^ "\n"
      )

(** [create_final_string str] is a string that takes in a given string
    input, [str] and re-creates the string with separated keywords. *)
let create_final_string str = 
  replace_keys [","] ";" (final_string (List.rev (find_keywords (split_str str) 
                                                    keyword_list [])) "")

(** [cleaned_list lst acc] is a keywords list that is formed from a given
    keywords list, [lst], and processes each string associated
    with each type for each element. *)
let rec cleaned_list lst acc = 
  match lst with
  | [] -> acc
  | h::t -> match h with
    | If(s) -> cleaned_list t (If(replace_keys ["is"; "equals"] "=" s)::acc)
    | Then(s) -> cleaned_list t (Then(s)::acc)
    | Else(s) -> cleaned_list t (Else(s)::acc)
    | Let(s) -> let str = "let" ^ (
        replace_keys [","] ";"
          (replace_keys 
             ["= = = ="; "= = ="; "= =";] "=" (
             replace_keys 
               ["to"; "be"; "equal"] "=" s))) ^ " in\n" in
      let altered_str_lst = (String.split_on_char '=' 
                               (String.trim (replace_keys ["let"] "" str))) in
      let varname = (String.trim (List.hd altered_str_lst)) in
      let varval = (List.nth (String.split_on_char ' '
                                (List.nth altered_str_lst 1)) 1) in
      ignore(
        if List.mem_assoc varname !environment
        then environment := (varname, varval)::
                            List.remove_assoc varname !environment
        else environment := (varname, varval)::!environment);
      let strng = if is_num varval || is_bool varval
        then str else 
          let memStr = mem_from_str str in
          if String.length memStr = 0
          then replace_keys [varval] (varval) str 
          else "let " ^ varname ^ " = " ^ memStr in
      cleaned_list t (Let(strng)::acc)
    | Print(s) -> cleaned_list t (Print(s)::acc)

(** [convert_list str] is a string that is formed from a given
    string, [str], that is supposed to represent a list containing
    square brackets on the ends, "[" and "]", and replaces commas
    with semicolons. *)
let convert_list str =
  let stripped_str = String.sub str 1 ((String.length str)-2) in
  let cleaned_str = replace_keys [","] ";" stripped_str in
  String.split_on_char ';' cleaned_str

(** [eval keyword_list assoc_list condition overlapping] is a string that
    is created by evaluating a given list of keywords, [keyword_list].
    It replaces any variable with its given "let" equivalent through
    the association list, [assoc_list], and maintains true/false states
    through nested if statements using a boolean tuple stack in [overlapping]
    and a boolean value in [condition].

    Raises: InvalidInputError if the string has incomplete conditions
            or invalid syntax*)
let rec eval keyword_list assoc_list condition overlapping= 
  match keyword_list with
  | [] -> "Unable to Evaluate"
  | h::t -> match h with
    | If(s) -> 
      let bool = fst (Stack.top overlapping) in 
      let splitted = String.split_on_char '=' (String.trim s) in
      let splitted_boolean = String.trim (List.nth splitted 0) in 
      if List.length splitted = 1 then
        if splitted_boolean = "true" then
          let () = if bool then Stack.push (true,true) overlapping
            else Stack.push (false,true) overlapping in
          eval t assoc_list true overlapping
        else if splitted_boolean = "false" then
          let () = Stack.push (false,false) overlapping in 
          eval t assoc_list false overlapping
        else 
        if List.mem_assoc (String.trim s) assoc_list then
          let key_val = List.assoc (String.trim s) assoc_list in
          if key_val = "true" then 
            let () = if bool then Stack.push (true,true) overlapping
              else Stack.push (false,true) overlapping in
            eval t assoc_list true overlapping
          else
            let () = Stack.push (false,false) overlapping in 
            eval t assoc_list false overlapping
        else
        if List.mem "contains" (String.split_on_char ' ' (String.trim s)) then 
          let mem_str = mem_from_str (String.trim s)in
          let mem_list = String.split_on_char ' ' mem_str in
          let search = List.nth mem_list 1 in
          let lst = List.nth mem_list 2 in 
          if List.mem_assoc lst assoc_list then
            let key_val = List.assoc lst assoc_list in
            let new_lst = convert_list key_val in
            if List.mem search new_lst then
              let () = if bool then Stack.push (true,true) overlapping
                else Stack.push (false,true) overlapping in
              eval t assoc_list true overlapping
            else if List.mem_assoc search assoc_list then
              let k_v = String.trim (List.assoc search assoc_list) in
              if List.mem k_v new_lst then
                let () = if bool then Stack.push (true,true) overlapping
                  else Stack.push (false,true) overlapping in
                eval t assoc_list true overlapping
              else
                let () = Stack.push (false,false) overlapping in 
                eval t assoc_list false overlapping
            else
              let () = Stack.push (false,false) overlapping in 
              eval t assoc_list false overlapping
          else if List.mem_assoc search assoc_list then
            let key_val = List.assoc search assoc_list in
            if List.mem key_val (convert_list lst) then
              let () = if bool then Stack.push (true,true) overlapping
                else Stack.push (false,true) overlapping in
              eval t assoc_list true overlapping
            else
              let () = Stack.push (false,false) overlapping in 
              eval t assoc_list false overlapping
          else
            let new_lst = convert_list lst in 
            if List.mem search new_lst then
              let () = if bool then Stack.push (true,true) overlapping
                else Stack.push (false,true) overlapping in
              eval t assoc_list true overlapping
            else
              let () = Stack.push (false,false) overlapping in 
              eval t assoc_list false overlapping
        else
          raise InvalidInputError
      else if List.length splitted > 1 then
        let second_boolean = String.trim (List.nth splitted 1) in 
        if List.mem_assoc splitted_boolean assoc_list then
          let condition = List.assoc splitted_boolean assoc_list in
          if second_boolean = condition then
            let () = if bool then Stack.push (true,true) overlapping
              else Stack.push (false,true) overlapping in 
            eval t assoc_list true overlapping
          else
            let () = Stack.push (false,false) overlapping in 
            eval t assoc_list false overlapping
        else
        if splitted_boolean = second_boolean then
          let () = if bool then Stack.push (true,true) overlapping
            else Stack.push (false,true) overlapping in 
          eval t assoc_list true overlapping
        else
          let () = Stack.push (false,false) overlapping in 
          eval t assoc_list false overlapping
      else
        raise InvalidInputError
    | Then(s) -> let outer = Stack.top overlapping in
      if fst outer then
        eval t assoc_list condition overlapping
      else
        eval t assoc_list false overlapping
    | Else(s) -> let _ = Stack.pop overlapping in
      let outer_2nd = Stack.top overlapping in 
      if fst outer_2nd then
        eval t assoc_list (snd outer_2nd) overlapping
      else
        eval t assoc_list false overlapping
    | Let(s) -> eval t assoc_list condition overlapping
    | Print(s) -> 
      let outer = Stack.top overlapping in
      if fst outer then
        if condition = true then 
          let final_str = String.trim (s) in
          if List.mem_assoc final_str assoc_list then
            List.assoc final_str assoc_list
          else
            final_str
        else
          eval t assoc_list condition overlapping
      else
        eval t assoc_list condition overlapping

(** [execute_eval str] is a string that is formed from a given
    string, [str], that represents the user's inputted text to be evaluated.
    It converts their input to a list of keywords using [cleaned_list] and
    calls [eval] to create the final string. *)
let execute_eval str = 
  let new_list = cleaned_list 
      (find_keywords (split_str str) keyword_list []) [] in
  let temp_stack = Stack.create () in 
  let () = Stack.push (true,true) temp_stack in
  eval new_list !environment true temp_stack
