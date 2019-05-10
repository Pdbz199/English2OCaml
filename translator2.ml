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
let lets = ["set"; "let"]
let prints = ["print"]
let keyword_lists = [ifs; thens; elses; lets; prints]

let decoration = ["do"; "."; ","; " in "]

let variables = ref []
let potato = !variables

let keyword_list = List.fold_left (fun acc elt -> List.rev_append elt acc) []
    keyword_lists

let rec replace_keys input_lst output start =
  match input_lst with
  | [] -> start
  | h::t -> replace_keys t output
              (Str.global_replace (Str.regexp_string h) output start)

let remove_keys input_lst start = replace_keys decoration "" start

let is_num s =
  try int_of_string (String.trim s) |> ignore; true
  with Failure _ -> false

let is_bool s =
  if s = "true" || s = "false" then true else false

let rec list_of_pairs_contains varname vars =
  match vars with
  | [] -> false
  | (name, value)::t -> if name = varname then true 
    else list_of_pairs_contains varname t

let rec get_var_value varname vars =
  match vars with 
  | [] -> ""
  | (name, value)::t -> 
    if name = varname 
    then value else get_var_value varname t

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

(** [find_keywords lst keyword_list acc] is a list of keywords that is formed
    by iterating through every element in a given [lst] and finding
    given keywords from a [keyword_list]. The values for each type
    are created by calling [complete_text] and [str_list_conversion]
    on the list following finding a keyword. *)
let rec find_keywords lst keyword_list acc = 
  match lst with 
  | [] -> acc
  | h::t -> if List.mem h keyword_list then 
      let combined_string = (str_list_conversion (complete_text t keyword_list []) "") in
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


let rec cleaned_list lst acc = 
  match lst with
  | [] -> acc
  | h::t -> match h with
    | If(s) -> cleaned_list t (If(replace_keys ["is"; "equals"] "=" s)::acc)
    | Then(s) -> cleaned_list t (Then(s)::acc)
    | Else(s) -> cleaned_list t (Else(s)::acc)
    | Let(s) -> let str = "let" ^ (replace_keys 
                                     ["= = = ="; "= = ="; "= =";] "=" (
                                     replace_keys 
                                       ["to"; "be"; "equal"] "=" s)) ^ 
                          " in\n" in
      let altered_str_lst = (String.split_on_char '=' 
                               (String.trim (replace_keys ["let"] "" str))) in
      let varname = (String.trim (List.hd altered_str_lst)) in
      let varval = (List.nth (String.split_on_char ' '
                                (List.nth altered_str_lst 1)) 1) in
      ignore(variables := (varname, varval)::!variables);
      let strng = if is_num varval || is_bool varval
        then str else replace_keys [varval] ("\"" ^ varval ^ "\"") str in
      cleaned_list t (Let(strng)::acc)
    | Print(s) -> cleaned_list t (Print(s)::acc)

(** [final_string lst acc] is a string that is formed from a given
    keywords list, [lst], and concatenates each string associated
    with that type for each element. *)
let rec final_string lst acc =
  match lst with
  | [] -> acc
  | h::t -> match h with
    | If(s) -> final_string t (acc ^ "if" ^ (replace_keys ["is"; "equals"] "=" s) ^
                               "\n")
    | Then(s) -> final_string t (acc ^ "then" ^ s ^ "\n")
    | Else(s) -> final_string t (acc ^"else" ^ s ^ "\n")
    | Let(s) -> let str = acc ^ "let" ^ (replace_keys 
                                           ["= = = ="; "= = ="; "= =";] "=" (
                                           replace_keys 
                                             ["to"; "be"; "equal"] "=" s)) ^ 
                          " in\n" in
      let altered_str_lst = (String.split_on_char '=' 
                               (String.trim (replace_keys ["let"] "" str))) in
      let varname = (String.trim (List.hd altered_str_lst)) in
      let varval = (List.nth (String.split_on_char ' '
                                (List.nth altered_str_lst 1)) 1) in
      ignore(variables := (varname, varval)::!variables);
      let strng = if is_num varval || is_bool varval
        then str else replace_keys [varval] ("\"" ^ varval ^ "\"") str in
      final_string t (strng)
    | Print(s) -> final_string t (acc ^ (
        if is_num s then "print_int" ^ s
        else if (is_bool (String.trim s))
        then ("print_string (if" ^ s ^ " = true then \"true\" else \"false\")")
        else (if list_of_pairs_contains (String.trim s) !variables 
              then let varval = get_var_value (String.trim s) !variables in
                if is_num varval then "print_int" ^ s else 
                if (is_bool (String.trim s)) then
                  ("print_string (if" ^ s ^ " = true then \"true\" 
                  else \"false\")")
                else "print_string" ^ s
              else "print_string \"" ^ (String.trim s) ^ "\"")) ^ "\n")

(** [create_final_string str] is a string that takes in a given string
    input, [str] and re-creates the string with separated keywords. *)
let create_final_string str = 
  final_string (List.rev (find_keywords (split_str str) keyword_list [])) ""

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
    | Then(s) -> eval t assoc_list condition overlapping
    | Else(s) -> let _ = Stack.pop overlapping in
      let outer_2nd = Stack.top overlapping in 
      if fst outer_2nd then
        eval t assoc_list (snd outer_2nd) overlapping
      else
        eval t assoc_list false overlapping
    | Let(s) -> eval t assoc_list condition overlapping
    | Print(s) -> if condition = true then 
        let final_str = String.trim (s) in
        if List.mem_assoc final_str assoc_list then
          List.assoc final_str assoc_list
        else
          final_str
      else
        eval t assoc_list condition overlapping

let execute_eval str = 
  let new_list = cleaned_list 
      (find_keywords (split_str str) keyword_list []) [] in
  let temp_stack = Stack.create () in let () = Stack.push (true,true) temp_stack in
  eval new_list !variables true temp_stack
