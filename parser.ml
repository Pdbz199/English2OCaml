type keyword = If of string list | Then of string list | Else of string list | Let of string list

let ifs = ["if"]
let thens = ["then"]
let elses = ["else"; "otherwise"]

let rec remove_keywords strs words acc =
  match strs with
  | [] -> acc
  | h::t -> if List.mem h words then acc else remove_keywords t words (h::acc)

let rec parse_if strs acc =
  match strs with
  | [] -> List.rev acc
  | h::t -> begin 
      match ifs with 
      | [] -> parse_if t acc
      | h2::t2 -> 
        if h2 = h 
        then parse_if t (If(remove_keywords t ifs [])::acc)
        else parse_if t acc
    end

let rec parse_then strs acc =
  match strs with
  | [] -> List.rev acc
  | h::t -> begin 
      match thens with 
      | [] -> parse_then t acc
      | h2::t2 -> 
        if h2 = h 
        then parse_then t (Then(remove_keywords t thens [])::acc)
        else parse_then t acc
    end

let rec parse_else strs acc =
  match strs with
  | [] -> List.rev acc
  | h::t -> begin 
      match elses with 
      | [] -> parse_else t acc
      | h2::t2 -> 
        if h2 = h 
        then parse_else t (Else(remove_keywords t elses [])::acc)
        else parse_else t acc
    end

let rec parse_keywords strs =
  parse_if strs []

let test = parse_keywords 
    (String.split_on_char ' ' "if true then if false then 5 else 4 else 10")