open Translator
open Tree
open Tokenize

(** [main ()] prompts the user for a functionality of the 
    program then starts it.*)
let rec main flag =
  let message = if flag then 
      "Please enter the option you would like: (eval or trans).\n"
    else "Try again. Choose either 'eval', 'trans', or 'quit'.\n" in
  ANSITerminal.(print_string [green]
                  "Welcome to our 3110 OCaml Translator.\n");
  print_endline message;
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | category -> match category with
    | "eval" ->  
      ANSITerminal.(print_string 
                      [green] "Please enter what you want to be evaluated: \n");
      let final_string = execute_eval (read_line ()) in
      print_string ("Evaluated: \n" ^ final_string ^ "\n")
    | "trans" ->  
      ANSITerminal.(print_string 
                      [green]" Please enter you want to be translated: \n");
      let final_string =  create_final_string (read_line ()) in 
      print_string ("Translated: \n" ^ final_string ^ "\n")
    | "quit" -> exit 0
    | _ -> 
      main false

(* Execute the engine. *)
let () = main true
