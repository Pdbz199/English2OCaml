open OUnit2
open Tokenize
open Tree
open Translator

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let tree1 : int Tree.tree = Node(10,Leaf, Node(2, Leaf, Leaf))
(* let appendedLists = Tree.append_lists [1;2;3;4] [5;6;7] *)

let tokenized = Tokenize.tokenize_string "if true then do that else this"
let separateParens = Tokenize.separate_parens 
    (Tokenize.tokenize_string "(6 + 4)") []
let separateParens2 = Tokenize.separate_parens 
    (Tokenize.tokenize_string "6 + 4") []
let separateParens3 = Tokenize.separate_parens 
    (Tokenize.tokenize_string "(6 + 4) * 5") []

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [make_eval_num_expression_test input1 expected_output] constructs an
    OUnit test named [name] that asserts the quality of [expected_output]
    with [evaluate_tree]. *)
let make_eval_num_expression_test  
    (name : string) 
    (input1 : string list)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (let symbols = ["+";"-";"*";"/"] in 
                                    let init_stack = Stack.create () in
                                    Stack.push (Node("",Leaf,Leaf)) init_stack;
                                    let expression_tree = build_math_tree 
                                        input1 symbols 
                                        (Node("",Leaf,Leaf)) (init_stack) 
                                        (Node("",Leaf,Leaf)) in 
                                    let final_num = evaluate_tree 
                                        expression_tree in
                                    string_of_int final_num)
        ~printer:pp_string)

(** [make_translator_test name input1 expected_output] constructs an
    OUnit test named [name] that asserts the quality of [expected_output]
    with [create_final_string input1]. *)
let make_translator_test 
    (name : string) 
    (input1 : string)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (create_final_string input1)
        ~printer:pp_string)

(** [make_evaluator_test name input1 expected_output] constructs an
    OUnit test named [name] that asserts the quality of [expected_output]
    with [execute_eval input1]. *)
let make_evaluator_test 
    (name : string) 
    (input1 : string)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (execute_eval input1)
        ~printer:pp_string)

(** [make_tokenizer_test input1 expected_output] constructs an
    OUnit test named [name] that asserts the quality of [expected_output]
    with [tokenize_string input1]. *)
let make_tokenizer_test 
    (name : string) 
    (input1 : string)
    (expected_output : string list) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (tokenize_string input1)
        ~cmp:cmp_set_like_lists ~printer:(pp_list (fun x -> x)))

(** [make_separate_parens_test input1 expected_output] constructs an
    OUnit test named [name] that asserts the quality of [expected_output]
    with [separate_parens input1]. *)
let make_separate_parens_test 
    (name : string) 
    (input1 : string list)
    (expected_output : string list) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (separate_parens input1 [])
        ~cmp:cmp_set_like_lists ~printer:(pp_list (fun x -> x)))

let tokenizer_tests = [
  make_tokenizer_test ("tokenizer 1") ("if true then do that else this") 
    (["if";"true";"then";"do";"that";"else";"this"]);
  make_tokenizer_test ("tokenizer 2") (" 1 2 3 4") 
    (["";"1";"2";"3";"4"]); 
  make_tokenizer_test ("tokenizer 3") ("if true )1 (1) ") 
    (["if";"true";")1";"(1)";""]);
]

let separate_parens_tests = [
  make_separate_parens_test ("parens 1") 
    ["if";"true";"then";"do";"that";"else";"this"] 
    ["if";"true";"then";"do";"that";"else";"this"];
  make_separate_parens_test ("parens 2") 
    ["1";"2";"3";"4"] ["1";"2";"3";"4"];
  make_separate_parens_test ("parens 3") 
    ["if";"true";"1)";] ["if";"true";"1";")"];
]

let make_eval_num_expression_tests = [
  make_eval_num_expression_test ("eval 1") ["(";"1";"+";"1";")"] "2";
  make_eval_num_expression_test ("eval 2") ["(";"2";"*";"4";")"] "8";
  make_eval_num_expression_test ("eval 3") ["(";"7";"-";"1";")"] "6";
  make_eval_num_expression_test ("eval 4") ["(";"8";"/";"2";")"] "4";
  make_eval_num_expression_test ("eval 5") ["(";"0";"+";"0";")"] "0";
  make_eval_num_expression_test ("eval 6") ["(";"1";"+";"(";"1";"+";"2";")"] 
    "4";
  make_eval_num_expression_test ("eval 7") ["(";"2";"*";"(";"1";"*";"2";")"] 
    "4";
  make_eval_num_expression_test ("eval 8") ["(";"10";"-";"(";"5";"*";"2";")"] 
    "0";
  make_eval_num_expression_test ("eval 9") ["(";"10";"/";"(";"7";"-";"5";")"] 
    "5";
  make_eval_num_expression_test ("eval 10") ["(";"7";"+";"(";"100";"/";"2";")"] 
    "57";
]

let trans_expr_tests = [
  make_translator_test ("trans 1") "print [1,2,3] contains 2"
    "print_string (string_of_bool (List.mem [1;2;3] 2))\n";
  make_translator_test ("trans 2") "print [1,2,3]"
    "List.iter (Printf.printf \"%d \") [1;2;3]\n";
  make_translator_test ("trans 3")
    "let x be equal to 5. if x equals 5 then print x otherwise print 5"
    "let x = 5 in\nif x = 5\n then\nprint_int x\nelse\nprint_int 5\n";
  make_translator_test ("trans 4")
    ""
    "";
  make_translator_test ("trans 5")
    "if it is raining then bring an umbrella, otherwise don't"
    "if it = raining\nthen \"bring umbrella\"\nelse \"don't\"\n";
  make_translator_test ("trans 6")
    "assign c to [1,2,3]. if c contains 3 then print c else print false"
    "let c = [1;2;3] in if List.mem c 3\nthen\nList.iter 
    (Printf.printf \"%d \") c\nelse\nprint_string (string_of_bool false)\n"
]

let eval_tests = [
  make_evaluator_test ("final eval 1") 
    "set x to 6. if x = 7 then if 4 = 5 then 
  if 5 = 4 print x else print y else print z else print q"
    "q";
  make_evaluator_test ("final eval 2") 
    "set x to 7. if x = 7 then if 4 = 4 then 
  if 5 = 4 print x else print y else print z else print q"
    "y";
  make_evaluator_test ("final eval 3") 
    "set x to 7. if x = 7 then if 4 = 5 then 
  if 4 = 4 print x else print y else print z else print q"
    "z";
  make_evaluator_test ("final eval 4") 
    "set x to 7. if x = 7 then if 4 = 5 then 
  if 5 = 4 print x else print y else print z else print q"
    "z";
  make_evaluator_test ("final eval 5") 
    "set x to 6. if x = 7 then if 4 = 5 then 
  if 4 = 4 print x else print y else print z else print q"
    "q";
  make_evaluator_test ("final eval 6") 
    "set x to false. let c equal [hi,bye,yo]. 
  let y equal yo. if x then if c contains y 
  then print c else print x else print hello"
    "hello";
  make_evaluator_test ("final eval 7") 
    "set x to 7. set c = [1,2,3]. if x = 8 then if c contains 2 
  then print c else print x else print hello"
    "hello";
  make_evaluator_test ("final eval 8") 
    "set x to 7. set c to [1,2,3]. if x = 7 then if c contains 2 
  then print c else print x else print hello"
    "[1;2;3]";
  make_evaluator_test ("final eval 9") 
    "set x to true. let c equal [hi,bye,yo]. 
  let y equal yo. if x then if c contains y 
  then print c else print x else print hello"
    "[hi;bye;yo]";
]

let suite =
  "test suite for A2"  >::: List.flatten [
    tokenizer_tests;
    separate_parens_tests;
    make_eval_num_expression_tests;
    eval_tests
  ]
let _ = run_test_tt_main suite