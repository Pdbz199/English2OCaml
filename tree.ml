type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
exception InvalidValueError
exception EmptyNode

(** [empty] is a tree without any nodes*)
let empty = 
  Leaf

(** [tree_compare t1 t2] compares the nodes of each tree, [t1] and [t2],
    and returns true if they are equal; false otherwise.*)
let rec tree_compare t1 t2 =
  match t1 with
  | Leaf -> begin
      match t2 with
      | Leaf -> true
      | _ -> false
    end
  | Node(v,l,r) -> begin
      match t2 with
      | Leaf -> false
      | Node(v2,l2,r2) -> if v <> v2 then false else 
          (tree_compare l l2) && (tree_compare r r2)
    end

(** [insert_left t1 t2 num] searches through a tree, [t1], until
    it finds a node with a value [num]. If it finds that node and
    the rest of the tree is equivalent to [t2] then, insert a new
    empty node to the left of that found node.*)
let rec insert_left t1 t2 num =
  match t1 with
  | Leaf -> (false,t1)
  | Node(v,l,r) -> if v = num && tree_compare t1 t2 then 
      (true,Node(v,Node("",Leaf,Leaf),r))
    else 
      let left = insert_left l t2 num in 
      if (fst (left)) then (true, Node(v,snd left,r))
      else 
        let right = insert_left r t2 num in 
        if (fst (right)) then (true, Node(v,l,snd right)) else
          (false,t1)

(** [insert_right t1 t2 num] searches through a tree, [t1], until
    it finds a node with a value [num]. If it finds that node and
    the rest of the tree is equivalent to [t2] then, insert a new
    empty node to the right of that found node.*)
let rec insert_right t1 t2 num =
  match t1 with
  | Leaf -> (false,t1)
  | Node(v,l,r) -> if v = num && tree_compare t1 t2 then 
      (true,Node(v,l,Node("",Leaf,Leaf)))
    else 
      let left = insert_right l t2 num in 
      if (fst (left)) then (true, Node(v,snd left,r))
      else 
        let right = insert_right r t2 num in 
        if (fst (right)) then (true, Node(v,l,snd right)) else
          (false,t1)

(** [change_current_val t1 t2 num new_val] is a tuple containing a boolean
    and a tree node as its 2 elements. It searches through a tree [t1], until
    it finds a node with a value [num]. If it finds that node and
    the rest of the tree is equivalent to [t2], then it replaces the value
    of that node with [new_val].*)
let rec change_current_val t1 t2 num new_val=
  match t1 with
  | Leaf -> (false,t1)
  | Node(v,l,r) -> if v = num && tree_compare t1 t2 then 
      (true,Node(new_val,l,r))
    else 
      let left = change_current_val l t2 num new_val in 
      if (fst (left)) then (true, Node(v,snd left,r))
      else 
        let right = change_current_val r t2 num new_val in 
        if (fst (right)) then (true, Node(v,l,snd right)) else
          (false,t1)

(** [change_insert t1 t2 num new_val] is a combination of [insert_right]
    and [change_current_val] to handle specialized edge cases.*)
let rec change_insert t1 t2 num new_val =
  match t1 with
  | Leaf -> (false,t1)
  | Node(v,l,r) -> if v = num && tree_compare t1 t2 then 
      (true,Node(new_val,l,Node("",Leaf,Leaf)))
    else 
      let left = change_insert l t2 num new_val in 
      if (fst (left)) then (true, Node(v,snd left,r))
      else 
        let right = change_insert r t2 num new_val in 
        if (fst (right)) then (true, Node(v,l,snd right)) else
          (false,t1)

(** [build_math_tree lst symbols acc stack current] creates a syntax tree
    based on a given string [lst] that is supposed to represent a 
    mathematical expression. It iterates through the [lst] and adds
    the value to different parts of an accumulated tree [acc] depending
    on the value - whether or not it exists in a given [symbols] list
    or an open or closed parenthesis, or just a normal number. It uses
    a given stack, [stack], to maintain parent position when needed
    to traverse back up a tree, and [current] represents the current
    position within the tree.

    Raises: InvalidValueError if the user is missing parentheses or inputs
    an invalid value within the [lst].*)
let rec build_math_tree lst symbols acc stack current=
  match lst with
  | [] -> acc
  | h::t -> if h = "(" then
      match current with
      | Leaf -> acc
      | Node(v,l,r) -> Stack.push (Node(v,Node("",Leaf,Leaf),r)) stack;
        build_math_tree t symbols (snd (insert_left acc current v)) 
          stack (Node("",Leaf,Leaf))
    else if not (List.mem h (")"::symbols)) then
      match current with
      | Leaf -> acc
      | Node(v,l,r)-> let popped = Stack.pop stack in
        build_math_tree t symbols 
          (snd (change_current_val acc current v h)) stack 
          (snd (change_current_val popped current v h))
    else if (List.mem h symbols) then
      match current with
      | Leaf -> Leaf
      | Node(v,l,r) -> (** let new_root = Node(h,l,r) in*)
        (* let new_root = snd (change_current_val acc current v h) in  *)
        Stack.push (Node(h,l,Node("",Leaf,Leaf))) stack;
        build_math_tree t symbols 
          (snd (change_insert acc current v h)) stack (Node("",Leaf,Leaf))
    else if h = ")" then
      let popped = Stack.pop stack in 
      build_math_tree t symbols acc stack popped
    else 
      raise InvalidValueError

(** [evaluate_tree tree] takes a given tree, [tree],
    produced by [build_math_tree] and
    is the evaluated numerical expression from that tree.*)
let rec evaluate_tree tree =
  match tree with
  | Leaf -> 0
  | Node(v,l,r) ->
    match l with
    | Leaf -> int_of_string v
    | _ -> match r with 
      | Leaf -> int_of_string v
      | _ -> if v = "+" then (evaluate_tree l) + (evaluate_tree r)
        else if v = "-" then (evaluate_tree l) - (evaluate_tree r)
        else if v = "/" then (evaluate_tree l) / (evaluate_tree r)
        else if v = "*" then (evaluate_tree l) * (evaluate_tree r)
        else raise InvalidValueError






