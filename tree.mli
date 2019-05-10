(** The type ['a tree'] represents a tree that holds ['a] values that 
    can be either empty ([Leaf]) or not ([Node]) *)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

exception InvalidValueError

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
val build_math_tree: string list ->
  string list ->
  string tree -> string tree Stack.t -> string tree -> string tree

(** [evaluate_tree tree] takes a given tree, [tree],
    produced by [build_math_tree] and
    is the evaluated numerical expression from that tree.*)
val evaluate_tree: string tree -> int