(* Thrown if and only if, we reach the Head node of our tree *)
exception EnvironmentReachedHead

(* Type of Environments *)
type 'a environment = Env of 'a environment * (string * 'a) list ref | Head

(* Function to look up the type of a string name variable in a type environment *)
val lookup : 'a environment -> string -> 'a
(* Recurse up the Environment Tree, until we have rebound our variable *)
val rebind : 'a environment -> string -> 'a -> 'a environment
(* Function to add an extra entry in to an environment *)
val bind   : 'a environment -> string -> 'a -> 'a environment
(* Remove an entry from the tree of environments *)
val unbind : 'a environment -> string -> 'a environment
(* Get the parent of our environment *)
val extend : 'a environment -> 'a environment
(* Extend our environment *)
val parent : 'a environment -> 'a environment
