(* Thrown if and only if, we reach the Head node of our tree *)
exception EnvironmentReachedHead of string

(* Type of Environments *)
type 'a environment = Env of 'a environment * (string * 'a ref) list ref | Head

(* Function to look up the type of a string name variable in a type environment *)
let rec lookup env str = match env with
	| Head -> raise (EnvironmentReachedHead str)
	| Env (parent, {contents = []}) -> lookup parent str
	| Env (parent, {contents = ((name, thing) :: gs)}) -> (match (name = str) with
		| true  -> !thing
		| false -> lookup (Env (parent, ref gs)) str)

(* Recurse up the Environment Tree, until we have rebound our variable *)
let rec rebind env str thing = match env with
	| Head             -> raise (EnvironmentReachedHead str)
	| Env (parent, gs) -> (try
			(match (List.find (fun (name, value) -> name = str) !gs) with name, old -> old := thing)
		with Not_found -> ignore (rebind parent str thing)); env

(* Function to add an extra entry in to an environment *)
let bind env str thing = match env with
	| Head             -> raise (EnvironmentReachedHead str)
	| Env (parent, gs) -> (try
			rebind env str thing
		with
			| EnvironmentReachedHead str -> gs := ((str, ref thing) :: !gs); env)

(* Remove an entry from the tree of environments *)
let rec unbind env str = match env with
	| Head             -> env
	| Env (parent, gs) -> gs := List.filter (fun (name, thing) -> str <> name) !gs; ignore (unbind parent str); env

(* Get the parent of our environment *)
let parent env = match env with
	| Head             -> raise (EnvironmentReachedHead "Getting parent")
	| Env (parent, gs) -> parent

(* Extend our environment *)
let extend env = Env (env, ref [])
