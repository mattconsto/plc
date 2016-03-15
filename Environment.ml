exception EnvironmentReachedHead

(* Type of Environments *)
type 'a environment = Env of 'a environment * (string * 'a) list ref | Head

(* Function to look up the type of a string name variable in a type environment *)
let rec lookup env str = match env with
	| Head -> raise EnvironmentReachedHead
	| Env (parent, {contents = []}) -> lookup parent str
	| Env (parent, {contents = ((name, thing) :: gs)}) -> (match (name = str) with
		| true  -> thing
		| false -> lookup (Env (env, ref gs)) str)

let rec rebind env str thing = match env with
	| Head             -> raise EnvironmentReachedHead
	| Env (parent, gs) -> match (List.fold_left (fun a (name, thing) -> a || str = name) false !gs) with
		| true  -> gs := ((str, thing) :: !gs); env
		| false -> rebind parent str thing

(* Function to add an extra entry in to an environment *)
let bind env str thing = match env with
	| Head             -> raise EnvironmentReachedHead
	| Env (parent, gs) -> try
			rebind env str thing
		with
			| EnvironmentReachedHead -> gs := ((str, thing) :: !gs); env

let unbind env str = match env with
	| Head             -> raise EnvironmentReachedHead
	| Env (parent, gs) -> gs := List.filter (fun (name, thing) -> str != name) !gs; env

let parent env = match env with
	| Head             -> raise EnvironmentReachedHead
	| Env (parent, gs) -> parent

let extend env = Env (env, ref [])

let rec environment_to_string env = let rec entries_to_string l = match l with
	| [] -> ""
	| (h, a) :: t -> h ^ (entries_to_string t)
in match env with
	| Head -> "HEAD"
	| Env (parent, entries) -> "[" ^ (environment_to_string parent) ^ ":" ^ (entries_to_string !entries) ^ "]"
