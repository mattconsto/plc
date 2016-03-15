exception EnvironmentReachedHead

type 'a environment = Env of 'a environment * (string * 'a) list ref | Head

val lookup : 'a environment -> string -> 'a
val rebind : 'a environment -> string -> 'a -> 'a environment
val bind   : 'a environment -> string -> 'a -> 'a environment
val unbind : 'a environment -> string -> 'a environment
val extend : 'a environment -> 'a environment
val parent : 'a environment -> 'a environment
