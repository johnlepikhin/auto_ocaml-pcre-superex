module GroupsSet : Set.S with type elt = int

(** Create super regex from list of regular expressions *)
val superex : (Pcre.cflag list * string) list -> Pcre.regexp

(** Get set of matched expressions (their numbers in source string list are returned) *)
val matched_regexps : superex:Pcre.regexp -> string -> GroupsSet.t
