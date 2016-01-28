
module GroupsSet = Set.Make (struct type t = int let compare = compare end)

let modifier_of_flag = function
  | `CASELESS -> "i"
  | `MULTILINE -> "m"
  | `DOTALL -> "s"
  | `EXTENDED -> "x"
  | `ANCHORED -> ""
  | `DOLLAR_ENDONLY -> ""
  | `EXTRA -> ""
  | `UNGREEDY -> ""
  | `NO_UTF8_CHECK -> ""
  | `NO_AUTO_CAPTURE -> ""
  | `AUTO_CALLOUT -> ""
  | `FIRSTLINE -> ""
  | `UTF8 -> "u"

let superex =
  let open Pcre in
  let rex_group_subst = subst "$1(?:$2" in
  let rex_group_matcher = regexp "([^\\\\])?\\(([^?])" in
  fun list ->
    List.map (fun (flags, regexp) ->
        let flags = List.map modifier_of_flag flags |> String.concat "" in
        let regexp = replace ~rex:rex_group_matcher ~itempl:rex_group_subst regexp in
        Printf.sprintf "((?%s)%s)" flags regexp
      ) list
    |> String.concat "|"
    |> regexp

let matched_regexps ~superex subj =
  let open Pcre in
  let captures = capturecount superex in
  let subgroups1 = captures + 1 in
  let subgroups2 = subgroups1 lsl 1 in
  let ovector = Array.make (subgroups1 + subgroups2) 0 in
  let rec loop pos set =
    try
      unsafe_pcre_exec (rflags []) superex ~pos ~subj_start:0 ~subj ~subgroups2 ovector None;

      let rec iter pos set =
        if pos < captures then (
          if ovector.((pos+1)*2) > -1 then
            iter (pos+1) (GroupsSet.add pos set)
          else
            iter (pos+1) set
        ) else
          set
      in
      let set = iter 0 set in
      loop ovector.(1) set
    with
    | _ -> set
  in
  loop 0 GroupsSet.empty
