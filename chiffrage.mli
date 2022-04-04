
(* Parsing du chiffrage de basse continue. Indépendant de la gamme voulue. *)

type raw =
  | Absolu of Note.t
  | Relative of (Note.t*int)

type indic =
  | Diese of raw
  | Bemol of raw
  | Exact of raw

(* Représente une note + des indications *)
type t = { basse: Note.t ; indics : indic list }

(* PArsing de l'indication: exemples: "C 3 5#", "C 3# b5 F" *)
val parse_chiffrage : string -> t

(* Interprétation de l'indication dans une gamme donnée. interv
   correspond à une gamme. *)
val interp : (Note.t -> int -> Note.t) -> t -> Note.t list
