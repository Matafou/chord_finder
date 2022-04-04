
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

(* Interprétation de l'indication dans une gamme donnée. *)

module type S =
sig
  val interp: t -> Note.t list
end

(* Foncteur prenant une gamme en paramètre *)
module Make: Gamme.Gamme -> S
