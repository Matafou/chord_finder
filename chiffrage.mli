
(* Parsing du chiffrage de basse continue. Indépendant de la gamme voulue. *)

type raw =
  | Absolu of Note.t
  | Relative of (Note.t*int)

type indic =
  | Diese of raw
  | Bemol of raw
  | Exact of raw

(* Représente une note + des indications *)
type t = { basse: Note.t ; indics : indic list ; others: Note.t list }

(* TODO: have a map? *)
type mesure = int*t
type portee = mesure list

(* How does a note fit in a chord and in a scale/gamme? *)
type matching_note =
  | Present of Note.t (* in the chord and in the gamme *)
  | Absent of Note.t (* in the chord, NOT in the gamme *)
  | PresentAlien of Note.t (* NOT in the chord, in the gamme *)
  | AbsentAlien of Note.t (* NOT in the chord, NOT in the gamme *)

val pr: Format.formatter -> t -> unit
val pr_legend: Format.formatter -> unit -> unit
val pr_matching: Format.formatter -> matching_note -> unit
val pr_matchings: Format.formatter -> matching_note list -> unit
val pr_l_matchings: Format.formatter -> matching_note list list -> unit
val pr_chord_matchings: Format.formatter -> (Accord.t*matching_note list) -> unit
val pr_l_chord_matchings: Format.formatter -> (Accord.t*matching_note list) list -> unit

val contain_absentAlien: matching_note list -> bool
val is_relative: indic -> bool
val interp_absolute: indic -> Note.t

(* nombre de notes présentes dans une list de matchings *)
val count_present: matching_note list -> int

module type S =
sig
  module G: Gamme.S
  val interp: t -> Note.t list
  val matching: (module Gamme.S) -> Note.t list -> Accord.t -> matching_note list
  val intersect_all: Accord.t list -> Note.t list ->  (int * Accord.t) list
  val compare_matching: t -> Accord.t -> Accord.t -> int
end

(* Build a Chiffrage from a Gamme. *)
module Make: Gamme.S -> S
