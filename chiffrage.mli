(* Parsing du chiffrage de basse continue. *)

module MapNotes: Map.S

(* a note of an indication is mandatory or not *)
type mandatoriness = Mandatory | Optional

type indic = { note: Note.t ; mandatoriness:mandatoriness ; in_gamme:bool }

(* all_notes is there only to keep the order in witch the notes were given. *)
type t = { basse: Note.t ;  all_notes: Note.t list; indics : indic MapNotes.t }

type score

val pr: Format.formatter -> t -> unit
val pr_legend: Format.formatter -> unit -> unit
val pr_matching: score -> Format.formatter -> Note.t -> unit
val pr_matchings: Format.formatter -> Accord.t*score -> unit
val pr_l_matchings: Format.formatter -> (Accord.t*score) list -> unit
val pr_chord_score: Format.formatter -> (Accord.t*score) -> unit
val pr_l_chord_score: Format.formatter -> (Accord.t*score) list -> unit

val compare_score: score -> score -> int

(* nombre de notes présentes dans une list de matchings *)
val count_present: score -> int
val contain_absentAlien: score -> bool

module type S =
sig
  module G: Gamme.S
  val interp_ast_indic: Note.t -> Ast.indic -> indic
  val interp_ast: Ast.chiffrage -> t
  val compute_adequacy: Accord.t -> t -> score
  val compare_chord: t -> Accord.t -> Accord.t -> int
end

(* Build a Chiffrage from a Gamme. *)
module Make: Gamme.S -> S
