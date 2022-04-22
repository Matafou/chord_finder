
module type Spec =
sig
  val dominante: Note.t
  val nom:Note.t -> string
  val ecarts: int list
end  

module type Note =
sig
  val n: Note.t
end

module type S = sig
  include Spec
  val map: (Note.t -> 'a) -> 'a list
  val exists: (Note.t -> bool) -> bool
  val mem: Note.t -> bool
  val for_all: (Note.t -> bool) -> bool
  val iter: (Note.t -> unit) -> unit
  val pr: Format.formatter -> unit -> unit
  val next: Note.t -> Note.t
  val interv: Note.t -> int -> Note.t
  val seconde: Note.t -> Note.t
  val tierce: Note.t -> Note.t
  val quarte: Note.t -> Note.t
  val quinte: Note.t -> Note.t
  val sixte: Note.t -> Note.t
  val septieme: Note.t -> Note.t
  val octave: Note.t -> Note.t
end

module MakeGamme(G:Spec): S
module Majeure: Note -> S
module Mineure: Note -> S
module Chromatique: Note -> S
module PentaMajeure: Note -> S

type gammeStandard =
  Majeur of Note.t
| Mineur of Note.t
| PentaM of Note.t
| Pentam of Note.t
| Blues of Note.t

val parseName: Note.t -> string -> gammeStandard
val gen_gamme: gammeStandard -> (module S)

