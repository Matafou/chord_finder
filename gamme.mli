
module type GammeSpec =
sig
  val dominante: Note.t
  val nom:Note.t -> string
  val ecarts: int list
end  

module type Note =
sig
  val n: Note.t
end

module type Gamme = sig
  include GammeSpec
  val map: (Note.t -> 'a) -> 'a list
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

module MakeGamme(G:GammeSpec): Gamme
module MakeGammeMajeure(N:Note): GammeSpec
module MakeGammeMineure(N:Note): GammeSpec

type gammeStandard =
  Majeur of Note.t
| Mineur of Note.t;;

val gen_gamme: gammeStandard -> (module Gamme)

