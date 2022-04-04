type t = { dominante: Note.t; nom: Note.t -> string; ecarts: int list }

module type GammeSpec =
sig
  val g: t
end  

module type Note =
sig
  val n: Note.t
end

module type Gamme = sig
  val g: t
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

module SibMajeur : Gamme
module FaMajeur : Gamme
module DoMajeur: Gamme
module DoChromatique: Gamme

type gammeStandard =
  Majeur of Note.t
| Mineur of Note.t;;

val gen_gamme: gammeStandard -> (module Gamme)

