
type t = { dominante: Note.t; nom:Note.t -> string; ecarts: int list }

module type GammeSpec =
sig
  val g: t
end  

module type Note =
sig
  val n: Note.t
end

module Do: Note = struct let n = Note.C end
module Sib: Note = struct let n = Note.AD end
module Fa: Note = struct let n = Note.F end

module MakeGammeMajeure(N:Note): GammeSpec = struct
  let g = { dominante = N.n; nom = (fun x -> (Note.to_string x^"M")) ; ecarts = [2;2;1;2;2;2;1] }
end

module MakeGammeMineure(N:Note): GammeSpec = struct
  let g = { dominante = N.n; nom = (fun x -> (Note.to_string x^"m")) ; ecarts = [2;1;2;2;1;2;2] }
end

module MakeGammeChromatique(N:Note): GammeSpec = struct
  let g = { dominante = N.n; nom = (fun x -> (Note.to_string x^" chromatique")) ;
            ecarts = [1;1;1;1;1;1;1;1;1;1;1;1] }
end


module DoMajeurSeq: GammeSpec = MakeGammeMajeure(Do)
module SibMajeurSeq: GammeSpec = MakeGammeMajeure(Sib)
module FaMajeurSeq: GammeSpec = MakeGammeMajeure(Fa)

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


(* g should contain the interval. By doubling a standard gamme we
   should capture all interval until the octave. *)
let extract_interv_notes g note interv = Pp.prefix interv (Pp.remove_before note g)



module MakeGamme(G:GammeSpec): Gamme = struct
  include G
  let gamme_ =
    let rec gen dom n l =
      match l with
      | [] -> []
      | i::l' -> n:: gen dom (Note.decale_chrom n i) l'
    in
    gen G.g.dominante G.g.dominante G.g.ecarts
  let degres = List.mapi (fun i n -> (n,i)) gamme_
  (* 2 = seconde, 3 = tierce, ...  *)
  let interv note n =
    let lnotes =
      try
        if n > 0 then extract_interv_notes (degres@degres) note n
        else if n < 0 then extract_interv_notes (List.rev (degres@degres)) note (-n)
        else assert false
      with Not_found ->
        let msg = "Note non présente dans la gamme "^(G.g.nom G.g.dominante)
                  ^": "^Note.to_string note in
        failwith msg

    in
    fst (Pp.last lnotes)
  let next note = interv note 2
  let map f = List.map f gamme_
  let iter f = List.iter f gamme_
  let pr fmt () = Format.fprintf fmt "%a@.@?" Note.pr_l (map (fun x -> x))
  let seconde note = interv note 2
  let tierce note = interv note 3
  let quarte note = interv note 4
  let quinte note = interv note 5
  let sixte note = interv note 6
  let septieme note = interv note 7
  let octave note = interv note 8
end

module SibMajeur = MakeGamme(SibMajeurSeq)
module FaMajeur = MakeGamme(FaMajeurSeq)
module DoMajeur: Gamme = MakeGamme(DoMajeurSeq)
module DoChromatique: Gamme = MakeGamme(MakeGammeChromatique(Do))

type gammeStandard =
  Majeur of Note.t
| Mineur of Note.t

let gen_gamme(g:gammeStandard): (module Gamme) =
  match g with
  | Majeur dom ->
     let module Ggg = struct let n = dom end in
     let module Gg = MakeGammeMajeure(Ggg) in
     let module G = MakeGamme(Gg) in
     (module G: Gamme)
  | Mineur dom -> let module Ggg = struct let n = dom end in
     let module Gg = MakeGammeMineure(Ggg) in
     let module G = MakeGamme(Gg) in
     (module G: Gamme)



