
(* type t = { dominante: Note.t; nom:Note.t -> string; ecarts: int list } *)

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

module MakeGammeMajeure(N:Note): Spec = struct
  let dominante = N.n
  let nom x = Note.to_string x^" Majeure"
  let  ecarts = [2;2;1;2;2;2;1]
end

module MakeGammeMineure(N:Note): Spec = struct
  let dominante = N.n
  let nom x = Note.to_string x^" Mineure"
  let ecarts = [2;1;2;2;1;2;2] 
end


module MakeGammePentaMajeure(N:Note): Spec = struct
  let dominante = N.n
  let nom x = Note.to_string x^" PentaM"
  let ecarts = [2;2;3;2;3]
end


module MakeGammePentaMineure(N:Note): Spec = struct
  let dominante = N.n
  let nom x = Note.to_string x^" Pentam"
  let ecarts = [3;2;2;3;2] 
end

module MakeGammeBlues(N:Note): Spec = struct
  let dominante = N.n
  let nom x = Note.to_string x^" Blues"
  let ecarts = [3;2;1;1;3;2] 
end

module MakeGammeChromatique(N:Note): Spec = struct
  let dominante = N.n
  let nom x = Note.to_string x^" chromatique"
  let ecarts = [1;1;1;1;1;1;1;1;1;1;1;1]
end


module type S = sig
  include Spec
  val map: (Note.t -> 'a) -> 'a list
  val exists: (Note.t -> bool) -> bool
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


(* g should contain the interval. By doubling a standard gamme we
   should capture all interval until the octave. *)
let extract_interv_notes g note interv = Pp.prefix interv (Pp.remove_before note g)



module MakeGamme(G:Spec): S = struct
  include G
  let gamme_ =
    let rec gen dom n l =
      match l with
      | [] -> []
      | i::l' -> n:: gen dom (Note.decale_chrom n i) l'
    in
    gen G.dominante G.dominante G.ecarts
  let degres = List.mapi (fun i n -> (n,i)) gamme_
  (* 2 = seconde, 3 = tierce, ...  *)
  let interv note n =
    let lnotes =
      try
        if n > 0 then extract_interv_notes (degres@degres) note n
        else if n < 0 then extract_interv_notes (List.rev (degres@degres)) note (-n)
        else assert false
      with Not_found ->
        let msg = "Note non présente dans la gamme "^(G.nom G.dominante)
                  ^": "^Note.to_string note in
        failwith msg

    in
    fst (Pp.last lnotes)
  let next note = interv note 2
  let map f = List.map f gamme_
  let exists f = List.exists f gamme_
  let for_all f = List.for_all f gamme_
  let iter f = List.iter f gamme_
  let pr fmt () =
    let notes = map (fun x -> x) in
    let notes_octave = notes@[List.hd notes] in
    Format.fprintf fmt "%a@.@?" Note.pr_l notes_octave
  let seconde note = interv note 2
  let tierce note = interv note 3
  let quarte note = interv note 4
  let quinte note = interv note 5
  let sixte note = interv note 6
  let septieme note = interv note 7
  let octave note = interv note 8
end

module M(N:Note) (MG:Note -> Spec): S = struct
  module Spec = MG(N)
  include Spec
  module M = MakeGamme(Spec)
  include M
end

module Majeure(N:Note): S = M(N)(MakeGammeMajeure)
module Mineure(N:Note): S = M(N)(MakeGammeMineure)
module Chromatique(N:Note): S = M(N)(MakeGammeChromatique)
module PentaMajeure(N:Note): S = M(N)(MakeGammePentaMajeure)
module PentaMineure(N:Note): S = M(N)(MakeGammePentaMineure)
module Blues(N:Note): S = M(N)(MakeGammeBlues)

type gammeStandard =
  Majeur of Note.t
| Mineur of Note.t
| PentaM of Note.t
| Pentam of Note.t
| Blues of Note.t

let parseName n s =
  match s with
  | "blues"  | "Blues" -> Blues n
  | "pentam" | "Pentam" -> Pentam n
  | "pentaM" | "PentaM" -> PentaM n
  | "majeure" | "Majeure"  | "majeur" | "Majeur" -> Majeur n
  | "mineure" | "Mineure" | "mineur" | "Mineur" -> Mineur n
  | _ -> raise Not_found


let gen_gamme(g:gammeStandard): (module S) =
  match g with
  | Majeur dom ->
     let module Ggg = struct let n = dom end in
     let module Gg:S = Majeure(Ggg) in
     (module Gg: S)
  | Mineur dom ->
     let module Ggg = struct let n = dom end in
     let module Gg = Mineure(Ggg) in
     (module Gg: S)
  | PentaM dom ->
     let module Ggg = struct let n = dom end in
     let module Gg = PentaMajeure(Ggg) in
     (module Gg: S)
  | Pentam dom ->
     let module Ggg = struct let n = dom end in
     let module Gg = PentaMineure(Ggg) in
     (module Gg: S)
  | Blues dom ->
     let module Ggg = struct let n = dom end in
     let module Gg = Blues(Ggg) in
     (module Gg: S)




