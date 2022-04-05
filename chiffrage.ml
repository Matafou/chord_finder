(* Les intervales en notation "chiffrage d'accord".
   https://fr.wikipedia.org/wiki/Chiffrage_des_accords *)

type raw =
  | Absolu of Note.t
  | Relative of (Note.t*int)

type indic =
  | Diese of raw
  | Bemol of raw
  | Exact of raw

type t = { basse: Note.t ; indics : indic list }

(* A given note was given by the user, how much does it fit in the
   chord and in the gamme? *)
type matching_note =
  | Present of Note.t (* in the chord and in the gamme *)
  | Absent of Note.t (* in the chord, NOT in the gamme *)
  | PresentAlien of Note.t (* NOT in the chord, in the gamme *)
  | AbsentAlien of Note.t (* NOT in the chord, NOT in the gamme *)


let count_present (lm:matching_note list):int=
  List.length (List.filter (function | Present _ | PresentAlien _ -> true | _ -> false) lm)

let pr_legend fmt () =
  Format.fprintf fmt "%s, %s, %s, %s"
    (Pp.str [Pp.T.Foreground Pp.T.Green] "match & gamme")
    (Pp.str [Pp.T.Foreground Pp.T.Cyan] "match & HORS gamme")
    (Pp.str [Pp.T.Foreground Pp.T.Default] "NO match & gamme")
    (Pp.str [Pp.T.Foreground Pp.T.Yellow] "NO match & HORS gamme")

let pr_matching fmt m =
  let n,color =
    match m with
    | Present n -> n,Pp.T.Green
    | Absent n -> n,Pp.T.Default
    | PresentAlien n -> n,Pp.T.Cyan
    | AbsentAlien n -> n,Pp.T.Yellow in
  Format.fprintf fmt "%s" (Pp.str [Pp.T.Foreground color] (Note.to_string n))

let pr_matchings fmt lm =
  Format.fprintf fmt "@[<h>%a@]" (Pp.print_list Pp.brk pr_matching) lm 

module type S =
sig
  module G: Gamme.S
  val interp: t -> Note.t list
  val matching: (module Gamme.S) -> Note.t list -> Accord.t -> matching_note list
end

module Make(G:Gamme.S):S = struct 
  module G=G
  let interv = G.interv
  let interp_raw_indic r =
    match r with
    | Absolu n -> n
    | Relative (n,i) -> interv n i

  let interp_indic i =
    match i with
    | Diese r -> Note.decale_chrom (interp_raw_indic r) 1
    | Bemol r -> Note.decale_chrom (interp_raw_indic r) (-1)
    | Exact r -> (interp_raw_indic r)

  let interp ch =
    ch.basse::List.map (fun indic -> interp_indic indic) ch.indics

  let decide_matching is_given is_in_gamme (n:Note.t): matching_note =  
    match is_given n, is_in_gamme n with
    | true , true -> Present n
    | true , false -> PresentAlien n
    | false , true -> Absent n
    | false , false -> AbsentAlien n

  let matching (module G:Gamme.S) (lnotes:Note.t list) (ch:Accord.t): matching_note list =
    let module G:Gamme.S = G in
    let is_given n = List.mem n lnotes  in
    let is_in_g n = G.exists (fun x -> x = n) in
    let test = decide_matching is_given is_in_g in
    List.map test (Accord.notes_of_chord ch)

end
