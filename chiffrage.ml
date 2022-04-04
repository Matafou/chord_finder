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

module type S =
sig
  val interp: t -> Note.t list
end


module Make(G:Gamme.Gamme):S = struct 
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
end
