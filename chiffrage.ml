open Note
(* Les intervales en notation "chiffrage d'accord".
   https://fr.wikipedia.org/wiki/Chiffrage_des_accords *)

(* type alteration = Diese | Bemol| Exact;; *)

type raw =
  | Absolu of Note.t
  | Relative of (Note.t*int)

type indic =
  | Diese of raw
  | Bemol of raw
  | Exact of raw

type t = { basse: Note.t ; indics : indic list }

let interval_of_string (n:Note.t) (s:string):indic =
  let remove_fst x = String.sub x 1 (String.length s - 1) in
  let remove_last x = String.sub x 0 (String.length s - 1) in
  try
    match s.[0] with
    | '#' -> Diese (Relative (n,int_of_string (remove_fst s)))
    | 'b' -> let rel = int_of_string (remove_fst s) in
             Bemol (Relative (n,rel))
    | _ ->
       (match s.[String.length s - 1] with
        | '#' -> Diese (Relative (n,int_of_string (remove_last s)))
        | 'b' -> Bemol(Relative (n,int_of_string (remove_last s)))
        | _ -> Exact (Relative (n,int_of_string s)))
  with _ -> raise (Unknown_Notation s)

let interval_or_note_of_string basse s: indic =
  let module G = Gamme.DoMajeur in
  try interval_of_string basse s
  with _ -> Exact (Absolu (Note.of_string s))

let parse_chiffrage s: t =
  let ls = String.split_on_char ' ' s in
  let ls = List.filter (fun x -> not (String.equal x "")) ls in
  let basse = Note.of_string (List.hd ls) in
  { basse = basse; indics = List.map (interval_or_note_of_string basse) (List.tl ls) }



let interp_raw_indic interv r =
  match r with
  | Absolu n -> n
  | Relative (n,i) -> interv n i

let interp_indic interv i =
  match i with
  | Diese r -> Note.decale_chrom (interp_raw_indic interv r) 1
  | Bemol r -> Note.decale_chrom (interp_raw_indic interv r) (-1)
  | Exact r -> (interp_raw_indic interv r)

let interp interv ch =
  ch.basse::List.map (fun indic -> interp_indic interv indic) ch.indics
