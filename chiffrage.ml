(* Les intervales en notation "chiffrage d'accord".
   https://fr.wikipedia.org/wiki/Chiffrage_des_accords *)

type level =
  | Absolu of Note.t
  | Relative of (Note.t*int)

type indic =
  | Diese of level
  | Bemol of level
  | Exact of level

(* a note of an indication is mandatory or not *)
type mandatoriness = Mandatory | Optional

(* Analysing the adequacy of a chord given

   - the gamme we are in
   - the mandatory notes (numeral from continuo basso))
   - the optional notes (other instruments)

   Principles
   - mandatory notes are mandatory, they must all be in the chord
   - other notes present in a chord are given a score wrt 
     + being present in optional note
     + being in the gamme
        
*)

(* a note of a chord is in the gamme or not *)
type alienness = InGamme | HorsGamme

(* a note (mandatory or not) is present in the chord.  *)
type presence = Present | Absent

type incomplete_score = { note: Note.t ;
                          present:presence; }

type chord_note =
  { note: Note.t ;
    present:presence;
    alienness : alienness ;
    mandatoriness: mandatoriness }

type 'a t = { basse: Note.t ; indics : 'a list }

type raw_indic = indic*mandatoriness
type indic_in_gamme = indic*mandatoriness*alienness* presence

type raw_t = indic t (raw_indic)

(* TODO: have a map? *)
type mesure = int*t
type portee = mesure list



let count_present (lm:matching_note list):int=
  List.length (List.filter (function | Present _ | _ -> false) lm)

let count_absent (lm:matching_note list):int=
  List.length (List.filter (function | Absent _ | _ -> false) lm)


(* type indication = *)
(*   | Mandatory of matching_note *)
(*   | Optional of matching_note *)
(* let count_mandatory (lm:indication list):int= *)
(*   List.length (List.filter (function | Mandatory (Present _) | Mandatory (PresentAlien _) -> true | _ -> false) lm) *)

(* let count_indic_present (lm:indication list):int= *)
(*   List.length (List.filter *)
(*                  (function *)
(*                   | Mandatory (Present _) *)
(*                     | Mandatory (PresentAlien _) *)
(*                     | Optional (Present _) *)
(*                     | Optional (PresentAlien _) -> true *)
(*                   | _ -> false) lm) *)

let pr_raw fmt r =
  match r with
  | Absolu n -> Format.fprintf fmt "%a" Note.pr n
  | Relative (_,i) -> Format.fprintf fmt "%d" i

let pr_indic fmt ind =
    match ind with
    | Diese r -> Format.fprintf fmt "%a#" pr_raw r
    | Bemol r -> Format.fprintf fmt "%ab" pr_raw r
    | Exact r -> Format.fprintf fmt "%a" pr_raw r

let pr fmt (ch:t) =
  Format.fprintf fmt "%a %a" Note.pr ch.basse (Pp.print_list Pp.brk pr_indic) ch.indics

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

let pr_l_matchings fmt llm =
  Format.fprintf fmt "@[<v>%a@]" (Pp.print_list Pp.brk pr_matchings) llm 

let pr_chord_matchings fmt (ch,lmtch) =
  Format.fprintf fmt "@[<h>%d: %a@ %a@]"
    (count_present lmtch) (Accord.pr_fixed_width 7) ch
    pr_matchings lmtch

let pr_l_chord_matchings fmt l =
  Format.fprintf fmt "@[<v>%a@]" (Pp.print_list Pp.brk pr_chord_matchings) l

let is_relative x =
  match x with
  | (Diese (Relative (_,_))) | (Bemol (Relative (_,_))) | (Exact (Relative (_,_)))
    -> true
  | _ -> false

let contain_absentAlien l =
  List.exists (function | AbsentAlien _ -> true | _ -> false) l

let interp_absolute i =
  let interp_raw r =
    match r with
    | Absolu n -> n
    | Relative (_,_) -> failwith "Illegal argument" in
  match i with
  | Diese r -> Note.decale_chrom (interp_raw r) 1
  | Bemol r -> Note.decale_chrom (interp_raw r) (-1)
  | Exact r -> (interp_raw r)

module type S =
sig
  module G: Gamme.S
  val interp: t -> Note.t list
  val matching: Note.t list -> Accord.t -> matching_note list
  val intersect_all: Accord.t list ->  Note.t list -> (int * Accord.t) list
  val compare_matching: t -> Accord.t -> Accord.t -> int
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

  let interp chfr =
    chfr.basse::List.map (fun indic -> interp_indic indic) chfr.indics
    @ chfr.others

  let interp_mandatory chfr =
    chfr.basse::List.map (fun indic -> interp_indic indic) chfr.indics
    @ chfr.others

  let decide_matching is_given is_in_gamme (n:Note.t): matching_note =  
    match is_given n, is_in_gamme n with
    | true , true -> Present n
    | true , false -> PresentAlien n
    | false , true -> Absent n
    | false , false -> AbsentAlien n

  let matching (lnotes:Note.t list) (ch:Accord.t): matching_note list =
    let module G:Gamme.S = G in
    let is_given n = List.mem n lnotes  in
    let is_in_g n = G.exists (fun x -> x = n) in
    let test = decide_matching is_given is_in_g in
    List.map test (Accord.notes_of_chord ch)


  let intersect l c = List.filter (fun x -> List.mem x l) (Accord.notes_of_chord c)
  let count_intersect l (chord:Accord.t) = List.length (intersect l chord)

  (* compare first on mandatory notes only, then with eveything *)
  let compare_matching (chfr:t) (ch1:Accord.t) (ch2:Accord.t) =
    let lmandat = interp_mandatory chfr in
    let lall = interp chfr in
    let cmp = Stdlib.compare (count_intersect lmandat ch1) (count_intersect lmandat ch2) in
    if cmp<>0 then cmp
    else
      let cmp2 = Stdlib.compare (count_absent (matching lmandat ch2) (count_absent lmandat ch1)  in
      if cmp2 <> 0 then cmp2
      else Stdlib.compare (count_intersect lall ch1) (count_intersect lall ch2)

  let intersect_all lchords lnotes  =
    let unsorted = List.map (fun chord -> (count_intersect lnotes chord,chord)) lchords in
    List.sort (fun (x,_) (y,_) -> Stdlib.compare y x) unsorted


  (* let matching_opt (module G:Gamme.S) (lnotes:Note.t list) (lnotes_opt:Note.t list) (ch:Accord.t): matching_note list = *)
  (*   let module G:Gamme.S = G in *)
  (*   (\* let is_given n = List.mem n (lnotes@lnotes_opt)  in *\) *)
  (*   (\* let is_opt n = List.mem n lnotes_opt  in *\) *)
  (*   let is_mandat n = List.mem n lnotes  in *)
  (*   let is_in_g n = G.exists (fun x -> x = n) in *)
  (*   let test = decide_matching is_mandat is_in_g in *)
  (*   let all = List.map test (Accord.notes_of_chord ch) in *)
  (*   List.sort (fun x y -> count_present x - count_present y) all *)
    



  (* let best_matchings (module G:Gamme.S)(chfr:t) (ch:Accord.t): matching_note list =  *)
  (*   let mandatory = interp chfr in *)
  (*   let optional = chfr.others in *)
  (*   matching_opt (module G) mandatory optional ch  *)



end
