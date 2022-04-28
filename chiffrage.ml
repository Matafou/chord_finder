(* Les intervales en notation "chiffrage d'accord".
   https://fr.wikipedia.org/wiki/Chiffrage_des_accords *)

module MapNotes = Map.Make(Note)

(* a note of an indication is mandatory or not *)
type mandatoriness = Mandatory | Optional

type indic = { note: Note.t ;
               mandatoriness:mandatoriness ; in_gamme:bool }

(* all_notes is there only to keep the order in witch the notes were given. *)
type t = { basse: Note.t ;  all_notes: Note.t list; indics : indic MapNotes.t }


let union_mandat m1 m2 =
  match m1,m2 with
  | Mandatory,_ | _,Mandatory -> Mandatory
  | _,_ -> Optional

type score = {
    present_mandatory: Note.t list; (* mandatory notes present in the chord   *)
    absent_mandatory: Note.t list; (* mandatory notes missing in the chord   *)
    present_optional: Note.t list; (* optional notes present in the chord *)
    absent_optional: Note.t list; (* optional notes missing in the chord *)
    present_other_ok: Note.t list; (* notes in the chord that are
                                      not suggested but ok with the
                                      gamme *)
    present_alien: Note.t list; (* notes in the chord that are not
                                   suggested AND not in the gamme *)
  }


let count_present (s:score) = List.length s.present_mandatory
                              + List.length s.present_optional

let contain_absentAlien (s:score) = s.present_alien <> []

let col_pres_mandat = Pp.T.Green
let col_pres_opt = Pp.T.Cyan
let col_pres_otherok = Pp.T.Default
let col_pres_alien = Pp.T.Magenta

let categorize s n =
  let fg,ul =
    if List.mem n s.present_mandatory then col_pres_mandat,true
    else if List.mem n s.present_optional then col_pres_opt,false
    else if List.mem n s.present_other_ok then col_pres_otherok,false
    else if List.mem n s.present_alien then col_pres_alien,false
    else assert false in
  [Pp.T.Foreground fg]@(if ul then [Pp.T.Underlined] else [])

(* [Pp.T.Foreground color] *)
  
let compare_score
      { present_mandatory=pm1; absent_mandatory=_am1; present_optional=po1;
        absent_optional=_ao1 ; present_other_ok=pok1; present_alien=pa1 }
      { present_mandatory=pm2; absent_mandatory=_am2; present_optional=po2;
        absent_optional=_ao2 ; present_other_ok=pok2; present_alien=pa2 } =
  let cmp l1 l2 = compare (List.length l1) (List.length l2) in
  let pm = cmp pm1 pm2 in
  let pa = cmp pa2 pa1 in
  let po = cmp po1 po2 in
  let pok = cmp pok2 pok1 in
  if false then assert false
  else if pa <> 0 then pa       (* no non-suggested alien note *)
  else if pm <> 0 then pm
  else if po <> 0 then po
  else if pok <> 0 then pok
  else 0

(* Analysing the adequacy of a given chord wrt

   - the gamme we are in
   - the mandatory notes (numeral from continuo basso))
   - the optional notes (other instruments)

   Principles
   - mandatory notes are mandatory, they must all be in the chord
   - other notes present in a chord are given a score wrt 
     + being present in optional note
     + being in the gamme *)

module type S =
sig
  module G: Gamme.S
  val interp_ast_indic: Note.t -> Ast.indic -> indic
  val interp_ast: Ast.chiffrage -> t
  val compute_adequacy: Accord.t -> t -> score
  val compare_chord: t -> Accord.t -> Accord.t -> int
end


module Make(G:Gamme.S):S =
struct
  module G=G

  (* numerals are mandatory notes, others are optional *)
  let compute_mandatoriness ast: mandatoriness =
    match ast.Ast.level with
    | Absolu _ -> Optional
    | Relative _ -> Mandatory

  (* An indication in a gamme gives a precise note. *)
  let interp_ast_indic (basse:Note.t) (i:Ast.indic): indic =
    let lvl = 
      match i.level with
      | Absolu n -> n
      | Relative (i) -> G.interv basse i in
    let note = 
      match i.accident with
      | Diese -> Note.decale_chrom lvl 1
      | Bemol -> Note.decale_chrom lvl (-1)
      | Exact -> lvl in
    { note = note; mandatoriness=compute_mandatoriness i ; in_gamme = G.mem note }

  let interp_l_ast_indic (basse:Note.t) (l:Ast.indic list) =
    let start_map =
      MapNotes.add basse { note=basse; mandatoriness=Mandatory;in_gamme=G.mem basse }
        MapNotes.empty in
    List.fold_left
      (fun acc astindic -> 
        let indic = interp_ast_indic basse astindic in
        (* update the mandatoriness of this note *)
        try let old_indic = MapNotes.find indic.note acc in
            let new_indic = { old_indic with mandatoriness = 
                                               union_mandat old_indic.mandatoriness
                                                 indic.mandatoriness } in
            MapNotes.add indic.note new_indic acc
        with Not_found -> MapNotes.add indic.note indic acc)
      start_map l


  let interp_ast (c:Ast.chiffrage): t =
    let others =
      c.others
      |> List.map (interp_ast_indic c.Ast.base)
      |> List.map (fun x -> x.note) in    
    let m_indics = interp_l_ast_indic c.base c.others in
    { basse = c.Ast.base; all_notes=c.base::others; indics = m_indics }

  let list_of_mandatory l_indic =
    let filtered =
      MapNotes.filter
        (fun _ -> function{mandatoriness=Mandatory;_} -> true | _ -> false)
        l_indic in
    let l = List.of_seq (MapNotes.to_seq filtered) in
    List.map fst l
    

  let list_of_optional l_indic =
    let filtered =
      MapNotes.filter
        (fun _ -> function{mandatoriness=Optional;_} -> true | _ -> false)
        l_indic in
    let l = List.of_seq (MapNotes.to_seq filtered) in
    List.map fst l
    


  let compute_adequacy (chrd:Accord.t) (chfr:t) : score =
    let open Accord in
    let l_indic:indic MapNotes.t = chfr.indics in
    let chrd_notes:Note.t list = chrd.tonique :: chrd.autres in
    (* initial score, like if the chord was empty *)
    let init_score = { present_mandatory = [] ;
                       absent_mandatory = list_of_mandatory l_indic;
                       present_optional= [];
                       absent_optional = list_of_optional l_indic;
                       present_other_ok = []; present_alien=[] } in
    (* dispatch notes of the chords in corresponding "present" categories *)
    let score = 
      List.fold_left
        (fun acc n ->
          if List.mem n acc.absent_mandatory then
            {acc with absent_mandatory = List.filter (fun x -> x<>n) acc.absent_mandatory;
                      present_mandatory = n :: acc.present_mandatory}
          else if List.mem n acc.absent_optional then
            {acc with absent_optional = List.filter (fun x -> x<>n) acc.absent_optional;
                      present_optional = n :: acc.present_optional}
          else if G.mem n then
            {acc with present_other_ok = n :: acc.present_other_ok}
          else {acc with present_alien = n :: acc.present_alien}
        )
        init_score chrd_notes
    in
    score

  
  let rank n l =
    let rec rk l i =
      match l with
      | [] -> raise Not_found
      | e::_ when e = n -> i
      | _::l' -> rk l' (i+1) in
    rk l 0



  let compare_chord (chfr:t) c1 c2 =
    let score1 = compute_adequacy c1 chfr in
    let score2 = compute_adequacy c2 chfr in
    let cmp = compare_score score1 score2 in
    if cmp <> 0 then cmp
    else
      let all_chords = Accord.chain_chord_makers Accord.all_chord_makers in
      (* the smalle the best *)
      Stdlib.compare (rank c2 all_chords) (rank c1 all_chords)
end

let pr fmt (ch:t) =
  Format.fprintf fmt "%a" (Pp.print_list Pp.brk Note.pr) ch.all_notes

let pr_legend fmt () =
  Format.fprintf fmt "@[<v>%s@,%s@,%s@,%s@]"
    (Pp.str [Pp.T.Foreground col_pres_mandat] "mandatory & present in the chord")
    (Pp.str [Pp.T.Foreground col_pres_opt] "optional and present in the chord & HORS gamme")
    (Pp.str [Pp.T.Foreground col_pres_otherok] "absent but compatible & present in the chord")
    (Pp.str [Pp.T.Foreground col_pres_alien] "Alien & present in the chord")

let pr_matching (s:score) fmt (n:Note.t) =
  let style = categorize s n in
  Format.fprintf fmt "%s" (Pp.str style (Note.to_string n))

let pr_matchings fmt ((ch,s):Accord.t*score) =
  let l = ch.tonique :: ch.autres in 
  Format.fprintf fmt "@[<h>%a@]" (Pp.print_list Pp.brk (pr_matching s)) l

let pr_l_matchings fmt llm =
  Format.fprintf fmt "@[<v>%a@]" (Pp.print_list Pp.brk pr_matchings) llm 



let pr_chord_score fmt ((ch,lmtch):Accord.t*score) =
  Format.fprintf fmt "@[<h>%d: %a@ %a@]"
    (count_present lmtch) (Accord.pr_fixed_width 7) ch
    pr_matchings (ch,lmtch)

let pr_l_chord_score fmt l =
  Format.fprintf fmt "@[<v>%a@]" (Pp.print_list Pp.brk pr_chord_score) l

