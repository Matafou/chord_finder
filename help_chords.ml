
(* intersection of a given a list of notes l and a chord   *)

let intersect l c = List.filter (fun x -> List.mem x l) (Accord.notes_of_chord c)

let count_intersect l (chord:Accord.t) = List.length (intersect l chord)

let intersect_all lchords lnotes  =
  let unsorted = List.map (fun chord -> (count_intersect lnotes chord,chord)) lchords in
  List.sort (fun (x,_) (y,_) -> Stdlib.compare y x) unsorted

let pr_candidates fmt (l_notes_chord,l_matchings) =
  Format.fprintf fmt "@[<h>%a@]@;   @[<v>%a@]" (Pp.print_list Pp.brk Note.pr) l_notes_chord
               Chiffrage.pr_l_chord_matchings l_matchings

let rec ask_gamme () =
  let () = Format.printf "Quelle gamme (\"ex: C#M, C # m, Sib pentaM, Ablues,...\") @?" in
  let s = read_line() in
  if s.[0] = 'q' then exit 0 else
    try Parse.parseStringGamme s
    with
      Not_found | Lex.Lexing_error _ | Parse_all.Error ->
                   let () = Format.printf "Don't understand \"%s\"@.@?" s in
                   ask_gamme ()


let compute_candidates g chfr limit remove_absent_alien cut all_chords =
  let (module C: Chiffrage.S) = g in
  let l = C.interp chfr in
  l
  |> intersect_all all_chords
  |> List.filter (fun (i,_) -> i>=limit)
  |> List.map snd
  |> List.map (fun ch -> ch,C.matching (module C.G) l ch)
  |> (fun l -> if remove_absent_alien then List.filter (fun (_,m) -> not (Chiffrage.contain_absentAlien m)) l else l)
  |> (fun l -> try Pp.prefix cut l with _ -> l)


let rec ask_acc_bass g n =
  try
    let s = read_line() in
    if s = "" then ask_acc_bass g n
    else if s.[0] <> 'q' then
      let () =
        try 
          let (module C: Chiffrage.S) = g in
          let chfr:Chiffrage.t = Parse.parseStringChiffrage s in
          let all_chords = Accord.chain_chord_makers Accord.all_chord_makers in
          let matchings:(Accord.t*Chiffrage.matching_note list) list =
            compute_candidates g chfr n true 6 all_chords in
          let l = C.interp chfr in
          let () = Format.printf "%a" pr_candidates (l,matchings) in
          ()
        with Note.Unknown_Notation s -> print_string ("Unknown notation \""^s^"\"\n") in
      Format.printf "@.**********************@.@?";
      ask_acc_bass g n
    else ()
  with 
    End_of_file -> ()
  | e ->
     (Format.printf "*** %s ***@.@?" (Printexc.to_string_default e);
      ask_acc_bass g n)
;;

let main2 () =
  let g:Gamme.gammeStandard = ask_gamme() in
  let gamme = Gamme.gen_gamme g in
  let (module G) = gamme in
  Format.printf "Gamme de %s: @[%a@]@?" (G.nom G.dominante) G.pr ();
  let module Chifr:Chiffrage.S = Chiffrage.Make(G) in
  let () = Format.printf "         %a@.@?" Chiffrage.pr_legend() in
  ask_acc_bass (module Chifr) 2;;


let iter_lookup_prev filter modC portee =
  let all_chords = Accord.chain_chord_makers Accord.all_chord_makers in
  let prev = ref (0) in
  let (module C:Chiffrage.S) = modC in
  let filtered_portee = List.filter filter portee in
  List.iter 
    (fun (n,l_intra_mesure) ->
      if n <> !prev then Format.printf "mesure %d: @[<v>" n
      else Format.printf "   %d(bis): @[<v>" n;
      List.iter (fun chfr ->
          let cand = compute_candidates (module C) chfr 2 true 7 all_chords in
          let l = C.interp chfr in
          Format.printf "(@[<h>%a@]) %a"
            Chiffrage.pr chfr
            pr_candidates (l,cand)) l_intra_mesure;
      Format.printf "@]@,";
      (if n <> !prev then Format.printf "@.";
      prev:=n)
    )
    filtered_portee;
         Format.printf "@]"
;;  

let usage = "usage: help_chord.exe -g <gamme> -f <file>";;

let file = ref ""
let gamme = ref ""
let filter_mesures: int list option ref = ref None
let set_file s = file := s
let set_gamme s = gamme := s
let set_filter s =
  let ls = String.split_on_char ',' s in
  let l = List.map int_of_string ls in
  filter_mesures := Some l

let params = [
    ("-f", Arg.String set_file, "set the file");
    ("-g", Arg.String set_gamme, "set la gamme");
    ("--only", Arg.String set_filter, "affiche seulement les mesures");
  ]


let main () =
  let _ = Arg.parse params (fun _ -> ()) usage in
  let g:Gamme.gammeStandard =
    if !gamme <> "" then
      (try Parse.parseStringGamme !gamme
       with Not_found | Lex.Lexing_error _ | Parse_all.Error ->
                         let () = Format.printf "Don't understand \"%s\"@.@?" !gamme in
                         exit (1))
    else ask_gamme () in
  let gamme = Gamme.gen_gamme g in
  let (module G) = gamme in
  Format.printf "Gamme de %s: @[%a@]@?" (G.nom G.dominante) G.pr ();
  let module Chifr:Chiffrage.S = Chiffrage.Make(G) in
  let filename = !file in
  let ic = open_in filename in
  let lb = Lexing.from_channel ic in
  let portee = Parse.parse_portee_with_error lb in
  let filter_fun (n,_) =
    match !filter_mesures with
    | None -> true
    | Some l -> List.mem n l in
  iter_lookup_prev filter_fun (module Chifr) portee;

  close_in ic;;

main()

