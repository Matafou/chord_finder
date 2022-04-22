
(* intersection of a given a list of notes l and a chord   *)

(*
let pr_candidates modC fmt ((chfr,l_matchings):Chiffrage.t*bool list) =
  let module C:Chiffrage.S = modC in
  let l_notes_chord = chfr.all
  let pseudo_chord = Accord.{ name="dummychord" ; tonique = List.hd l_notes_chord ;
                              autres = List.tl l_notes_chord} in
  let pseudo_score = C.compute_adequacy   
  Format.fprintf fmt "@[<h>%a@]@;   @[<v>%a@]" (Pp.print_list Pp.brk Note.pr) l_notes_chord
               Chiffrage.pr_l_chord_score l_matchings
*)

let pr_candidates fmt ((pseudo_score,l_matchings)
                       :(Accord.t*Chiffrage.score)*((Accord.t*Chiffrage.score) list)) =
  Format.fprintf fmt "@[<h>%a@]@;   @[<v>%a@]" 
    Chiffrage.pr_matchings pseudo_score
    (* (Pp.print_list Pp.brk Note.pr) l_notes_chord *)
               Chiffrage.pr_l_chord_score l_matchings

let rec ask_gamme () =
  let () = Format.printf "Quelle gamme (\"ex: C#M, C # m, Sib pentaM, Ablues,...\") @?" in
  let s = read_line() in
  if s.[0] = 'q' then exit 0 else
    try Parse.parseStringGamme s
    with
      Not_found | Lex.Lexing_error _ | Parse_all.Error ->
                   let () = Format.printf "Don't understand \"%s\"@.@?" s in
                   ask_gamme ()


let compute_candidates g (chfr:Chiffrage.t) remove_absent_alien cut all_chords =
  let (module C: Chiffrage.S) = g in
  List.sort (C.compare_chord chfr) all_chords
  |> List.rev 
  |> List.map (fun ch -> ch,C.compute_adequacy ch chfr)
  |> (if false && remove_absent_alien
      then (List.filter (fun (_,m) -> not (Chiffrage.contain_absentAlien m)))
      else (fun x -> x))
  |> (fun l -> try Pp.prefix cut l with _ -> l)


let rec ask_acc_bass g n =
  let all_chords = Accord.chain_chord_makers Accord.all_chord_makers in
  try
    let s = read_line() in
    if s = "" then ask_acc_bass g n
    else if s.[0] <> 'q' then
      let () =
        try 
          let (module C: Chiffrage.S) = g in
          let ast_chfr:Ast.chiffrage = Parse.parseStringChiffrage s in
          let chfr:Chiffrage.t = C.interp_ast ast_chfr in
          let pseudo_chord = Accord.{name=""; tonique = ast_chfr.base ;
                                     autres = List.tl chfr.all_notes} in
          let pseudo_score = C.compute_adequacy pseudo_chord chfr in 
          let matchings:(Accord.t*Chiffrage.score) list =
            compute_candidates g chfr true 5 all_chords in
          let () = Format.printf "%a" pr_candidates ((pseudo_chord,pseudo_score),matchings) in
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


let iter_lookup_prev num_sugg filter modC (portee:(int * Ast.chiffrage) list) =
  let all_chords = Accord.chain_chord_makers Accord.all_chord_makers in
  let prev = ref (0) in
  let (module C:Chiffrage.S) = modC in
  let filtered_portee = List.filter filter portee in
  List.iter 
    (fun (n,ast_chfr) ->
      if n <> !prev then Format.printf "mesure %d: @[<v>" n
      else Format.printf "   %d(bis): @[<v>" n;
      let chfr = C.interp_ast ast_chfr in
      let pseudo_chord = Accord.{name=""; tonique = ast_chfr.base ;
                                 autres = List.tl chfr.all_notes} in
      let pseudo_score = C.compute_adequacy pseudo_chord chfr in 
      let cand = compute_candidates modC chfr true num_sugg all_chords in
      Format.printf "(@[<h>%a@]) %a" Ast.pr_ast_chiffrage ast_chfr
        pr_candidates ((pseudo_chord,pseudo_score),cand);
      Format.printf "@]@,";
      (Format.printf "@.";
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
  iter_lookup_prev 2 filter_fun (module Chifr) portee;

  close_in ic;;

main()

