
(* intersection of a given a list of notes l and a chord   *)

let intersect l c = List.filter (fun x -> List.mem x l) (Accord.notes_of_chord c)

let count_intersect l (chord:Accord.t) = List.length (intersect l chord)

let intersect_all lchords lnotes  =
  let unsorted = List.map (fun chord -> (count_intersect lnotes chord,chord)) lchords in
  List.sort (fun (x,_) (y,_) -> Stdlib.compare y x) unsorted

let pr_chord_matchings fmt (ch,lmtch) =
  Format.fprintf fmt "@[<h>%d: %a@ %a@]"
    (Chiffrage.count_present lmtch) (Accord.pr_fixed_width 7) ch
    Chiffrage.pr_matchings lmtch


let rec ask_gamme () =
  let () = Format.printf "Quelle gamme (\"ex: C#M, C # m, Sib pentaM, Ablues,...\") @?" in
  let s = read_line() in
  if s.[0] = 'q' then exit 0 else
    try Parse.parseStringGamme s
    with
      Not_found | Lex.Lexing_error _ | Parse_all.Error ->
                   let () = Format.printf "Don't understand \"%s\"@.@?" s in
                   ask_gamme ()


let rec ask_acc_bass g n =
  try
    let s = read_line() in
    if s = "" then ask_acc_bass g n
    else if s.[0] <> 'q' then
      let () =
        try 
          let (module G: Chiffrage.S) = g in
          let ch:Chiffrage.t = Parse.parseStringChiffrage s in
          let l = G.interp ch in
          let all_chords = Accord.chain_chord_makers Accord.all_chord_makers in
          let chords : (int*Accord.t) list = intersect_all all_chords l in
          let chords_filtered = List.filter (fun (i,_) -> i>=n) chords in
          let chords_alone = List.map snd chords_filtered in
          let (module C: Chiffrage.S) = g in
          let matchings:(Accord.t*Chiffrage.matching_note list) list =
            List.map (fun ch -> ch,C.matching (module C.G) l ch) chords_alone in
          let () = Format.printf "@[<h>%a@]@;   @[<v>%a@]" (Pp.print_list Pp.brk Note.pr) l
               (Pp.print_list Pp.cut pr_chord_matchings) matchings in
          ()
        with Note.Unknown_Notation s -> print_string ("Unknown notation \""^s^"\"\n") in
      Format.printf "**********************@.@?";
      ask_acc_bass g n
    else ()
  with 
    End_of_file -> ()
  | e ->
     (Format.printf "*** %s ***@.@?" (Printexc.to_string_default e);
      ask_acc_bass g n)
;;

let () =
  let g:Gamme.gammeStandard = ask_gamme() in
  let gamme = Gamme.gen_gamme g in
  let (module G) = gamme in
  Format.printf "Gamme de %s: @[%a@]@?" (G.nom G.dominante) G.pr ();
  let module Chifr:Chiffrage.S = Chiffrage.Make(G) in
  let () = Format.printf "         %a@.@?" Chiffrage.pr_legend() in
  ask_acc_bass (module Chifr) 2;;
