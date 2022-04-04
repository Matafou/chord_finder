
let intersect l chord = List.filter (fun x -> List.mem x l) (Accord.notes_of_chord chord)

let count_intersect l (chord:Accord.t) = List.length (intersect l chord)

let intersect_all lchords lnotes  =
  let unsorted = List.map (fun chord -> (count_intersect lnotes chord,chord)) lchords in
  List.sort (fun (x,_) (y,_) -> Stdlib.compare y x) unsorted


let pr_note_highlight fmt n =
  Format.fprintf fmt "%s"
    (Pp.str [Pp.T.Foreground Pp.T.Green] (Note.to_string n))

let pr_note_highlight_if test fmt (n:Note.t) =
  if test n then pr_note_highlight fmt n else Note.pr fmt n

let pr_note_hl_if_mem l fmt note =
  Format.fprintf fmt "%a" (pr_note_highlight_if (fun x -> List.mem x l)) note

let pr_lnotes_hl_if_mem (l:Note.t list) fmt chord =
  Format.fprintf fmt "%a" (Pp.print_list Pp.comma (pr_note_hl_if_mem l))
    (Accord.notes_of_chord chord)

let pr_accord_hl_mem lnotes fmt (n,ac) =
  Format.fprintf fmt "@[<h>%d: %a  %a@]" n (Accord.pr_fixed_width 5) ac
    (pr_lnotes_hl_if_mem lnotes) ac


let pr_accords_hl_mem lnotes fmt (l:(int*Accord.t) list) =
  Format.fprintf fmt "@[<v>%a@]@.@?" (Pp.print_list Pp.some_space (pr_accord_hl_mem lnotes)) l

let rec ask n =
  let s = read_line() in
  if s.[0] <> 'q' then
    (* let s = String.sub s 0 (String.length s - 1) in (* remove trailing \n *) *)
    let lnotes = Note.parse_list s in
    let all_chords = Accord.chain_chord_makers Accord.all_chord_makers in
    let chords : (int*Accord.t) list = intersect_all all_chords lnotes in
    let chords_filtered = List.filter (fun (i,_) -> i>=n) chords in
    Format.printf "%a" (pr_accords_hl_mem lnotes) chords_filtered;
    Format.printf "**********************@.@?";
    ask n
  else ()


let rec ask_acc_bass g n =
  try
    let s = read_line() in
    if s = "" then ask_acc_bass g n
    else if s.[0] <> 'q' then
      let () =
        try 
          let (module G: Gamme.Gamme) = g in
          let ch:Chiffrage.t = Chiffrage.parse_chiffrage s in
          let l = Chiffrage.interp G.interv ch in
          let all_chords = Accord.chain_chord_makers Accord.all_chord_makers in
          let chords : (int*Accord.t) list = intersect_all all_chords l in
          let chords_filtered = List.filter (fun (i,_) -> i>=n) chords in
          Format.printf "@[<h>%a  @;%a@?" (Pp.print_list Pp.brk Note.pr) l
            (pr_accords_hl_mem l) chords_filtered
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




let rec ask_gamme () =
  let () = Format.printf "Quelle gamme (\"dominante adjectif\") @?" in
  let s = read_line() in
  if s.[0] <> 'q' then
    try 
      let gammename = Parse.parseStringGamme s in
      match gammename with
      | Majeur dom -> Gamme.Majeur dom
      | Mineur dom -> Gamme.Mineur dom
    with _ -> ask_gamme ()
  else exit 0;;


(* let parse_note s = *)
  (* let lb = Lexing.from_string s in *)
  (* Parse.note Lex.next_token lb *)


let () =
  let g:Gamme.gammeStandard = ask_gamme() in
  let gamme = Gamme.gen_gamme g in
  let (module G) = gamme in
  Format.printf "Gamme de %s: @[%a@]@?" (G.g.nom G.g.dominante) G.pr ();
  ask_acc_bass gamme 2;;
