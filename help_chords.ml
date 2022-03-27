open Accord

let intersect l chord = List.filter (fun x -> List.mem x l) (notes_of_chord chord)

let count_intersect l (chord:accord) = List.length (intersect l chord)

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
    (notes_of_chord chord)

let pr_accord_hl_mem lnotes fmt (n,ac) =
  Format.fprintf fmt "@[<h>%d: %a  %a@]" n (Accord.pr_fixed_width 5) ac
    (pr_lnotes_hl_if_mem lnotes) ac


let pr_accords_hl_mem lnotes fmt (l:(int*accord) list) =
  Format.fprintf fmt "@[<v>%a@]@.@?" (Pp.print_list Pp.some_space (pr_accord_hl_mem lnotes)) l

let rec ask n =
  let s = read_line() in
  if s.[0] <> 'q' then
    (* let s = String.sub s 0 (String.length s - 1) in (* remove trailing \n *) *)
    let lnotes = Note.parse_list s in
    let all_chords = chain_chord_makers all_chord_makers in
    let chords : (int*accord) list = intersect_all all_chords lnotes in
    let chords_filtered = List.filter (fun (i,_) -> i>=n) chords in
    Format.printf "%a" (pr_accords_hl_mem lnotes) chords_filtered;
    Format.printf "**********************@.@?";
    ask n
  else ()


let rec ask_acc_bass n =
  try
    let s = read_line() in
    if s = "" then ask_acc_bass n
    else if s.[0] <> 'q' then
      let () =
        try 
          let l = Chiffrage.parse_list s in
          let all_chords = chain_chord_makers all_chord_makers in
          let chords : (int*accord) list = intersect_all all_chords l in
          let chords_filtered = List.filter (fun (i,_) -> i>=n) chords in
          Format.printf "@[<h>%a  @;%a@?" (Pp.print_list Pp.brk Note.pr) l
            (pr_accords_hl_mem l) chords_filtered
        with Note.Unknown_Notation s -> print_string ("Unknown notation \""^s^"\"\n") in
      Format.printf "**********************@.@?";
      ask_acc_bass n
    else ()
  with End_of_file -> ()
;;

let () = ask_acc_bass 2;;
