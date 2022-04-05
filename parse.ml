(* Pour les messages d'erreur du parseur de formule *)
let print_position outx lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Format.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)


let parse_notes_with_error lexbuf: Note.t list =
  try Parse_all.note_list Lex.next_token lexbuf with
  | Lex.Error (lb,msg) as e ->
     Format.eprintf " Lexical error, %a: %s@.@?" print_position lb msg;
     raise e
  | Parse_all.Error as e ->
     Format.eprintf "syntax error, %a: @.@?" print_position lexbuf;
     raise e

let parse_gamme_name_with_error lexbuf: Gamme.gammeStandard =
  try Parse_all.gamme_name_eof Lex.next_token lexbuf with
  | Lex.Error (lb,msg) as e ->
     Format.eprintf " Lexical error, %a: %s@.@?" print_position lb msg;
     raise e
  | Parse_all.Error as e ->
    Format.eprintf "syntax error, %a: @.@?" print_position lexbuf;
    raise e 

let parse_chiffrage_with_error lexbuf: Chiffrage.t =
  try Parse_all.chiffrage_eof Lex.next_token lexbuf with
  | Lex.Error (lb,msg) as e ->
     Format.eprintf " Lexical error, %a: %s@.@?" print_position lb msg;
     raise e
  | Parse_all.Error as e ->
    Format.eprintf "syntax error, %a: @.@?" print_position lexbuf;
    raise e 

let parseNoteFile fname =
  let ic = open_in fname in
  let lb = Lexing.from_channel ic in
  let form = parse_notes_with_error lb in
  close_in ic;
  form

let parseGammeFile fname =
  let ic = open_in fname in
  let lb = Lexing.from_channel ic in
  let form = parse_gamme_name_with_error lb in
  close_in ic;
  form

let parseStringNotes s =
  let lb = Lexing.from_string s in
  let form = parse_notes_with_error lb in
  form

let parseStringGamme s =
  let lb = Lexing.from_string s in
  let form = parse_gamme_name_with_error lb in
  form

let parseStringChiffrage s =
  let lb = Lexing.from_string s in
  let form = parse_chiffrage_with_error lb in
  form
