{
open Parse_all
module L = Lexing 
module B = Buffer
exception Lexing_error of string
let get = L.lexeme

let ignore_lex lb = ignore (Lexing.lexeme lb) ;;
exception Error of (Lexing.lexbuf*string)

}

let blank = [' ' '\t']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let digit = ['0'-'9']
let unsigned_int = digit+
let integer = ['-''+']?digit+
let identchar = [ 'A'-'Z' 'a'-'z'  '0'-'9' '_' '.' ]
let identstart = ['A'-'Z' 'a'-'z' '_' '@']
let decimal_literal = '-'?['0'-'9']+
let note_do = "Do"|"do"|"C"
let note_re = "Ré"|"ré"|"Re"|"re"|"D"
let note_mi = "Mi"|"mi"|"E"
let note_fa = "Fa"|"fa"|"F"
let note_sol = "Sol"|"sol"|"G"
let note_la = "La"|"la"|"A"
let note_si = "Si"|"si"|"B"
let nomGammes = "Blues" | "blues" | "Pentam" | "pentam" | "PentaM" | "pentaM"
                | "majeur"| "majeure" | "Majeur"| "Majeure"
                | "mineureharm" | "MineureHarm" | "mineurharm" | "MineurHarm"
                | "mineuremel" | "MineureMel" | "mineurmel" | "MineurMel"
                | "mineurenat" | "MineureNat" | "mineurnat" | "MineurNat"
rule next_token = parse
| "(*"               { comment lexbuf ; next_token lexbuf }
| "*)"               { raise (Lexing_error "end of comment without matching start") }
| "#"               { DIESE }
| "b"               { BEMOL }
| "M"               { MAJEUR }
| "mh"               { MINEURHARM }
| "mm"               { MINEURMEL }
| "mn"               { MINEURNAT }
| ";"               { PV }
| ","               { VIRG }
| "."               { PT }
| "gamme"               { GAMME }
| note_do            { DO }
| note_re            {RE}
| note_mi           { MI }
| note_fa           { FA }
| note_sol           { SOL }
| note_la           { LA }
| note_si           { SI }
| unsigned_int      {INT (int_of_string (Lexing.lexeme lexbuf))}
| nomGammes { let g = Lexing.lexeme lexbuf in ID g }
| eof                { EOF }
| '\n'               { L.new_line lexbuf; next_token lexbuf }
| blank +            { next_token lexbuf }
(* | identstart identchar* { ID (get lexbuf)} *)
(* | integer            { NUM (int_of_string (get lexbuf))} *)
| "(*"               { comment lexbuf ; next_token lexbuf }
| "*)"               { raise (Lexing_error "end of comment without matching start") }
| _ { UNKNOWN }

and comment = parse 
| "(*"    { ignore_lex lexbuf ; comment lexbuf ; comment lexbuf }
| "*)"    { ignore_lex lexbuf }
| '\n'    { L.new_line lexbuf;comment lexbuf }
| eof     { raise (Lexing_error "eof in a comment") }
| _       { ignore_lex lexbuf ; comment lexbuf }


