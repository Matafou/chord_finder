%{
    (* open PhaseGraph *)
    (* module F = Formula *)
%}

(** Reserved Symbols *)
%token EOF

(** Formula tokens *)
%token DIESE BEMOL MAJEUR MINEUR DO RE MI FA SOL LA SI PT VIRG PV
%token <string> ID
%token <int> INT

(** unrecognized token *)
%token UNKNOWN

/* %start <Formula.t> formula_main */
%start <Note.t> note_eof
/* %start <Accord.t> accord */
%start <Note.t list> note_list
%start <Gamme.gammeStandard> gamme_name_eof
%start <Chiffrage.t> chiffrage_eof
%start <(int*Chiffrage.t list) list> portee_eof
%%
(* Generic lists with separator and an optional final separator.
   See http://gallium.inria.fr/blog/lr-lists/ for explanations. *)
flexible_list(delim, X):
| (* nothing *) { [] }
| x = X { [x] }
| x = X delim xs = flexible_list(delim, X) { x :: xs }
;

  (* Same as flexible_list but with the optional separator is at start
     (e.g. match foo with | ... | ... end) *)
flexible_list_on_start(delim, X):
| (* nothing *) { [] }
| x = X { [x] }
| xs = flexible_list_on_start(delim, X) delim x = X { x :: xs }
;


(* ********************* FORMULA ******************** *)
note_list:
/* | l=list(note) EOF { l } */
| n=note EOF { [n] }
| n=note l=note_list { n::l }
;

(* Formulas that can appear in a list without being inside parenthesis *)
note_eof:
| n = note EOF { n }
;

gamme_name_eof:
| g=gamme_name EOF { g }

(* Formulas that can appear in a list without being inside parenthesis *)
note:
| n = note_simple { n }
| n = note_simple DIESE { Note.diesify n }
| n = note_simple BEMOL { Note.bemolify n }
/* | n = note_simple BECARE { becarify n } */
;

chiffrage_eof:
| c=chiffrage_complet EOF { c }
;
chiffrage_complet:
| n = note
  l=list(chiffrage) {
    let updated_indics =
      (* l has dummy notes in relative chiffrages replace them by n *)
      List.map Chiffrage.(
        function
        | (Diese (Relative (_,i))) -> (Diese (Relative (n,i)))
        | (Bemol (Relative (_,i))) -> (Bemol (Relative (n,i)))
        | (Exact (Relative (_,i))) -> (Exact (Relative (n,i)))
        | c -> c)
        l in
    Chiffrage.{basse=n; indics = updated_indics} }
;

chiffrage:
| n = note_simple { Chiffrage.(Exact (Absolu n)) }
| n = note_simple DIESE { Chiffrage.(Diese (Absolu n)) }
| n = note_simple BEMOL { Chiffrage.(Bemol (Absolu n)) }
| i = INT DIESE {let dummy_note = Note.C in Chiffrage.(Diese (Relative (dummy_note,i)))}
| i = INT BEMOL {let dummy_note = Note.C in Chiffrage.(Bemol (Relative (dummy_note,i)))}
| i = INT {let dummy_note = Note.C in Chiffrage.(Exact (Relative (dummy_note,i)))}
;

gamme_name:
| n=note MAJEUR { Gamme.Majeur n }
| n=note MINEUR { Gamme.Mineur n }
| n=note g=ID { Gamme.parseName n g }
;

note_simple:
  | DO {Note.C}
  | RE {Note.D}
  | MI {Note.E}
  | FA {Note.F}
  | SOL{Note.G}
  | LA {Note.A}
  | SI {Note.B}
;

portee_eof:
p=portee EOF {p}
;

portee:
  | { [] }
  | m = mesure p = portee {m::p}
;
mesure:
  | n = mes_num c=chiffrage_complet PV { (fst n,[c]) }
  | n = mes_num c1=chiffrage_complet VIRG c2=chiffrage_complet PV { (fst n, [c1;c2]) }
;

mes_num:
  | i=INT PT            {(i,0)}
  | i=INT VIRG j=INT PT {(i,j)}
;
