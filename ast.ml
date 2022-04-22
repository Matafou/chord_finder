
type level =
  | Absolu of Note.t
  | Relative of int

type accident =
  | Diese
  | Bemol
  | Exact

type indic = { level:level ; accident:accident }

type chiffrage = { base: Note.t ; others : indic list }

let pr_raw fmt r =
  match r with
  | Absolu n -> Format.fprintf fmt "%a" Note.pr n
  | Relative i -> Format.fprintf fmt "%d" i

let pr_acc fmt ind =
    match ind with
    | Diese -> Format.fprintf fmt "#" 
    | Bemol -> Format.fprintf fmt "b"
    | Exact -> Format.fprintf fmt ""

let pr_ast_indic fmt a =
  Format.fprintf fmt "%a%a" pr_raw a.level pr_acc a.accident

let pr_ast_chiffrage fmt chfr =
  Format.fprintf fmt "%a@ %a" Note.pr chfr.base (Pp.print_list Pp.brk pr_ast_indic) chfr.others
