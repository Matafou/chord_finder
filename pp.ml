
(* LIST OPERATIONS *)

let rec remove_before note lgamme =
  match lgamme with
  | [] -> raise Not_found
  | (n,_) :: _ when n = note -> lgamme
  | _ :: l' -> remove_before note l'

let rec prefix n l =
  if n <= 0 then []
  else match l with
       | [] -> assert false
       | e::l' -> e::prefix (n-1) l'

let rec last l =
  match l with
  | [] -> assert false
  | e::[] -> e
  | _::l' -> last l'


(* PRINTING *)

(*T.Black; T.Red; T.Green; T.Yellow; T.Blue; T.Magenta; T.Cyan; T.White; T.Default *)
module T = ANSITerminal

let color_to_string = function
  | T.Black -> "black"
  | T.Red -> "red"
  | T.Green -> "green"
  | T.Yellow -> "yellow"
  | T.Blue -> "blue"
  | T.Magenta -> "magent"
  | T.Cyan -> "cyan"
  | T.White -> "white"
  | T.Default -> "def"

let str style s: string = T.sprintf style "%s" s;;


let rec print_list sep print fmt =
  function
  | [] -> ()
  | [x] -> Format.fprintf fmt "%a" print x
  | x :: r -> Format.fprintf fmt "%a%a%a" print x sep () (print_list sep print) r


let comma fmt () = Format.fprintf fmt ",@ "
let dot fmt () = Format.fprintf fmt "."
let semi fmt () = Format.fprintf fmt ";@ "
let newline fmt () = Format.fprintf fmt "@\n"
let brk fmt () = Format.fprintf fmt "@;"
let cut fmt () = Format.fprintf fmt "@,"
let nothing _fmt () = ()
let some_space fmt () = Format.fprintf fmt "    @ "
