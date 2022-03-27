
(* PRINTING *)

module T = ANSITerminal
let colors =
  [T.Black; T.Red; T.Green; T.Yellow; T.Blue; T.Magenta; T.Cyan;
   T.White; T.Default]

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
