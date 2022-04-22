
type t = A | AD | B | C | CD | D | DD | E | F | FD | G | GD;;

let all_notes = [A;AD;B;C;CD;D;DD;E;F;FD;G;GD]


(* on compte à partir de zéro. Ça rend les calculs plus simple, mais à
   l'affichage on s'efforcera de respecter la décompte traditionnel.
   *)
let to_int n =
  match n with
  | C -> 0
  | CD -> 1
  | D -> 2
  | DD -> 3
  | E -> 4
  | F -> 5
  | FD -> 6
  | G -> 7
  | GD -> 8
  | A -> 9
  | AD -> 10
  | B -> 11
;;

let compare a b = Stdlib.compare (to_int a) (to_int b)

let to_string_american n =
  match n with
  | C -> "C"
  | CD -> "C#"
  | D -> "D"
  | DD -> "D#"
  | E -> "E"
  | F -> "F"
  | FD -> "F#"
  | G -> "G"
  | GD -> "G#"
  | A -> "A"
  | AD -> "A#"
  | B -> "B"
;;

let to_string n =
  match n with
  | C -> "Do"
  | CD -> "Do#"
  | D -> "Ré"
  | DD -> "Ré#"
  | E -> "Mi"
  | F -> "Fa"
  | FD -> "Fa#"
  | G -> "Sol"
  | GD -> "Sol#"
  | A -> "La"
  | AD -> "La#"
  | B -> "Si"
;;


let of_int i =
  match i mod 12 with
  | 0 -> C
  | 1 -> CD
  | 2 -> D
  | 3 -> DD
  | 4 -> E
  | 5 -> F
  | 6 -> FD
  | 7 -> G
  | 8 -> GD
  | 9 -> A
  | 10 -> AD
  | 11 -> B
  | _ -> assert false
;;

exception Unknown_Notation of string

(* todo: ne pas indentifier les bemols et les dièses. *)
let of_string s =
  match s with
  | "C" -> C
  | "#C" | "C#" | "bD" | "Db" -> CD
  | "D" -> D
  | "D#" | "Eb"  | "#D" | "bE" -> DD
  | "E" -> E
  | "F" -> F
  | "F#" | "Gb"  | "#F" | "bG" -> FD
  | "G" -> G
  | "G#" | "Ab"  | "#G" | "bA" -> GD
  | "A" -> A
  | "A#" | "Bb"  | "#A" | "bB" -> AD
  | "B" -> B
  | _ -> raise (Unknown_Notation s)
;;

(* Les intervalles absolus. *)

let decale_chrom n i = of_int ((to_int n + i) mod 12)
let next_chrom n = of_int ((to_int n + 1) mod 12)

exception Impossible_note of string

let diesify n =
  match n with
  | E | B | CD | DD | FD | GD | AD -> raise (Impossible_note (to_string n^"#"))
  | _ -> decale_chrom n 1

let bemolify n =
  match n with
  | F | C | CD | DD | FD | GD | AD -> raise (Impossible_note (to_string n^"b"))
  | _ -> decale_chrom n (-1)


(* Gamme majeure, sans tenir compte du fait que les altérations # et ♭
   ne tombent pas au milieu d'un ton. Pour l'instant on se base sur les intervalles simples:
   https://theoriemusicale.camilleroux.com/intervalles   *)

let seconde_mineure n = decale_chrom n 1
let seconde_majeure n = decale_chrom n 2
let seconde = seconde_majeure;;
let tierce_mineure n = decale_chrom n 3
let tierce_majeure n = decale_chrom n 4
let quarte_juste n = decale_chrom n 5
let quarte_diminuee = tierce_majeure ;;
let quarte_augmentee n = decale_chrom n 6
let quinte_diminuee = quarte_augmentee ;;
let quinte_juste n = decale_chrom n 7
let quinte_augmentee n = decale_chrom n 8
let sixte_mineure = quinte_augmentee
let sixte_majeure n = decale_chrom n 9
let sixte = sixte_majeure;;
let septieme_mineure n = decale_chrom n 10
let septieme_majeure n = decale_chrom n 11


let pr fmt n = Format.fprintf fmt "%s" (to_string n)
let pr_l fmt l = Format.fprintf fmt "%a" (Pp.print_list Pp.comma pr) l

let parse_list s =
  let ls = String.split_on_char ' ' s in
  List.map of_string (List.filter (String.equal "") ls)
