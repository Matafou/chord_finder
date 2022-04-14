open Note

type t = { name:string; tonique: Note.t; autres: Note.t list }

let mk s n l = { name = s; tonique = n ; autres = l }

let tos = to_string

let accord_majeur n = mk (tos n) n [ tierce_majeure n; quinte_juste n ]
let accord_mineur n = mk (tos n ^ "m") n [ tierce_mineure n; quinte_juste n ]
let accord_majeur_7m n =
  mk (tos n ^ "7") n [ tierce_majeure n; quinte_juste n ; septieme_mineure n ]

let accord_majeur_6 n =
  mk (tos n ^ "6") n [ tierce_majeure n; quinte_juste n ; sixte n ]
let accord_mineur_6m n =
  mk (tos n ^ "m6") n [ tierce_mineure n; quinte_juste n ; sixte n ]

let accord_diminue n =
  mk (tos n ^ "dim") n [ tierce_mineure n; quinte_diminuee n ]

let accord_augmente n =
  mk (tos n ^ "aug") n [ tierce_mineure n; quinte_augmentee n ]

let accord_majeur_7M n =
  mk (tos n ^ "7M") n [ tierce_majeure n; quinte_juste n ; septieme_majeure n ]
let accord_mineur_7m n =
  mk (tos n ^ "m7") n [ tierce_mineure n; quinte_juste n ; septieme_mineure n ]
let accord_mineur_7M n =
  mk (tos n ^  "m7M") n [ tierce_mineure n; quinte_juste n ; septieme_majeure n ]

let all_chord_makers = [ accord_majeur; accord_mineur; accord_majeur_7M; accord_mineur_7m; accord_majeur_6;accord_mineur_6m ;
                       accord_diminue; accord_augmente ]

let chain_chord_makers l =
  List.flatten (List.map (fun f -> List.map (fun n -> f n) all_notes) l)

let notes_of_chord chord = chord.tonique::chord.autres

(* Printing *)

let pr fmt (ac:t) = Format.fprintf fmt "%s" ac.name

let pr_fixed_width n fmt (ac:t) =
  let lgth = String.length ac.name in
  let spaces = String.init (max 0 (n-lgth)) (fun _ -> ' ') in
  Format.fprintf fmt "%s%s" ac.name spaces

