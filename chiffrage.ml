open Note
(* Les intervales en notation "chiffrage d'accord".
   https://fr.wikipedia.org/wiki/Chiffrage_des_accords *)

type alteration = Diese | Bemol| Exact;;

type interval = (int*alteration)

let interval_of_string s =
  let remove_fst x = String.sub x 1 (String.length s - 1) in
  let remove_last x = String.sub x 0 (String.length s - 1) in
  try
    match s.[0] with
    | '#' -> (int_of_string (remove_fst s) , Diese)
    | 'b' -> (int_of_string (remove_fst s) , Bemol)
    | _ ->
       (match s.[String.length s - 1] with
        | '#' -> (int_of_string (remove_last s) , Diese)
        | 'b' -> (int_of_string (remove_last s) , Bemol)
        | _ -> int_of_string s , Exact)
  with _ -> raise (Unknown_Notation s)

let alter n alt =
  match alt with
  | Diese -> of_int ((to_int n + 1) mod 12)
  | Bemol -> of_int ((to_int n - 1) mod 12)
  | Exact -> n;;

let interval n (i,alt) =
  let open SiBemolMajeur in
  let note_sans_alt =
    match i with
    | 2 -> seconde n
    | 3 -> tierce n
    | 4 -> quarte n
    | 5 -> quinte n
    | 6 -> sixte n
    | 7 -> septieme n
    | _ -> failwith ("interval inconnu: "^ string_of_int i) in
  alter note_sans_alt alt

let interval_or_note_to_string basse s =
  try interval basse (interval_of_string s)
  with _ -> of_string s;;

let parse_list s =
  let ls = String.split_on_char ' ' s in
  let ls = List.filter (fun x -> not (String.equal x "")) ls in
  if ls = [] then []
  else
    let basse = Note.of_string (List.hd ls) in
    basse :: List.map (interval_or_note_to_string basse) (List.tl ls)

let interp_interval_de_basse basse (l:interval list): t list =
  let lnotes = List.map (interval basse) l in
  basse :: lnotes
