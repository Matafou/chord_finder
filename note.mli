type t

exception Unknown_Notation of string

val all_notes: t list
val to_int: t -> int
val to_string: t -> string
val of_int: int -> t
val of_string: string -> t
val pr: Format.formatter -> t -> unit
val parse_list: string -> t list

val seconde_mineure : t -> t
val seconde_majeure : t -> t
val seconde : t -> t
val tierce_mineure : t -> t
val tierce_majeure : t -> t
val quarte_juste : t -> t
val quarte_diminuee : t -> t
val quarte_augmentee : t -> t
val quinte_diminuee : t -> t
val quinte_juste : t -> t
val quinte_augmentee : t -> t
val sixte_mineure : t -> t
val sixte_majeure : t -> t
val sixte : t -> t
val septieme_mineure : t -> t
val septieme_majeure : t -> t
