exception ColorChange of string

module type TOTAL_ORD = sig
  type t

  val compare : t -> t -> int
  (** [compare a b] shall return
      a negative value if [a] is smaller than [b],
      [0] if [a] and [b] are equal or
      a positive value if [a] is greater than [b] *)
end

module type S = sig
  type elt
  type t

  val empty : t
  (** [empty] returns the empty set. *)

  val is_empty : t -> bool
  (** [is_empty s] returns [true] if the set [s] is empty. *)

  exception Empty

  val member : elt -> t -> bool
  (** [member x s] returns whether or not [x] is present in [s] *)

  val insert : elt -> t -> t
  (** [insert x s] inserts an element [x] into the set [s]. *)

  val insert_no_dup : elt -> t -> t
  (** [insert x s] inserts an element [x] into the set [s] if it isn't already present, otherwise returns [s]. *)

  val add : t -> elt -> t
  (** [add s x] inserts an element [x] into the set [s]. *)

  val delete : t -> elt -> t
  (** [delete s x] deletes a copy of [x] from the set [s]. *)

  val nth : t -> int -> elt option
  (** [nth s n] is the nth element of x when viewed in sorted order. *)

  val rank : t -> elt -> int
  (** [rank s x] is the number of elements in [s] strictly less than x. *)

  val find_min : t -> elt option
  (** [find_min s] is the smallest element in [s] or None if empty. *)

  val find_max : t -> elt option
  (** [find_min s] is the largest element in [s] or None if empty. *)

  val find_first : (elt -> bool) -> t -> elt option
  (** [find_first f s] where f is a monotonically-increasing function, 
      is the first element x in [s] such that f x is true, or None if none satisfy. *)

  val to_list : t -> elt list
  (** [to_list s] is the sorted list representation of the set [s]. *)
end

module Make (E : TOTAL_ORD) : S with type elt = E.t = struct
  type elt = E.t

  module C = struct
    type t = R | B | BB | NB

    let blacker = function
      | NB -> R
      | R -> B
      | B -> BB
      | BB -> raise (ColorChange "attempt to blacken DoubleBlack")

    let redder = function
      | BB -> B
      | B -> R
      | R -> NB
      | NB -> raise (ColorChange "attempt to redden NegativeBlack")
  end

  type t = E | EE | T of C.t * elt * t * t * int

  let empty = E
  let is_empty = function E -> true | _ -> false

  exception Empty

  (* let blacken t = *)
  (*   match t with E | EE -> t | T (c, v, l, r, s) -> T (C.blacker c, v, l, r, s) *)

  let to_black t =
    match t with E | EE -> t | T (_, x, l, r, s) -> T (C.B, x, l, r, s)

  let redden t =
    match t with E | EE -> t | T (c, v, l, r, s) -> T (C.redder c, v, l, r, s)

  let is_bb = function EE | T (C.BB, _, _, _, _) -> true | _ -> false
  let sz tree = match tree with E | EE -> 0 | T (_, _, _, _, s) -> s

  let rec member x tree =
    match tree with
    | E | EE -> false
    | T (_, value, left, right, _) ->
        if x == value then true
        else if x < value then member x left
        else member x right

  let rec balance = function
    | C.B, z, T (C.R, y, T (C.R, x, a, b, _), c, _), d, _
    | C.B, z, T (C.R, x, a, T (C.R, y, b, c, _), _), d, _
    | C.B, x, a, T (C.R, z, T (C.R, y, b, c, _), d, _), _
    | C.B, x, a, T (C.R, y, b, T (C.R, z, c, d, _), _), _ ->
        let s_a = sz a and s_b = sz b and s_c = sz c and s_d = sz d in
        T
          ( C.R,
            y,
            T (C.B, x, a, b, s_a + s_b + 1),
            T (C.B, z, c, d, s_c + s_d + 1),
            s_a + s_b + s_c + s_d + 3 )
    | C.BB, z, T (C.R, y, T (C.R, x, a, b, _), c, _), d, _
    | C.BB, z, T (C.R, x, a, T (C.R, y, b, c, _), _), d, _
    | C.BB, x, a, T (C.R, z, T (C.R, y, b, c, _), d, _), _
    | C.BB, x, a, T (C.R, y, b, T (C.R, z, c, d, _), _), _ ->
        let s_a = sz a and s_b = sz b and s_c = sz c and s_d = sz d in
        T
          ( C.B,
            y,
            T (C.B, x, a, b, s_a + s_b + 1),
            T (C.B, z, c, d, s_c + s_d + 1),
            s_a + s_b + s_c + s_d + 3 )
    | ( C.BB,
        x,
        a,
        T (C.NB, z, T (C.B, y, b, c, _), (T (C.B, _, _, _, _) as d), _),
        _ ) ->
        let s_a = sz a and s_b = sz b and s_c = sz c and s_d = sz d in
        T
          ( C.B,
            y,
            T (C.B, x, a, b, s_a + s_b + 1),
            balance (C.B, z, c, redden d, s_c + s_d + 1),
            s_a + s_b + s_c + s_d + 3 )
    | ( C.BB,
        z,
        T (C.NB, x, (T (C.B, _, _, _, _) as a), T (C.B, y, b, c, _), _),
        d,
        _ ) ->
        let s_a = sz a and s_b = sz b and s_c = sz c and s_d = sz d in
        T
          ( C.B,
            y,
            balance (C.B, x, redden a, b, s_a + s_b + 1),
            T (C.B, z, c, d, s_c + s_d + 1),
            s_a + s_b + s_c + s_d + 3 )
    | a, b, c, d, s -> T (a, b, c, d, s)

  let bubble = function
    | color, x, l, r, s when is_bb l || is_bb r ->
        balance (C.blacker color, x, redden l, redden r, s)
    | color, x, l, r, s -> balance (color, x, l, r, s)

  let insert x s =
    let rec ins = function
      | E | EE -> T (C.R, x, E, E, 1)
      | T (color, y, a, b, s) ->
          if x < y then balance (color, y, ins a, b, s + 1)
          else balance (color, y, a, ins b, s + 1)
    in
    ins s |> to_black

  let add s x = insert x s

  let insert_no_dup x s =
    let rec ins = function
      | E | EE -> T (C.R, x, E, E, 1)
      | T (color, y, a, b, s) as n ->
          if x < y then balance (color, y, ins a, b, s + 1)
          else if x > y then balance (color, y, a, ins b, s + 1)
          else n
    in
    ins s |> to_black

  let rec find_min = function
    | E | EE -> None
    | T (_, x, E, _, _) -> Some x
    | T (_, _, l, _, _) -> find_min l

  let rec find_max = function
    | E | EE -> None
    | T (_, x, _, E, _) -> Some x
    | T (_, _, _, r, _) -> find_max r

  let rec find_first f s =
    match s with
    | E | EE -> None
    | T (_, x, l, _, _) when f x ->
        Some (Option.value (find_first f l) ~default:x)
    | T (_, _, _, r, _) -> find_first f r

  let rec remove_max = function
    | E | EE -> raise Empty
    | T (_, _, _, E, _) as t -> remove t
    | T (c, x, l, r, s) -> bubble (c, x, l, remove_max r, s)

  and remove = function
    | E | EE -> E
    | T (C.R, _, E, E, _) -> E
    | T (C.B, _, E, E, _) -> EE
    | T (C.B, _, E, T (C.R, x, a, b, s), _)
    | T (C.B, _, T (C.R, x, a, b, s), E, _) ->
        T (C.B, x, a, b, s)
    | T (c, _, l, r, s) ->
        let l' = remove_max l and mx = Option.get (find_max l) in
        bubble (c, mx, l', r, s)

  let delete t x =
    let rec del = function
      | E | EE -> E
      | T (c, y, a, b, s) as tr ->
          if x < y then bubble (c, y, del a, b, s)
          else if x > y then bubble (c, y, a, del b, s)
          else remove tr
    in
    del t |> to_black

  let rec nth t n =
    match t with
    | E | EE -> None
    | T (_, _, _, _, s) when n >= s || n < 0 -> None
    | T (_, x, l, r, _) ->
        let i = sz l in
        if i = n then Some x else if n < i then nth l n else nth r (n - i - 1)

  let rec rank t k =
    match t with
    | E | EE -> 0
    | T (_, x, l, r, _) -> (
        if k > x then
          let p = rank r k in
          sz l + 1 + p
        else
          match l with
          | E | EE -> 0
          | T (_, x', _, _, _) when x' < x && k = x -> sz l
          | _ -> rank l k)

  let rec to_list = function
    | E | EE -> []
    | T (_, x, l, r, _) -> to_list l @ [ x ] @ to_list r
end
