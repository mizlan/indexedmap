module type TOTAL_ORD = sig
  type t

  val compare : t -> t -> int
  (** [compare a b] shall return
      a negative value if [a] is smaller than [b],
      [0] if [a] and [b] are equal or
      a positive value if [a] is greater than [b] *)
end

module type S = sig
  type key
  type !+'a t

  val empty : 'a t
  (** [empty] returns the empty map. *)

  val is_empty : 'a t -> bool
  (** [is_empty s] returns [true] if the map [s] is empty. *)

  exception Empty

  val mem : key -> 'a t -> bool
  (** [mem k m] returns whether or not the key [k] is present in [m] *)

  val find : key -> 'a t -> (key * 'a) option
  (** [find k m] returns the value associated with k in [m] *)

  val add : key -> 'a -> 'a t -> 'a t
  (** [add k v m] inserts an element [k] into the map [m] if it isn't already present, otherwise returns [m]. *)

  val remove : key -> 'a t -> 'a t
  (** [remove k m] deletes [k] and its associated value from the map [m]. *)

  val nth : int -> 'a t -> (key * 'a) option
  (** [nth s n] is the nth element of x when viewed in sorted order. *)

  val rank : key -> 'a t -> int
  (** [rank s x] is the number of elements in [s] strictly less than x. *)

  val find_min : 'a t -> (key * 'a) option
  (** [find_min k m] is the smallest key (and its associated value) in [m] or None if empty. *)

  val find_max : 'a t -> (key * 'a) option
  (** [find_max k m] is the largest key (and its associated value) in [m] or None if empty. *)

  val find_first : (key -> bool) -> 'a t -> (key * 'a) option
  (** [find_first f s] where f is a monotonically-increasing function, 
      is the first element x in [s] such that f x is true, or None if none satisfy. *)

  val to_list : 'a t -> (key * 'a) list
  (** [to_list s] is the sorted list representation of the map [s]. *)
end

module Make (E : TOTAL_ORD) : S with type key = E.t = struct
  type key = E.t

  module C = struct
    type t = R | B | BB | NB

    exception ColorChange of string

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

  type !+'a t = E | EE | T of C.t * key * 'a * 'a t * 'a t * int

  let empty = E
  let is_empty = function E -> true | _ -> false

  exception Empty

  (* let blacken t = *)
  (*   match t with E | EE -> t | T (c, v, l, r, s) -> T (C.blacker c, v, l, r, s) *)

  let to_black t =
    match t with E | EE -> t | T (_, x, v, l, r, s) -> T (C.B, x, v, l, r, s)

  let redden t =
    match t with E | EE -> t | T (c, x, v, l, r, s) -> T (C.redder c, x, v, l, r, s)

  let is_bb = function EE | T (C.BB, _, _, _, _, _) -> true | _ -> false
  let sz tree = match tree with E | EE -> 0 | T (_, _, _, _, _, s) -> s

  let rec mem x tree =
    match tree with
    | E | EE -> false
    | T (_, key, _, left, right, _) ->
        if x == key then true
        else if x < key then mem x left
        else mem x right

  let rec find x tree =
    match tree with
    | E | EE -> None
    | T (_, key, v, left, right, _) ->
        if x == key then Some (x, v)
        else if x < key then find x left
        else find x right

  let rec balance = function
    | C.B, z, vz, T (C.R, y, vy, T (C.R, x, vx, a, b, _), c, _), d, _
    | C.B, z, vz, T (C.R, x, vx, a, T (C.R, y, vy, b, c, _), _), d, _
    | C.B, x, vx, a, T (C.R, z, vz, T (C.R, y, vy, b, c, _), d, _), _
    | C.B, x, vx, a, T (C.R, y, vy, b, T (C.R, z, vz, c, d, _), _), _ ->
        let s_a = sz a and s_b = sz b and s_c = sz c and s_d = sz d in
        T
          ( C.R,
            y, vy,
            T (C.B, x, vx, a, b, s_a + s_b + 1),
            T (C.B, z, vz, c, d, s_c + s_d + 1),
            s_a + s_b + s_c + s_d + 3 )
    | C.BB, z, vz, T (C.R, y, vy, T (C.R, x, vx, a, b, _), c, _), d, _
    | C.BB, z, vz, T (C.R, x, vx, a, T (C.R, y, vy, b, c, _), _), d, _
    | C.BB, x, vx, a, T (C.R, z, vz, T (C.R, y, vy, b, c, _), d, _), _
    | C.BB, x, vx, a, T (C.R, y, vy, b, T (C.R, z, vz, c, d, _), _), _ ->
        let s_a = sz a and s_b = sz b and s_c = sz c and s_d = sz d in
        T
          ( C.B,
            y, vy,
            T (C.B, x, vx, a, b, s_a + s_b + 1),
            T (C.B, z, vz, c, d, s_c + s_d + 1),
            s_a + s_b + s_c + s_d + 3 )
    | ( C.BB,
        x, vx,
        a,
        T (C.NB, z, vz, T (C.B, y, vy, b, c, _), (T (C.B, _, _, _, _, _) as d), _),
        _ ) ->
        let s_a = sz a and s_b = sz b and s_c = sz c and s_d = sz d in
        T
          ( C.B,
            y, vy,
            T (C.B, x, vx, a, b, s_a + s_b + 1),
            balance (C.B, z, vz, c, redden d, s_c + s_d + 1),
            s_a + s_b + s_c + s_d + 3 )
    | ( C.BB,
        z, vz,
        T (C.NB, x, vx, (T (C.B, _, _, _, _, _) as a), T (C.B, y, vy, b, c, _), _),
        d,
        _ ) ->
        let s_a = sz a and s_b = sz b and s_c = sz c and s_d = sz d in
        T
          ( C.B,
            y, vy,
            balance (C.B, x, vx, redden a, b, s_a + s_b + 1),
            T (C.B, z, vz, c, d, s_c + s_d + 1),
            s_a + s_b + s_c + s_d + 3 )
    | a, b, c, d, e, s -> T (a, b, c, d, e, s)

  let bubble = function
    | color, x, v, l, r, s when is_bb l || is_bb r ->
        balance (C.blacker color, x, v, redden l, redden r, s)
    | color, x, v, l, r, s -> balance (color, x, v, l, r, s)

  let add x v s =
    let rec ins = function
      | E | EE -> T (C.R, x, v, E, E, 1)
      | T (color, y, v, a, b, s) as n ->
          if x < y then balance (color, y, v, ins a, b, s + 1)
          else if x > y then balance (color, y, v, a, ins b, s + 1)
          else n
    in
    ins s |> to_black

  (* let insert_no_dup x s = *)
  (*   let rec ins = function *)
  (*     | E | EE -> T (C.R, x, E, E, 1) *)
  (*     | T (color, y, a, b, s) as n -> *)
  (*         if x < y then balance (color, y, ins a, b, s + 1) *)
  (*         else if x > y then balance (color, y, a, ins b, s + 1) *)
  (*         else n *)
  (*   in *)
  (*   ins s |> to_black *)

  let rec find_min = function
    | E | EE -> None
    | T (_, x, v, E, _, _) -> Some (x, v)
    | T (_, _, _, l, _, _) -> find_min l

  let rec find_max = function
    | E | EE -> None
    | T (_, x, v, _, E, _) -> Some (x, v)
    | T (_, _, _, _, r, _) -> find_max r

  let rec find_first f s =
    match s with
    | E | EE -> None
    | T (_, x, v, l, _, _) when f x ->
        Some (Option.value (find_first f l) ~default:(x, v))
    | T (_, _, _, _, r, _) -> find_first f r

  let rec internal_remove_max = function
    | E | EE -> raise Empty
    | T (_, _, _, _, E, _) as t -> internal_remove_root t
    | T (c, x, v , l, r, s) -> bubble (c, x, v, l, internal_remove_max r, s)

  and internal_remove_root = function
    | E | EE -> E
    | T (C.R, _, _, E, E, _) -> E
    | T (C.B, _, _, E, E, _) -> EE
    | T (C.B, _, _, E, T (C.R, x, v, a, b, s), _)
    | T (C.B, _, _, T (C.R, x, v, a, b, s), E, _) ->
        T (C.B, x, v, a, b, s)
    | T (c, _, _, l, r, s) ->
        let l' = internal_remove_max l and (mx_x, mx_v) = Option.get (find_max l) in
        bubble (c, mx_x, mx_v, l', r, s)

  let remove x t =
    let rec del = function
      | E | EE -> E
      | T (c, y, v, a, b, s) as tr ->
          if x < y then bubble (c, y, v, del a, b, s)
          else if x > y then bubble (c, y, v, a, del b, s)
          else internal_remove_root tr
    in
    del t |> to_black

  let rec nth n t =
    match t with
    | E | EE -> None
    | T (_, _, _, _, _, s) when n >= s || n < 0 -> None
    | T (_, x, v, l, r, _) ->
        let i = sz l in
        if i = n then Some (x, v) else if n < i then nth n l else nth (n - i - 1) r

  let rec rank k t =
    match t with
    | E | EE -> 0
    | T (_, x, _, l, r, _) -> (
        if k > x then
          let p = rank k r in
          sz l + 1 + p
        else
          match l with
          | E | EE -> 0
          | T (_, x', _, _, _, _) when x' < x && k = x -> sz l
          | _ -> rank k l)

  let rec to_list = function
    | E | EE -> []
    | T (_, x, v, l, r, _) -> to_list l @ [ (x, v) ] @ to_list r
end
