module Make (E : Indexedmap_intf.TOTAL_ORD) :
  Indexedmap_intf.S with type key = E.t = struct
  type key = E.t
  type ordering = LT | GT | EQ

  let cmp a b =
    let o = E.compare a b in
    if o < 0 then LT else if o = 0 then EQ else GT

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

  let to_black t =
    match t with E | EE -> t | T (_, x, v, l, r, s) -> T (C.B, x, v, l, r, s)

  let to_red t =
    match t with E | EE -> t | T (_, x, v, l, r, s) -> T (C.R, x, v, l, r, s)

  let redden t =
    match t with
    | E | EE -> t
    | T (c, x, v, l, r, s) -> T (C.redder c, x, v, l, r, s)

  let is_bb = function EE | T (C.BB, _, _, _, _, _) -> true | _ -> false
  let sz tree = match tree with E | EE -> 0 | T (_, _, _, _, _, s) -> s
  let length tree = match tree with E | EE -> 0 | T (_, _, _, _, _, s) -> s

  let rec mem x tree =
    match tree with
    | E | EE -> false
    | T (_, key, _, left, right, _) -> (
        match cmp x key with EQ -> true | LT -> mem x left | GT -> mem x right)

  let rec find x tree =
    match tree with
    | E | EE -> None
    | T (_, key, v, left, right, _) -> (
        match cmp x key with
        | EQ -> Some (x, v)
        | LT -> find x left
        | GT -> find x right)

  let rec balance = function
    | C.B, z, vz, T (C.R, y, vy, T (C.R, x, vx, a, b, _), c, _), d, _
    | C.B, z, vz, T (C.R, x, vx, a, T (C.R, y, vy, b, c, _), _), d, _
    | C.B, x, vx, a, T (C.R, z, vz, T (C.R, y, vy, b, c, _), d, _), _
    | C.B, x, vx, a, T (C.R, y, vy, b, T (C.R, z, vz, c, d, _), _), _ ->
        let s_a = sz a and s_b = sz b and s_c = sz c and s_d = sz d in
        T
          ( C.R,
            y,
            vy,
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
            y,
            vy,
            T (C.B, x, vx, a, b, s_a + s_b + 1),
            T (C.B, z, vz, c, d, s_c + s_d + 1),
            s_a + s_b + s_c + s_d + 3 )
    | ( C.BB,
        x,
        vx,
        a,
        T
          ( C.NB,
            z,
            vz,
            T (C.B, y, vy, b, c, _),
            (T (C.B, _, _, _, _, _) as d),
            _ ),
        _ ) ->
        let s_a = sz a and s_b = sz b and s_c = sz c and s_d = sz d in
        T
          ( C.B,
            y,
            vy,
            T (C.B, x, vx, a, b, s_a + s_b + 1),
            balance (C.B, z, vz, c, to_red d, s_c + s_d + 1),
            s_a + s_b + s_c + s_d + 3 )
    | ( C.BB,
        z,
        vz,
        T
          ( C.NB,
            x,
            vx,
            (T (C.B, _, _, _, _, _) as a),
            T (C.B, y, vy, b, c, _),
            _ ),
        d,
        _ ) ->
        let s_a = sz a and s_b = sz b and s_c = sz c and s_d = sz d in
        T
          ( C.B,
            y,
            vy,
            balance (C.B, x, vx, to_red a, b, s_a + s_b + 1),
            T (C.B, z, vz, c, d, s_c + s_d + 1),
            s_a + s_b + s_c + s_d + 3 )
    | a, b, c, d, e, s -> T (a, b, c, d, e, s)

  let bubble = function
    | color, x, v, l, r, s when is_bb l || is_bb r ->
        balance (C.blacker color, x, v, redden l, redden r, s)
    | color, x, v, l, r, s -> balance (color, x, v, l, r, s)

  let add k v m =
    let add_size = if not (mem k m) then 1 else 0 in
    let rec ins = function
      | E | EE -> T (C.R, k, v, E, E, 1)
      | T (color, x, v', a, b, s) -> (
          match cmp k x with
          | LT -> T (color, x, v', ins a, b, s + add_size)
          | GT -> T (color, x, v', a, ins b, s + add_size)
          | EQ -> T (color, k, v, a, b, s))
    in
    ins m |> to_black

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
    | T (c, x, v, l, r, s) -> bubble (c, x, v, l, internal_remove_max r, s)

  and internal_remove_root = function
    | E | EE -> E
    | T (C.R, _, _, E, E, _) -> E
    | T (C.B, _, _, E, E, _) -> EE
    | T (C.B, _, _, E, T (C.R, x, v, a, b, s), _)
    | T (C.B, _, _, T (C.R, x, v, a, b, s), E, _) ->
        T (C.B, x, v, a, b, s)
    | T (c, _, _, l, r, s) ->
        let l' = internal_remove_max l
        and mx_x, mx_v = Option.get (find_max l) in
        bubble (c, mx_x, mx_v, l', r, s)

  let remove x t =
    let rec del = function
      | E | EE -> E
      | T (c, y, v, a, b, s) as tr -> (
          match cmp x y with
          | LT -> bubble (c, y, v, del a, b, s)
          | GT -> bubble (c, y, v, a, del b, s)
          | EQ -> internal_remove_root tr)
    in
    del t |> to_black

  let update k f m =
    match find k m with
    | None -> ( match f None with None -> m | Some v -> add k v m)
    | Some (_, v) -> (
        match f (Some v) with None -> remove k m | Some v' -> add k v' m)

  let rec nth n t =
    match t with
    | E | EE -> None
    | T (_, _, _, _, _, s) when n >= s || n < 0 -> None
    | T (_, x, v, l, r, _) ->
        let i = sz l in
        if i = n then Some (x, v)
        else if n < i then nth n l
        else nth (n - i - 1) r

  let rec rank k t =
    match t with
    | E | EE -> 0
    | T (_, x, _, l, r, _) -> (
        match cmp k x with
        | GT -> sz l + 1 + rank k r
        | EQ -> sz l
        | LT -> rank k l)

  let to_list t =
    let rec go acc = function
      | E | EE -> acc
      | T (_, x, v, l, r, _) ->
          let acc' = go acc r in
          go ((x, v) :: acc') l
    in
    go [] t
end
