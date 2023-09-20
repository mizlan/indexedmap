exception Insert of string
exception ColorChange of string
exception Empty
exception IndexOutOfBounds of string

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

type 'a tree = E | EE | T of C.t * 'a * 'a tree * 'a tree * int

let blacken t =
  match t with E | EE -> t | T (c, v, l, r, s) -> T (C.blacker c, v, l, r, s)

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

let insert_no_dup x s =
  let rec ins = function
    | E | EE -> T (C.R, x, E, E, 1)
    | T (color, y, a, b, s) as n ->
        if x < y then balance (color, y, ins a, b, s + 1)
        else if x > y then balance (color, y, a, ins b, s + 1)
        else n
  in
  ins s |> to_black

let rec max = function
  | E | EE -> raise Empty
  | T (_, x, _, E, _) -> x
  | T (_, _, _, r, _) -> max r

let rec remove_max = function
  | E | EE -> raise Empty
  | T (_, _, _, E, _) as t -> remove t
  | T (c, x, l, r, s) -> bubble (c, x, l, remove_max r, s)

and remove = function
  | E | EE -> E
  | T (C.R, _, E, E, _) -> E
  | T (C.B, _, E, E, _) -> EE
  | T (C.B, _, E, T (C.R, x, a, b, s), _) | T (C.B, _, T (C.R, x, a, b, s), E, _)
    ->
      T (C.B, x, a, b, s)
  | T (c, _, l, r, s) ->
      let l' = remove_max l and mx = max l in
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
  | E | EE -> Error "empty set"
  | T (_, _, _, _, s) when n >= s || n < 0 -> Error "index out of bounds"
  | T (_, x, l, r, _) ->
      let i = sz l in
      if i = n then Ok x else if n < i then nth l n else nth r (n - i - 1)

let rec rank t k =
  match t with
  | E | EE -> Error "empty set"
  | T (_, x, l, r, _) -> (
      if k > x then
        let ( let* ) = Result.bind in
        let* p = rank r k in
        Ok (sz l + 1 + p)
      else
        match l with
        | E | EE -> Ok 0
        | T (_, x', _, _, _) when x' < x && k = x -> Ok (sz l)
        | _ -> rank l k)

let rec to_list = function
  | E | EE -> []
  | T (_, x, l, r, _) -> to_list l @ [ x ] @ to_list r
