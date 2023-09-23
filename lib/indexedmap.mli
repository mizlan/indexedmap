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

module Make (E : TOTAL_ORD) : S with type elt = E.t
