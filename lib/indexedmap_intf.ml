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

  val length: 'a t -> int

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
  (** [rank k m] is the number of elements in [m] whose keys are strictly less than [k]. *)

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
