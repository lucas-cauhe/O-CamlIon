exception BadTree

module type KEYS = sig
  type a
  val get_fk : 'o -> a
  val get_pkey : 'o -> a
end

(* module type DB = sig
  val search : 'key -> 'key t -> int option
  val add : 'v -> 'key t -> int option
  val delete : 'key -> 'key t -> 'key t
  val draw : int t -> unit
end *)
