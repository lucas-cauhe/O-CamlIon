exception BadTree

module type DB = sig
  type 'a db
  type leaf_nodes
  val empty : 'a db * leaf_nodes
  val search : 'a db * leaf_nodes -> 'a -> (int * int) option
  val insert : 'a db * leaf_nodes -> int -> 'a -> 'a db * leaf_nodes
  val delete : 'a db * leaf_nodes -> 'a -> 'a db * leaf_nodes
  val draw_tree : 'a db -> ('a -> unit) -> unit
  val draw_leaves : leaf_nodes -> unit
end