type coord = {
  x : int;
  y : int;
}

type shape = coord list

val origin : coord
val pos : int -> int -> coord
val rotate_shape : shape -> int -> shape
val mirror_shape : shape -> shape
val ordinal : coord -> int option
val add : coord -> coord -> coord
val compare_coord_list : shape -> shape -> int
val normalise : shape -> shape
