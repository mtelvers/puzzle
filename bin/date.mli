type date = {
  month : int;
  day : int;
}

val v : int -> int -> date
val day : date -> int
val day_string : date -> string
val month : date -> int
val month_string : date -> string
val short_month_string : date -> string
val year : date list
val valid : date -> bool
val pair : date -> int * int
