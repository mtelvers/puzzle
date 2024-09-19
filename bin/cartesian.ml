type coord = {
  x : int;
  y : int;
}

type shape = coord list

let pos x y = { x; y }
let origin = { x = 0; y = 0 }

let rotate deg p =
  match deg with
  | 0 -> p
  | 90 -> { x = p.y; y = -p.x }
  | 180 -> { x = -p.x; y = -p.y }
  | 270 -> { x = -p.y; y = p.x }
  | _ -> assert false

let normalise shape =
  let max_x = List.fold_left (fun acc p -> max acc p.x) 0 shape in
  let min_x = List.fold_left (fun acc p -> min acc p.x) max_x shape in
  let max_y = List.fold_left (fun acc p -> max acc p.y) 0 shape in
  let min_y = List.fold_left (fun acc p -> if p.x = min_x then min acc p.y else acc) max_y shape in
  List.map (fun { x; y } -> { x = x + -min_x; y = y + -min_y }) shape

let ordinal p = if p.x >= 0 && p.x < 7 && p.y >= 0 && p.y < 7 then Some ((p.y * 7) + p.x) else None
let compare_coord p1 p2 = if compare p1.y p2.y <> 0 then compare p1.y p2.y else compare p1.x p2.x
let compare_coord_list l1 l2 = List.compare compare_coord l1 l2
let rotate_shape shape deg = List.map (rotate deg) shape |> List.sort compare_coord
let mirror p = { x = -p.x; y = p.y }
let mirror_shape shape = List.map mirror shape |> List.sort compare_coord
let add p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }
