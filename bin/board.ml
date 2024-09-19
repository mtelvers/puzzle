(*let data = "JFMAMJ*JASOND*1234567890123456789012345678901****" *)
let data = "      *      *                               ****"
let get = String.get
let set str pos ch = [ String.sub str 0 pos; String.make 1 ch; String.sub str (pos + 1) (49 - pos - 1) ] |> String.concat ""

let print b =
  let () = print_endline "+------+" in
  List.iteri
    (fun y cols ->
      let () = print_char '|' in
      for x = 0 to cols do
        print_char (get b (Option.value ~default:0 (Cartesian.ordinal { x; y })))
      done;
      print_endline "|")
    [ 5; 5; 6; 6; 6; 6; 2 ];
  print_endline "+---+"

let place shape at ch board =
  List.fold_left
    (fun b p ->
      match (b, Cartesian.ordinal (Cartesian.add at p)) with
      | None, _
      | _, None ->
          None
      | Some b, Some o -> if get b o = ' ' then Some (set b o ch) else None)
    (Some board) shape

let free_spaces board =
  let _, lst =
    String.fold_left
      (fun (i, l) ch ->
        match ch with
        | ' ' -> (i + 1, Cartesian.pos (i mod 7) (i / 7) :: l)
        | _ -> (i + 1, l))
      (0, []) board
  in
  lst
