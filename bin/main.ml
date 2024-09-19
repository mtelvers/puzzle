let shape1 = [ Cartesian.origin; { x = 1; y = 0 }; { x = 2; y = 0 }; { x = 0; y = 1 }; { x = 0; y = 2 } ]
let shape2 = [ Cartesian.origin; { x = 1; y = 0 }; { x = 1; y = 1 }; { x = 2; y = 1 }; { x = 3; y = 1 } ]
let shape3 = [ Cartesian.origin; { x = 1; y = 0 }; { x = 1; y = 1 }; { x = 1; y = 2 }; { x = 2; y = 2 } ]
let shape4 = [ Cartesian.origin; { x = 1; y = 0 }; { x = 2; y = 0 }; { x = 0; y = 1 }; { x = 1; y = 1 } ]
let shape5 = [ Cartesian.origin; { x = 1; y = 0 }; { x = 2; y = 0 }; { x = 0; y = 1 }; { x = 1; y = 1 }; { x = 2; y = 1 } ]
let shape6 = [ Cartesian.origin; { x = 1; y = 0 }; { x = 2; y = 0 }; { x = 0; y = 1 }; { x = 2; y = 1 } ]
let shape7 = [ Cartesian.origin; { x = 1; y = 0 }; { x = 2; y = 0 }; { x = 3; y = 0 }; { x = 0; y = 1 } ]
let shape8 = [ Cartesian.origin; { x = 1; y = 0 }; { x = 2; y = 0 }; { x = 3; y = 0 }; { x = 1; y = 1 } ]

let shape_alternatives shape =
  let mirrored_shape = Cartesian.mirror_shape shape in
  List.map (Cartesian.rotate_shape shape) [ 0; 90; 180; 270 ] @ List.map (Cartesian.rotate_shape mirrored_shape) [ 0; 90; 180; 270 ]
  |> List.map Cartesian.normalise |> List.sort_uniq Cartesian.compare_coord_list

let shapes =
  [
    ('1', shape_alternatives shape1);
    ('2', shape_alternatives shape2);
    ('3', shape_alternatives shape3);
    ('4', shape_alternatives shape4);
    ('5', shape_alternatives shape5);
    ('6', shape_alternatives shape6);
    ('7', shape_alternatives shape7);
    ('8', shape_alternatives shape8);
  ]

let () =
  List.iter
    (fun (ch, sa) ->
      let () = Printf.printf "Shape %c\n" ch in
      List.iter
        (fun s ->
          for y = 3 downto -3 do
            Printf.printf "% 2i " y;
            for x = -3 to 3 do
              if List.mem (Cartesian.pos x y) s then Printf.printf "%c" ch else Printf.printf " "
            done;
            Printf.printf "\n"
          done;
          Printf.printf "\n")
        sa)
    shapes

let puzzle date =
  let month, day = Date.pair date in
  Board.place [ { x = (month - 1) mod 6; y = (month - 1) / 6 } ] Cartesian.origin '*' Board.data
  |> Option.value ~default:Board.data
  |> Board.place [ { x = (day - 1) mod 7; y = ((day - 1) / 7) + 2 } ] Cartesian.origin '*'
  |> Option.value ~default:Board.data

let rec try_shapes board shapes lst =
  match shapes with
  | [] -> board :: lst
  | (ch, shape_rotations) :: tl ->
      let free = Board.free_spaces board in
      List.fold_left
        (fun lst shape ->
          List.fold_left
            (fun lst pos ->
              match Board.place shape pos ch board with
              | Some board -> try_shapes board tl lst
              | None -> lst)
            lst free)
        lst shape_rotations

(*
let () =
  List.iter
    (fun date ->
      let p = puzzle date in
      let () = Board.print p in
      let solutions = try_shapes p shapes [] in
      emit_page ("html/" ^ Html.to_filename date) (Html.home_page_doc solutions date))
    Date.year
*)

module T = Domainslib.Task

let num_domains =
  try int_of_string Sys.argv.(1) with
  | _ -> 1

let pool = T.setup_pool ~num_domains ()

let _ =
  T.run pool (fun _ ->
      List.fold_left
        (fun promises date ->
          let p = puzzle date in
          let () = Board.print p in
          let prom =
            T.async pool (fun _ ->
                let solutions = try_shapes p shapes [] in
                Html.emit_page ("html/" ^ Html.to_filename date) (Html.home_page_doc solutions date))
          in
          prom :: promises)
        [] Date.year
      |> List.map (T.await pool))

let () = T.teardown_pool pool
