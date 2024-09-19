open Tyxml.Html

let colour = function
  | '1' -> `Color ("#e6194B", None)
  | '2' -> `Color ("#f58231", None)
  | '3' -> `Color ("#ffe119", None)
  | '4' -> `Color ("#bfef45", None)
  | '5' -> `Color ("#3cb44b", None)
  | '6' -> `Color ("#42d4f4", None)
  | '7' -> `Color ("#4363d8", None)
  | '8' -> `Color ("#911eb4", None)
  | _ -> `Color ("#000000", None)

let svg_board board =
  let lst = List.init (String.length board) (String.get board) in
  svg
    ~a:[ Tyxml.Svg.a_height (72., None); Tyxml.Svg.a_width (72., None) ]
    (Tyxml.Svg.polyline
       ~a:
         [
           Tyxml.Svg.a_fill `None;
           Tyxml.Svg.a_stroke (`Color ("#000000", None));
           Tyxml.Svg.a_points [ (0., 0.); (62., 0.); (62., 20.); (72., 20.); (72., 62.); (32., 62.); (32., 72.); (0., 72.); (0., 0.) ];
         ]
       []
    :: (List.mapi
          (fun i ch ->
            match ch with
            | '*' -> None
            | _ ->
                Some
                  (let x = 1. +. (10. *. float_of_int (i mod 7)) in
                   let y = 1. +. (10. *. float_of_int (i / 7)) in
                   Tyxml.Svg.rect
                     ~a:
                       [
                         Tyxml.Svg.a_fill (colour ch);
                         Tyxml.Svg.a_x (x, None);
                         Tyxml.Svg.a_y (y, None);
                         Tyxml.Svg.a_width (10., None);
                         Tyxml.Svg.a_height (10., None);
                       ]
                     []))
          lst
       |> List.filter_map (fun x -> x)))

let to_date_string date = Printf.sprintf "%i %s" (Date.day date) (Date.month_string date)
let to_filename date = Printf.sprintf "%02i-%02i.html" (Date.month date) (Date.day date)

let month_link month day =
  let d = Date.v month day in
  if Date.valid d then a ~a:[ a_href (to_filename d) ] [ txt (Date.short_month_string d) ] else txt (Date.short_month_string d)

let day_link month day =
  let d = Date.v month day in
  if Date.valid d then a ~a:[ a_href (to_filename d) ] [ txt (Date.day_string d) ] else txt (Date.day_string d)

let home_page_doc solutions date =
  let month, day = Date.pair date in
  html
    (head (title (txt (to_date_string date))) [ link ~rel:[ `Stylesheet ] ~href:"home.css" () ])
    (body
       [
         div
           ~a:[ a_class [ "center" ] ]
           [
             h1 [ txt (to_date_string date) ];
             p (List.sort_uniq compare solutions |> List.map svg_board);
             table
               ~a:[ a_class [ "center" ] ]
               [
                 tr ~a:[]
                   [
                     td [ month_link 1 day ];
                     td [ month_link 2 day ];
                     td [ month_link 3 day ];
                     td [ month_link 4 day ];
                     td [ month_link 5 day ];
                     td [ month_link 6 day ];
                   ];
                 tr ~a:[]
                   [
                     td [ month_link 7 day ];
                     td [ month_link 8 day ];
                     td [ month_link 9 day ];
                     td [ month_link 10 day ];
                     td [ month_link 11 day ];
                     td [ month_link 12 day ];
                   ];
                 tr ~a:[]
                   [
                     td [ day_link month 1 ];
                     td [ day_link month 2 ];
                     td [ day_link month 3 ];
                     td [ day_link month 4 ];
                     td [ day_link month 5 ];
                     td [ day_link month 6 ];
                     td [ day_link month 7 ];
                   ];
                 tr ~a:[]
                   [
                     td [ day_link month 8 ];
                     td [ day_link month 9 ];
                     td [ day_link month 10 ];
                     td [ day_link month 11 ];
                     td [ day_link month 12 ];
                     td [ day_link month 13 ];
                     td [ day_link month 14 ];
                   ];
                 tr ~a:[]
                   [
                     td [ day_link month 15 ];
                     td [ day_link month 16 ];
                     td [ day_link month 17 ];
                     td [ day_link month 18 ];
                     td [ day_link month 19 ];
                     td [ day_link month 20 ];
                     td [ day_link month 21 ];
                   ];
                 tr ~a:[]
                   [
                     td [ day_link month 22 ];
                     td [ day_link month 23 ];
                     td [ day_link month 24 ];
                     td [ day_link month 25 ];
                     td [ day_link month 26 ];
                     td [ day_link month 27 ];
                     td [ day_link month 28 ];
                   ];
                 tr ~a:[] [ td [ day_link month 29 ]; td [ day_link month 30 ]; td [ day_link month 31 ] ];
               ];
           ];
       ])

let emit_page name page =
  Printf.printf "Generating: %s\n" name;
  let file_handle = open_out name in
  let fmt = Format.formatter_of_out_channel file_handle in
  Format.fprintf fmt "%a@." (pp ~indent:true ()) page;
  close_out file_handle
