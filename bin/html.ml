open Tyxml.Html

let colour = function
  | '1' -> `Color ("#e74c3c", None) (* Darker red *)
  | '2' -> `Color ("#e67e22", None) (* Darker orange *)
  | '3' -> `Color ("#f1c40f", None) (* Darker yellow *)
  | '4' -> `Color ("#badc58", None) (* Darker lime *)
  | '5' -> `Color ("#2ecc71", None) (* Darker green *)
  | '6' -> `Color ("#3498db", None) (* Darker light blue *)
  | '7' -> `Color ("#3867d6", None) (* Darker blue *)
  | '8' -> `Color ("#8e44ad", None) (* Darker purple *)
  | _ -> `Color ("#2d3436", None)
(* Near black *)

let svg_board board =
  let lst = List.init (String.length board) (String.get board) in
  svg
    ~a:[ Tyxml.Svg.a_height (90., None); Tyxml.Svg.a_width (90., None); Tyxml.Svg.a_class [ "puzzle-piece" ] ]
    (Tyxml.Svg.polyline
       ~a:
         [
           Tyxml.Svg.a_fill `None;
           Tyxml.Svg.a_stroke (`Color ("#2d3436", None));
           Tyxml.Svg.a_stroke_width (1.5, None);
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

let month_link current month =
  let d = Date.v month (Date.day current) in
  td
    ~a:[ a_class (if Date.month current = month then [ "current-month" ] else []) ]
    [ (if Date.valid d then a ~a:[ a_href (to_filename d) ] [ txt (Date.short_month_string d) ] else txt (Date.short_month_string d)) ]

let day_link current day =
  let d = Date.v (Date.month current) day in
  td
    ~a:[ a_class (if Date.day current = day then [ "current-day" ] else []) ]
    [ (if Date.valid d then a ~a:[ a_href (to_filename d) ] [ txt (Date.day_string d) ] else txt (Date.day_string d)) ]

let home_page_doc solutions date =
  html
    ~a:[ a_lang "en" ]
    (head
       (title (txt (to_date_string date)))
       [
         meta ~a:[ a_charset "UTF-8" ] ();
         meta ~a:[ a_name "viewport"; a_content "width=device-width, initial-scale=1.0" ] ();
         link ~rel:[ `Stylesheet ] ~href:"home.css" ();
       ])
    (body
       [
         div
           ~a:[ a_class [ "center" ] ]
           [
             h1 [ txt (to_date_string date) ];
             div ~a:[ a_class [ "puzzle-container" ] ] (List.sort_uniq compare solutions |> List.map svg_board);
             nav
               ~a:[ a_class [ "navigation" ] ]
               [
                 div
                   ~a:[ a_class [ "calendar-nav" ] ]
                   [
                     h2 [ txt "Navigate by Month" ];
                     table
                       ~a:[ a_class [ "month-table" ] ]
                       [
                         tr [ month_link date 1; month_link date 2; month_link date 3; month_link date 4; month_link date 5; month_link date 6 ];
                         tr [ month_link date 7; month_link date 8; month_link date 9; month_link date 10; month_link date 11; month_link date 12 ];
                       ];
                     h2 [ txt "Navigate by Day" ];
                     table
                       ~a:[ a_class [ "day-table" ] ]
                       [
                         tr [ day_link date 1; day_link date 2; day_link date 3; day_link date 4; day_link date 5; day_link date 6; day_link date 7 ];
                         tr [ day_link date 8; day_link date 9; day_link date 10; day_link date 11; day_link date 12; day_link date 13; day_link date 14 ];
                         tr [ day_link date 15; day_link date 16; day_link date 17; day_link date 18; day_link date 19; day_link date 20; day_link date 21 ];
                         tr [ day_link date 22; day_link date 23; day_link date 24; day_link date 25; day_link date 26; day_link date 27; day_link date 28 ];
                         tr [ day_link date 29; day_link date 30; day_link date 31 ];
                       ];
                   ];
               ];
             footer
               ~a:[ a_class [ "footer" ] ]
               [ p [ txt "Solutions for "; a ~a:[ a_href "https://www.dragonfjord.com/product/a-puzzle-a-day" ] [ txt "DragonFjord A-Puzzle-A-Day" ] ] ];
           ];
       ])

let emit_page name page =
  Printf.printf "Generating: %s\n" name;
  let file_handle = open_out name in
  let fmt = Format.formatter_of_out_channel file_handle in
  Format.fprintf fmt "%a@." (pp ~indent:true ()) page;
  close_out file_handle
