type date = {
  month : int;
  day : int;
}

let v month day = { month; day }
let day date = date.day
let day_string date = string_of_int date.day
let month date = date.month
let pair date = (date.month, date.day)

let month_string date =
  match date.month with
  | 1 -> "January"
  | 2 -> "February"
  | 3 -> "March"
  | 4 -> "April"
  | 5 -> "May"
  | 6 -> "June"
  | 7 -> "July"
  | 8 -> "August"
  | 9 -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
  | _ -> ""

let short_month_string date =
  match date.month with
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "Jun"
  | 7 -> "Jul"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | 12 -> "Dec"
  | _ -> ""

let days_in_month = function
  | 1 -> 31
  | 2 -> 29
  | 3 -> 31
  | 4 -> 30
  | 5 -> 31
  | 6 -> 30
  | 7 -> 31
  | 8 -> 31
  | 9 -> 30
  | 10 -> 31
  | 11 -> 30
  | 12 -> 31
  | _ -> 0

let year =
  List.init 12 (fun m -> m + 1)
  |> List.map (fun month ->
         List.init (days_in_month month) (fun d ->
             let day = d + 1 in
             { month; day }))
  |> List.flatten

(*
let year = [ { month = 4; day = 6 } ; { month = 8; day = 8 }]
           *)
let valid date = date.day >= 1 && date.day <= days_in_month date.month
