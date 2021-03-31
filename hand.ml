open Bag

let bag = init_bag
let rec generate_hand hand = if List.length hand < 7 then let tile = snd (next_tile bag) in
generate_hand ( {letter = tile.letter; value = tile.value } :: hand) else hand  
(* let new_hand = generate_hand []    *)
let print_hor hand =  
  print_endline"+---------------------------+";
  print_endline (
  "| "  ^ (Char.escaped (List.nth hand 0).letter)  ^
  " | " ^ (Char.escaped (List.nth hand 1).letter)  ^
  " | " ^ (Char.escaped (List.nth hand 2).letter)  ^
  " | " ^ (Char.escaped (List.nth hand 3).letter)  ^
  " | " ^ (Char.escaped (List.nth hand 4).letter)  ^
  " | " ^ (Char.escaped (List.nth hand 5).letter)  ^
  " | " ^ (Char.escaped (List.nth hand 6).letter)  ^ 
  " |");
  print_endline (
  "| "  ^ (string_of_int (List.nth hand 0).value)  ^
  " | " ^ (string_of_int (List.nth hand 1).value)  ^
  " | " ^ (string_of_int (List.nth hand 2).value)  ^
  " | " ^ (string_of_int (List.nth hand 3).value)  ^
  " | " ^ (string_of_int (List.nth hand 4).value)  ^
  " | " ^ (string_of_int (List.nth hand 5).value)  ^
  " | " ^ (string_of_int (List.nth hand 6).value)  ^ 
  " |");
  print_endline"+---------------------------+"


 let print_vert hand = 
  print_endline"+-----+";
  print_endline("| " ^ (Char.escaped (List.nth hand 0).letter)  ^ " " ^ (string_of_int (List.nth hand 0).value) ^ " |");
  print_endline("| " ^ (Char.escaped (List.nth hand 1).letter)  ^ " " ^ (string_of_int (List.nth hand 1).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (List.nth hand 2).letter)  ^ " " ^ (string_of_int (List.nth hand 2).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (List.nth hand 3).letter)  ^ " " ^ (string_of_int (List.nth hand 3).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (List.nth hand 4).letter)  ^ " " ^ (string_of_int (List.nth hand 4).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (List.nth hand 5).letter)  ^ " " ^ (string_of_int (List.nth hand 5).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (List.nth hand 6).letter)  ^ " " ^ (string_of_int (List.nth hand 6).value)  ^ " |");
  print_endline ("+-----+"); 
 




 