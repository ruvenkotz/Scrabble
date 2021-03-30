type tile = {
  letter : char;
  value : int;
}

let hand = []
let index_r i = Char.chr (i + 65)

(**Generates a fake hand to test the print functions*)
let rec generate_hand hand = if List.length hand < 7 then 
  generate_hand ( {letter = index_r (Random.int 25); value = Random.int 10} :: hand) else hand

let new_hand = generate_hand hand


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
 




 