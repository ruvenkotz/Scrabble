type tile = {
  letter : char;
  value : int;
}

let hand = []
let index_r i = Char.chr (i + 65)


let rec generateHand hand = if List.length hand < 7 then 
  generateHand ( {letter = index_r (Random.int 25); value = Random.int 10} :: hand) else hand

let newhand = generateHand hand

(*Takes in a hand prints it horizontally 
for the players on the top and bottom of the screen
Pre-condition: Hand must be exactly 7 tiles*)
let printHor hand =  
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



(*Takes in a hand and prints it vertically 
for the players on the top and bottom of the screen
Pre-condition: Hand must be exactly 7 tiles*)


 let printVert hand = 
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
 




 