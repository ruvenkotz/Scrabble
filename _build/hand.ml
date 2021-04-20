open Bag
open Board

exception LetterNotFound

let bag = Bag.init_bag

let board = Board.board_init

type t = Bag.tile list list


let rec generate_hand_helper hand =  if List.length hand < 7 then let tile = snd (next_tile bag) in
generate_hand_helper ( {letter = tile.letter; value = tile.value } :: hand) else hand 

let rec init_hand = generate_hand_helper []

let hands : t = []

let rec add_a_hand : t = (generate_hand_helper []) :: hands



let print_hor hand =  
 (print_endline"+---------------------------+";
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
  print_endline"+---------------------------+")

 let print_vert hand = 
  (print_endline"+-----+";
  print_endline("| " ^ (Char.escaped (List.nth hand 0).letter)  ^ " " ^ (string_of_int (List.nth hand 0).value) ^ " |");
  print_endline("| " ^ (Char.escaped (List.nth hand 1).letter)  ^ " " ^ (string_of_int (List.nth hand 1).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (List.nth hand 2).letter)  ^ " " ^ (string_of_int (List.nth hand 2).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (List.nth hand 3).letter)  ^ " " ^ (string_of_int (List.nth hand 3).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (List.nth hand 4).letter)  ^ " " ^ (string_of_int (List.nth hand 4).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (List.nth hand 5).letter)  ^ " " ^ (string_of_int (List.nth hand 5).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (List.nth hand 6).letter)  ^ " " ^ (string_of_int (List.nth hand 6).value)  ^ " |");
  print_endline ("+-----+"); )
 

(*Removes a tile from a hand*)
let rec remove_tile letter front = function
|[] -> failwith "Tried to remove a tile that doesn't exist"
|h::t -> if h.letter = letter then front @ t else remove_tile letter (h::front) t

(*Checks to see that the letter chosen is in the player's hand. 
  Throws [LetterNotFound] if not*)
let rec check_letter letter = function
|[] -> raise(LetterNotFound)
|h :: t -> if h.letter = letter then () else check_letter letter t

(*Parses the position and places the letter on the board*)
let place_letter board letter pos = 
  let row_col = String.split_on_char ' ' pos  in
  let row = List.nth row_col 0 |> int_of_string in
  let col = List.nth row_col 1 |> int_of_string in
  set_char board row col letter 

  
let play_a_word board hand bag = 
  print_endline("Choose a tile a play: ");
  let letter = read_line() in check_letter (String.get letter 0) hand;
  print_endline("Choose a row and to place your tiles ");
  let pos = read_line() in place_letter board (String.get letter 0) pos;

  


