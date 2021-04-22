open Bag
open Board
open Array
exception LetterNotFound

let bag = Bag.init_bag

type t = Bag.tile array array

let create_starting_hand hand = 
    for i = 0 to 6 do
    let tile = snd (next_tile bag) in
    set hand i {letter = tile.letter; value = tile.value } 
  done


let print_hor hand =  
 (print_endline"+---------------------------+";
  print_endline (
  "| "  ^ (Char.escaped (Array.get hand 0).letter)  ^
  " | " ^ (Char.escaped (Array.get hand 1).letter)  ^
  " | " ^ (Char.escaped (Array.get hand 2).letter)  ^
  " | " ^ (Char.escaped (Array.get hand 3).letter)  ^
  " | " ^ (Char.escaped (Array.get hand 4).letter)  ^
  " | " ^ (Char.escaped (Array.get hand 5).letter)  ^
  " | " ^ (Char.escaped (Array.get hand 6).letter)  ^ 
  " |");
  print_endline (
  "| "  ^ (string_of_int (Array.get hand 0).value)  ^
  " | " ^ (string_of_int (Array.get hand 1).value)  ^
  " | " ^ (string_of_int (Array.get hand 2).value)  ^
  " | " ^ (string_of_int (Array.get hand 3).value)  ^
  " | " ^ (string_of_int (Array.get hand 4).value)  ^
  " | " ^ (string_of_int (Array.get hand 5).value)  ^
  " | " ^ (string_of_int (Array.get hand 6).value)  ^ 
  " |");
  print_endline"+---------------------------+")

 let print_vert hand = 
  (print_endline"+-----+";
  print_endline("| " ^ (Char.escaped (Array.get hand 0).letter)  ^ " " ^ (string_of_int (Array.get hand 0).value) ^ " |");
  print_endline("| " ^ (Char.escaped (Array.get hand 1).letter)  ^ " " ^ (string_of_int (Array.get hand 1).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (Array.get hand 2).letter)  ^ " " ^ (string_of_int (Array.get hand 2).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (Array.get hand 3).letter)  ^ " " ^ (string_of_int (Array.get hand 3).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (Array.get hand 4).letter)  ^ " " ^ (string_of_int (Array.get hand 4).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (Array.get hand 5).letter)  ^ " " ^ (string_of_int (Array.get hand 5).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (Array.get hand 6).letter)  ^ " " ^ (string_of_int (Array.get hand 6).value)  ^ " |");
  print_endline ("+-----+"); )
 

(*Removes a tile from a hand*)

let rec find_tile letter hand index =
  match (to_list hand) with
|[] -> failwith "Tried to remove a tile that doesn't exist"
|h::t -> if h.letter = letter then index else find_tile letter hand index+1



(* let rec remove_tile letter front = function
|[] -> failwith "Tried to remove a tile that doesn't exist"
|h::t -> if h.letter = letter then append front t else remove_tile letter (h::front) t *)

(*Checks to see that the letter chosen is in the player's hand. 
  Throws [LetterNotFound] if not*)
let rec check_letter letter hand = match hand with
|[] -> raise(LetterNotFound)
|h :: t -> if h.letter = letter then () else check_letter letter t


let rec split_word l word = match word with
|"" -> List.rev l
|_ -> split_word ((String.get word 0) :: l) (String.sub word 1 (String.length word - 1))

(* let pos_list start_row end_row start_col end_col = 
 
  if start_row - end_row <> 0 then 
    if start_row < end_row then
      let l1 = Array.make (end_row - start_row + 1) (0,0) in
      for i = start_row to end_row do
        Array.set 
 *)


(*Parses the position and places the letter on the board*)
let rec place_word board word start_pos end_pos hand = 
  let start_row_col = String.split_on_char ' ' start_pos  in
  let start_row = List.nth start_row_col 0 |> int_of_string in
  let start_col = List.nth start_row_col 1 |> int_of_string in
  let end_row_col = String.split_on_char ' ' end_pos  in
  let end_row = List.nth end_row_col 0 |> int_of_string in
  let end_col = List.nth end_row_col 1 |> int_of_string in 
  let char_list = split_word [] word in 
  if start_row < end_row  
    then 
      for i = 0 to (end_row - start_row) do
        print_board (Board.set_char board (i + start_row) start_col (List.nth char_list i))
      done
    else if start_col < end_col
      then 
        for i = 0 to (end_col - start_col) do
          print_board (Board.set_char board start_row (start_col+i) (List.nth char_list i))
        done


let play_a_word board hand bag = 
  print_endline("Choose a position to start your word ");
  let start_pos = read_line() in ();
  print_endline("Choose a position to end your word ");
  let end_pos = read_line() in ();
  print_endline("Write your word: ");
  let word = read_line() in place_word board word start_pos end_pos hand;

  


