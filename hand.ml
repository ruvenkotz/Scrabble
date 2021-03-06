open Bag
open Board
open Array
exception LetterNotFound
exception WordDoesNotFit
exception InvalidPositioning

type t = Bag.tile array 

(*Dummy hand used for testing purposes*)
let hand1 = Array.make 7 {letter = 'A'; value = 1 }

let bag = init_bag

(*Dummy board used for testing purposes*)
let board : Board.t = board_init ()

let create_starting_hand hand bag= 
    for i = 0 to 6 do
    let tile = snd (next_tile bag) in
    set hand i {letter = tile.letter; value = tile.value } 
  done

let rec print_helper hand =
  match hand with
  | [] -> []
  | h :: t -> 
    if h.value == 10 
      then string_of_int h.value :: print_helper t
      else ("0" ^ string_of_int h.value)  :: print_helper t

let print_hor hand = 
 
 (print_endline"+----------------------------------+";
  print_endline (
  "| "  ^ (Char.escaped (Array.get hand 0).letter)  ^
  "  | " ^ (Char.escaped (Array.get hand 1).letter)  ^
  "  | " ^ (Char.escaped (Array.get hand 2).letter)  ^
  "  | " ^ (Char.escaped (Array.get hand 3).letter)  ^
  "  | " ^ (Char.escaped (Array.get hand 4).letter)  ^
  "  | " ^ (Char.escaped (Array.get hand 5).letter)  ^
  "  | " ^ (Char.escaped (Array.get hand 6).letter)  ^ 
  "  |");
  let l = print_helper (to_list hand) in 
  print_endline (
  "| "  ^ (List.nth l 0)  ^
  " | " ^ (List.nth l 1)  ^
  " | " ^ (List.nth l 2)  ^
  " | " ^ (List.nth l 3)  ^
  " | " ^ (List.nth l 4)  ^
  " | " ^ (List.nth l 5)  ^
  " | " ^ (List.nth l 6)  ^ 
  " |");
  print_endline"+----------------------------------+")

 let print_vert hand = 
  (print_endline"+-----+";
  print_endline("| " ^ (Char.escaped (Array.get hand 0).letter) 
   ^ " " ^ (string_of_int (Array.get hand 0).value) ^ " |");
  print_endline("| " ^ (Char.escaped (Array.get hand 1).letter) 
   ^ " " ^ (string_of_int (Array.get hand 1).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (Array.get hand 2).letter) 
   ^ " " ^ (string_of_int (Array.get hand 2).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (Array.get hand 3).letter)  
  ^ " " ^ (string_of_int (Array.get hand 3).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (Array.get hand 4).letter) 
   ^ " " ^ (string_of_int (Array.get hand 4).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (Array.get hand 5).letter) 
   ^ " " ^ (string_of_int (Array.get hand 5).value)  ^ " |");
  print_endline("| " ^ (Char.escaped (Array.get hand 6).letter) 
   ^ " " ^ (string_of_int (Array.get hand 6).value)  ^ " |");
  print_endline ("+-----+"); )
 

(*[index] returns the index of a tile given a character. Raise [LetterNotFound] 
  if it does not exist.*)
 let rec index letter i = function
 |[] -> print_endline("Error: tile is not in your hand"); raise(LetterNotFound)
 |h :: t -> if h.letter = letter then i else index letter (i+1) t

let rec tile_getter letter hand ind =  
  if ind<7 then
    begin
  if (Array.get hand ind).letter = letter then Array.get hand ind
  else tile_getter letter hand (ind+1)
end
else 
  raise(LetterNotFound)

(*[check_hand] checks to see that the letter chosen is in the player's hand.*)
let rec check_hand letter hand =  match hand with
  |[] -> print_endline("Error: tile is not in your hand"); 
  raise(LetterNotFound)
  |h :: t -> if h.letter = letter || h.letter = ' ' then true 
  else check_hand letter t
  
(*[split_word] splits a word into a list of its characters*)
let rec split_word l word = match word with
|"" -> List.rev l
|_ -> split_word ((String.get word 0) :: l) 
(String.sub word 1 (String.length word - 1))

(*[new_tiles] picks new tiles for those removed*)
let new_tiles (hand : t) bag =  
  for i = 0 to (length hand -1) do
    if Char.equal (get hand 0).letter '*' then
    let tile = snd (next_tile bag) in  
    set hand 0 tile
  done

(**[find_first_tiles] finds the first occurence of a tile in a hand. 
Returns the index*)
let rec find_first_tile tile hand acc= match hand with 
  |[]-> print_endline("Error: tile is not in your hand"); raise (TileNotFound)
  |h::t -> if tile = h.letter then acc else find_first_tile tile t (acc+1)
      
let tile_replace tile hand bag= 
  if tile = {letter = '*'; value = 0} then 
    for i = 0 to (length hand -1) do
      if Char.equal (get hand i).letter '*' then
      let tile = snd (next_tile bag) in  
      set hand i tile
    done
  else  
  let ind = find_first_tile tile.letter (to_list hand) 0 in 
  let new_tile = snd (next_tile bag) in
  set hand ind {letter = new_tile.letter; value = new_tile.value }


(*[place_a letter] plays the letter from the hand onto the given position on the
board.*)
  let place_a_letter board l pos hand = 
    let letter = String.get l 0 in
    let start_row_col = String.split_on_char ' ' pos  in
    let row = List.nth start_row_col 0 |> int_of_string in
    let col = List.nth start_row_col 1 |> int_of_string in
    if check_hand letter hand then 
         Board.set_char board row col letter



(*[set_blank_tile] set the blank tile to the tile of the player's choice if they
  decide to play a blank tile*)
let set_blank_tile l =
match l with
| " " -> (print_endline("What letter would you like your blank tile to be?");
          let letter =  read_line() in letter)
| _ -> l

(*[print_tile_lst] prints all the letter in a tile list*)
(*
let rec print_tile_lst tile_lst = 
  match tile_lst with 
  | []-> ()
  | h::t -> print_endline(Char.escaped h.letter);
  print_tile_lst t
  *)

let play_a_word board h tiles_lst = 
  let hand = to_list h in
  print_endline("How many tiles do you want to play ");
  let num_tiles = read_line() in ();
  print_endline("Enter the starting position of your word ");
  let start_pos = read_line() in
  let start_row_col = String.split_on_char ' ' start_pos in
  let start_row = List.nth start_row_col 0 |> int_of_string in
  let start_col = List.nth start_row_col 1 |> int_of_string in
  print_endline("Enter the ending position of your word ");
  let end_pos = read_line() in
  let end_row_col = String.split_on_char ' ' end_pos in
  let end_row = List.nth end_row_col 0 |> int_of_string in
  let end_col = List.nth end_row_col 1 |> int_of_string in
  (* let tiles_played = Array.make (int_of_string num_tiles) 'A' in *)
  for i = 0 to (int_of_string num_tiles) - 1 do
    print_endline("Choose a letter to play: ");
    print_endline("If you want to play a blank tile hit the space bar");
    let letter = read_line() in ();
    print_endline("Choose a position to place your tile: ");
    let pos = read_line() in ();
    let letter_con = set_blank_tile letter in
    print_endline("Letter: " ^ letter_con);
    place_a_letter board letter_con pos (to_list h);
    tiles_lst:=(List.nth hand (index (String.get letter 0) 0 hand))::!tiles_lst;
    print_endline("Letter " ^ letter_con);
    print_board board;
    set h (find_first_tile (String.get letter 0) (to_list h) 0 )
    {letter = '*'; value = 0};
    print_endline("Your current hand is:");
    print_hor (h);
  done;
  check_word board start_row start_col end_row end_col

 
let rec revert_hand hand tile_lst = 
  let hand_list = to_list hand in
  match tile_lst with 
  [] -> ()
  | h::t -> set hand (find_first_tile '*' hand_list 0) h; 
  revert_hand hand t
