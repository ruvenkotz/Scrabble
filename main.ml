open Bag
open Hand
open Board

let bag = Bag.init_bag
let hand1 = Array.make 7 {letter = 'A'; value = 10 }
let hand2 = Array.make 7 {letter = 'A'; value = 10 }
let hand3 = Array.make 7 {letter = 'A'; value = 10 }
let hand4 = Array.make 7 {letter = 'A'; value = 10 }

let board = (Array.make_matrix 15 15 Empty)
let num_of_players = ref 0
let hands = [| hand1; hand2; hand3; hand4|]

(*Exchanges tiles in the hand up to the specified number wanted.*)
let rec exchanging hand ind_num num_ex= 
    if (ind_num<num_ex) then begin
    print_endline("What tile would you like to exchange?"); 
    try let letter =  read_line() in 
    if String.length letter = 1 then 
    let tile = tile_getter (String.get letter 0) hand 0 in
    return_tile tile bag;
    tile_replace tile hand bag;
    exchanging hand (ind_num+1) num_ex
else failwith ""
with failure -> 
  print_endline("Please enter a tile in your hand!");
  exchanging hand ind_num num_ex
end
else 
  print_endline("All tiles exchanged! ")


(**If a valid number of tiles to exchange was entered, then goes into the 
exchanging process. Otherwise, asks the user to re-input the number they wanted 
exchanged*)
let rec exchange_num s hand= 
  try let num_ex = (int_of_string s) in 
    if (num_ex>0 && num_ex < 8) then begin
        exchanging hand 0 num_ex
    end
  else failwith ""
with failure -> 
  print_endline("Please enter a valid number of tiles!");
  exchange_num (read_line()) hand


(*Ruven: I'll change [board_init] once the board is made mutable *)
let rec player_act player_number hand= 
  print_endline("Choose an action player " ^ (string_of_int player_number) 
  ^ "! You can pass, exchange tiles, or place tiles on board!");
  print_endline("Your current hand is:");
  print_hor hand;
  let s = read_line() in 
  try
  if s = "exchange" then 
    begin
    print_endline("How many tiles would you like to exchange?");
    exchange_num (read_line()) hand;
    print_endline("Your new hand is:");
    print_hor hand;
    end
  else if s = "pass" then 
    print_endline("Skipping turn!")
  else if s = "place" then
    for i = 0 to 0 do
      play_a_word board hand;
      new_tiles hand bag;
    done 
  else failwith ""
with failure -> 
  print_endline("Please enter a valid action");
  player_act player_number hand


(*Allows the user to have turns, with either the option to quit or causing 
there to be player actions for each player otherwise.*)
let rec turn turn_num play_num hands= 
print_endline("This is Turn " ^string_of_int turn_num);
print_endline("Keep playing? Please type yes/no");
let ans = read_line() in 
if String.equal ans "no" then begin
  print_endline("Thank you for playing!");
  Stdlib.exit 0
end
else
  for i = 1 to play_num do 
    player_act i (Array.get hands (i-1))
  done;
  turn (turn_num+1) play_num hands


(*Sets the initial hands of the players*)
let set_hands ()= 
  for i = 0 to !num_of_players-1 do
     create_starting_hand (Array.get hands i) bag
done

(*Prints out the hands of the players*)
let print_hands () = 
  for i = 0 to !num_of_players - 1 do
    print_hor (Array.get hands i)
done


(*Starts the game is the number of players is valid, otherwise raises an 
unspecified error not seen by the user and prompts them to re=enter a valid 
number of players*)
let rec player_gen (s) =
  try let num = (int_of_string s) in 
    if (num=2 || num=3 || num=4) then begin
      num_of_players := num;
      print_endline("The Hands are:");
      set_hands ();
      print_hands ();
      print_endline("The Board is: ");
      print_board (board);
      print_endline("Let the game begin!");
      turn 1 num hands;
    end
    else failwith "" 
  with failure -> 
    print_endline("Please enter a valid number of players!");
    player_gen (read_line())


(*Starting up game*)
let main () =
  print_endline("Welcome to Scrabble!");
  print_endline("How many players do you want? Enter either 2,3, or 4");
  player_gen (read_line())
 

let () = main ()

