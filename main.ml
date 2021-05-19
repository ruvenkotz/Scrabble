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

let scores = [|0;0;0;0|]
let max_score = ref 0
let winner = ref 0 
let tiles_placed = ref 0

(*Exchanges tiles in the hand up to the specified number wanted.
TODO: exchange blank tiles and don't let them exchange a tile that they received
this turn*)
let rec exchanging hand ind_num num_ex= 
    if (ind_num<num_ex) then begin
    print_endline("What tile would you like to exchange?"); 
    print_endline("If you have a blank tile that you would like to ezchange, 
    hit space and then enter");
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
  print_endline("Do you still want to exchange tiles? Please type yes/no");
  let ans = read_line(()) in 
  if ans = "yes" then 
    begin
    print_endline("How many tiles would you like to exchange?");
    exchange_num (read_line()) hand
    end
  else
  failwith "Wanted to change action"
(**Almost works, but need to make sure tiles don't get replaced that were invalid
words*)
let all_tiles_placed hand = 
  for i = 0 to Array.length hand do 
    if Array.get hand i = {letter = '*'; value = 0} then
      tiles_placed:=!tiles_placed+1
    done

let rec place hand = 
  try  
      match play_a_word board hand with 
      | None-> failwith ""
      | Some i ->  all_tiles_placed hand;
      tile_replace {letter = '*'; value = 0} hand bag;
      if !tiles_placed=7 then 50
      else i
with failure -> 
  print_endline("Do you still want to place a word? Please type yes or no");
  let ans = read_line() in 
  if ans = "yes" then 
  place hand
else 
  failwith "Wanted to change action"

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
  else if s = "place" then begin
      Array.set scores (player_number-1) ((Array.get scores player_number-1)+ 
      place hand);
      print_endline("Word Placed!");
      tiles_placed:= 0
  end
  else failwith ""
with failure -> if failure =
  Failure "" then 
    begin 
  print_endline("Please enter a valid action");
  player_act player_number hand
    end 
  else 
    begin
    print_endline("Allowing new action to be chosen!");
    player_act player_number hand
  end


let rec empty_hands_check hands ind =
  if ind<(!num_of_players) then
    if Array.length (Array.get hands ind)= 0 then true
    else empty_hands_check hands (ind+1)
  else 
    false
let print_scores () = 
  for i = 0 to !num_of_players - 1 do
    print_endline ("Player " ^ (string_of_int (i+1 ))^":");
    print_endline (string_of_int (Array.get scores i))
  done

let get_max_score () = 
  for i = 0 to !num_of_players - 1 do
  let player_score = Array.get scores i in 
    if player_score>(!max_score) then 
      begin
      max_score:= player_score;
      winner:= (i+1)
      end
done

let end_game ()=
print_endline("The Final Scores are: ");
print_scores ();
get_max_score ();
if (!max_score)= 0 then 
  begin
  print_endline ("It's a tie!");
  end
else 
  begin
  print_endline("The high score was: " ^ (string_of_int !max_score));
  print_endline("The winner is: Player " ^ (string_of_int !winner) ^"!");
  end;
print_endline("Good game!");
Stdlib.exit 0

(*Allows the user to have turns, with either the option to quit or causing 
there to be player actions for each player otherwise.*)
let rec turn turn_num play_num hands= 
let count = total_count bag in
print_endline ("The current number of tiles left in the bag is: ");
print_endline (count |> string_of_int);
if (count = 0) then if empty_hands_check hands 0= true then 
  begin
  print_endline("Game is over!");
  end_game ()
  end
else ()
else
  begin
print_endline("This is Turn " ^string_of_int turn_num);
print_endline("The Scores are: ");
print_scores ();
  end;
print_endline("Keep playing? Please type yes/no");
let ans = read_line() in 
if String.equal ans "no" then begin
  end_game ()
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
      set_hands ();
      print_endline("The Hands are:");    
      print_hands ();
      print_endline("The Board is: ");
      print_board (board);
      print_endline("The scores are: ");
      print_scores ();
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

