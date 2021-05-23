open Bag
open Hand
open Board

let bag = Bag.init_bag
let hand1 = Array.make 7 {letter = 'A'; value = 10 }
let hand2 = Array.make 7 {letter = 'A'; value = 10 }
let hand3 = Array.make 7 {letter = 'A'; value = 10 }
let hand4 = Array.make 7 {letter = 'A'; value = 10 }

let board =board_init ()
let num_of_players = ref 0
let hands = [| hand1; hand2; hand3; hand4|]

let scores = [|0;0;0;0|]
let max_score = ref 0
let winner = ref 0 
let tiles_placed = ref 0

(*[exchanging] Exchanges tiles in the hand up to the specified number wanted. *)
let rec exchanging hand ind_num num_ex= 
  if (ind_num<num_ex) then begin
    print_endline("What tile would you like to exchange?"); 
    print_endline("If you have a blank tile that you would like to exchange,\n
    hit space and then enter");
    try let letter =  read_line() in 
    if String.length letter = 1 then 
      begin 
      let tile = tile_getter (String.get letter 0) hand 0 in
      return_tile tile bag;
      tile_replace tile hand bag;
      exchanging hand (ind_num+1) num_ex
      end else failwith ""
    with failure -> 
    print_endline("Please enter a tile in your hand!");
    exchanging hand ind_num num_ex
  end else 
    print_endline("All tiles exchanged! ")


(**[exchange_num] does the exchanging process as long as a valid number of tiles 
was entered. Otherwise, asks the user to re-input the number they wanted 
exchanged*)
let rec exchange_num s hand=
  try let num_ex = (int_of_string s) in 
    if (num_ex>0 && num_ex < 8) then begin
        exchanging hand 0 num_ex
    end else failwith ""
  with failure -> 
    print_endline("Please enter a valid number of tiles!");
    print_endline("Do you still want to exchange tiles? Please type yes/no");
    let ans = read_line(()) in 
    if ans = "yes" then 
      begin
      print_endline("How many tiles would you like to exchange?");
      exchange_num (read_line()) hand
      end else
    failwith "Wanted to change action"
(**Almost works, but need to make sure tiles don't get replaced that 
    were invalid words*)
let all_tiles_placed hand = 
  for i = 0 to Array.length hand do 
    if Array.get hand i = {letter = '*'; value = 0} then
      tiles_placed:=(!tiles_placed)+1
    done

(**[place] returns the score for the word placed if it is valid, otherwise 
  allows the user to quit and choose a different action.*)
let rec place hand = 
  print_endline("The Board is: ");
  print_board (board);
  let tiles_placed_ref = ref [] in 
  try 
    match play_a_word board hand tiles_placed_ref with 
    | None-> failwith ""
    | Some i ->  begin 
    if !tiles_placed=7 then 
      begin
      print_endline("You scored 50 points!"); 50
      end else 
      begin 
      print_endline("You scored " ^ string_of_int i ^ " points!");
      tile_replace {letter = '*'; value = 0} hand bag; i
      end   
    end
  with failure -> 
    revert_hand hand !tiles_placed_ref;
    reset_board board;
    print_endline("Do you still want to place a word? Please type yes or no");
    let ans = read_line() in 
    if ans = "yes" then begin
    print_endline ("Your current hand is:");
    print_hor hand;
    place hand 
    end else 
    failwith "Wanted to change action"

(**[player_act] allows the user to choose one of three valid actions.*)
let rec player_act player_number hand= 
  print_endline("Choose an action player " ^ (string_of_int player_number) 
  ^ "! You can pass, exchange tiles, or place tiles on board!");
  print_endline("Your current hand is:");
  print_hor hand;
  let s = read_line() in try
  if s = "exchange" then 
    begin
    print_endline("How many tiles would you like to exchange?");
    exchange_num (read_line()) hand;
    print_endline("Your new hand is:");
    print_hor hand;
    end else if s = "pass" then 
    print_endline("Skipping turn!")
  else if s = "place" then begin
      let score = place hand in 
      Array.set scores (player_number-1) ((Array.get scores (player_number-1))+ 
      score);
      print_endline("Word Placed!");
      print_endline("Your new hand is:");
      print_hor hand;
      tiles_placed:= 0
  end else failwith ""
  with failure -> if failure =
    Failure "" then begin 
    print_endline("Please enter a valid action");
    player_act player_number hand
    end else begin
    print_endline("Allowing new action to be chosen!");
    player_act player_number hand
    end

(**[empty_hands_check] returns true if a hand is empty*)
let rec empty_hands_check hands ind =
  if ind<(!num_of_players) then
    if Array.length (Array.get hands ind)= 0 then true
    else empty_hands_check hands (ind+1)
  else 
    false
(**[print_scores] prints out the current score for each player*)
let print_scores () = 
  for i = 0 to !num_of_players - 1 do
    print_endline ("Player " ^ (string_of_int (i+1 ))^":");
    print_endline (string_of_int (Array.get scores i))
  done

(**[get_max_score] assigns the max score to that which player has the highest 
score*)
let get_max_score () = 
  for i = 0 to !num_of_players - 1 do
    let player_score = Array.get scores i in 
    if player_score>(!max_score) then 
      begin
      max_score:= player_score;
      winner:= (i+1)
      end
  done

(**[end_game] prints out the final scores and the winners and then ends the 
game*)
let end_game ()=
  print_endline("The Final Scores are: ");
  print_scores ();
  get_max_score ();
  if (!max_score)= 0 then 
    begin
    print_endline ("It's a tie!");
    end else 
    begin
    print_endline("The high score was: " ^ (string_of_int !max_score));
    print_endline("The winner is: Player " ^ (string_of_int !winner) ^"!");
    end;
  print_endline("Good game!");
  Stdlib.exit 0

(*[turn] allows the user to have turns, with either the option to quit or 
  causing there to be player actions for each player otherwise.*)
let rec turn turn_num play_num hands= 
  let count = total_count bag in
  print_endline ("The current number of tiles left in the bag is: ");
  print_endline (count |> string_of_int);
  if (count = 0) then if empty_hands_check hands 0= true then 
    begin
    print_endline("Game is over!");
    end_game ()
    end else ()
  else
    begin
    print_endline("This is Turn " ^string_of_int turn_num);
    print_endline("The Scores are: ");
    print_scores ();
    end;
  print_endline("Keep playing? Please type yes/no");
  let ans = read_line() in 
  if String.equal ans "yes" then begin
    for i = 1 to play_num do 
    player_act i (Array.get hands (i-1))
   done;
  turn (turn_num+1) play_num hands
  end else
    end_game ()
  
(*[set_hands] sets the initial hands of the players*)
let set_hands ()= 
  for i = 0 to !num_of_players-1 do
     create_starting_hand (Array.get hands i) bag
  done

(*[print_hands] prints out the hands of the players*)
let print_hands () = 
  for i = 0 to !num_of_players - 1 do
    print_hor (Array.get hands i)
  done



(*[player_gen] starts the game is the number of players is valid, 
  otherwise raises an unspecified error not seen by the user and 
  prompts them to re=enter a valid number of players*)
let rec player_gen (s) =
  try let num = (int_of_string s) in 
    if (num=2 || num=3 || num=4) then 
      begin
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
    end else failwith "" 
  with failure -> 
    print_endline("Please enter a valid number of players!");
    player_gen (read_line())


(*[main] starts up game*)
let main () =
  print_endline("Welcome to Scrabble!");
  print_endline("The rules of scrabble are: after the first turn, the player 
  must place tiles that allows all vertical and horizontal tiles to be valid 
  words. There are no diagonal words allowed. The other actions a player can 
  take is exchanging tiles or passing their turn. The game ends when the bag 
  of tiles is empty, and a player has placed all their tiles");
  print_endline("The board is a 15 by 15 board. To play a word, the player will
  be asked to input both the number of tiles they want to place down, and the 
  start and end position of their word. They will then place tile by tile. 
  The proper way to input is to type in the row, and then space, and then the 
  column. The rows and columns range from 0 to 14. So, for example, if you 
  want to place a tile in the middle of the board, you will type 7 7");
  print_endline("How many players do you want? Enter either 2,3, or 4");
  player_gen (read_line())
 

let () = main ()