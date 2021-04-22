open Bag
open Hand
open Board

let hand1 = Array.make 7 {letter = 'A'; value = 10 }
let hand2 = Array.make 7 {letter = 'A'; value = 10 }
let hand3 = Array.make 7 {letter = 'A'; value = 10 }
let hand4 = Array.make 7 {letter = 'A'; value = 10 }

let num_of_players = ref 0
let hands = [| hand1; hand2; hand3; hand4|]


let rec player_act player_number = 
  print_endline("Choose an action player " ^ (string_of_int player_number) 
  ^ "! You can pass, exchange tiles, or place tiles on board!");
  let s = read_line() in 
  try
  if s = "exchange" then 
    print_endline("Skipping turn!")
  else if s = "pass" then 
    print_endline("Skipping turn!")
  else if s = "place" then
    print_endline("Skipping turn!")
  else failwith ""
with failure -> 
  print_endline("Please enter a valid action");
  player_act player_number

let rec turn turn_num play_num= 
print_endline("This is Turn " ^string_of_int turn_num);
print_endline("Keep playing? Please type yes/no");
let ans = read_line() in 
if String.equal ans "no" then begin
  print_endline("Thank you for playing!");
  Stdlib.exit 0
end
else
  for i = 1 to play_num do 
    player_act i
  done;
  turn (turn_num+1) play_num


let set_hands ()= 
  for i = 0 to !num_of_players do
     create_starting_hand (Array.get hands i)
done

let print_hands () = 
  for i = 0 to !num_of_players - 1 do
    print_hor (Array.get hands i)
done

let rec player_gen (s) =
  try let num = (int_of_string s) in 
    if (num=2 || num=3 || num=4) then begin
      num_of_players := num;
      print_endline("The Hands are:");
      set_hands ();
      print_hands ();
      print_endline("The Board is: ");
      print_board board_init;
      print_endline("Let the game begin!");
      turn 1 num;
    end
    else failwith "" 
  with failure -> 
    print_endline("Please enter a valid number of players!");
    player_gen (read_line())


let main () =
  print_endline("Welcome to Scrabble!");
  print_endline("How many players do you want? Enter either 2,3, or 4");
  player_gen (read_line())
 

let () = main ()

