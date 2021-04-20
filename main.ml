open Bag
open Hand
open Board

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

let rec player_gen (s) =
  try let num = (int_of_string s) in
    if (num=2 || num=3 || num=4) then begin
      print_endline("The Hands are:");
      for i=0 to num-1 do 
        print_hor ((*Used to be init but doesn't seem to work anymore*)generate_hand_helper [])
      done;
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

