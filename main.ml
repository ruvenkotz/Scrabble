open Bag
open Hand
open Board

let rec player_gen (s) =
  try let num = (int_of_string s) in
    if (num=2 || num=3 || num=4) then 
      for i=0 to num-1 do 
        print_hor (generate_hand [])
      done
    else failwith ""
  with failure -> 
    print_endline("Please enter a valid number of players!");
    player_gen (read_line())

let main () =
  print_endline("Welcome to Scrabble!");
  print_endline("How many players do you want? Enter either 2,3, or 4");
  player_gen (read_line());
  print_endline("The Hands are:");
  print_endline("The Board is: ");
  print_board board_init

let () = main ()

