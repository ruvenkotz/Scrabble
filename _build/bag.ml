(*open Yojson.Basic.Util
(*Plan: create bag of 102 tiles with specific amount of each tile. Use json and 
hand module in order to get tiles. *)

(*bag.json gets the amount of times a specific tile should be put in bag.*)
(*Bag should be a tile list, which consists of a value and character*)
let j = Yojson.Basic.from_file "bag.json"

type tile = {
  letter : char;
  value : int;
}

type tile_count = 
{ tile: tile;
count: int}

type t = 
{bag: tile_count list}

let bag_of_json j =
  {
    tile ={letter = j |> member "char" |> to_string;
    value = j |> member "val" |> to_int};
    count = j |> member "count" |> to_int;
    
  }

let init_bag j = 
  bag_of_json j 

*)