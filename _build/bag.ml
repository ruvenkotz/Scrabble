open Yojson.Basic.Util

exception EmptyBag
exception TileNotFound
exception InvalidChar
(*
let j = Yojson.Basic.from_file "bag.json"

type tile = {
  letter : char;
  value : int;
}

type tile_count = 
{ ti: tile;
count: int}

type t = 
{bag: tile_count list;
total_tiles: int }


let tile_count_of_json j= 
{
  ti ={letter = (j |> member "char" |> to_string).[0];
  value = j |> member "val" |> to_int};
  count = j |> member "count" |> to_int;
}
let bag_of_json j =
  {
    bag = j |> member "bag" |> to_list |> List.map tile_count_of_json;
    total_tiles = 102
  }

let init_bag:t = bag_of_json j


let rec new_bag_helper (drawn:tile) (counts:tile_count list)= 
  match counts with 
  | [] -> raise (TileNotFound)
  | h::t -> if h.ti = drawn then let h1= {ti = drawn; count = h.count-1} in 
  if h1.count>0 then h1::t 
  else raise (TileNotFound)
else new_bag_helper drawn t

let next_tile b = 
  match b.bag with 
  | [] -> raise (EmptyBag)
  | h::t -> let tile_index = Random.int 26 in 
  let tile_drawn = (List.nth b.bag tile_index).ti in 
  print_endline ("Tile Drawn is: ");
  print_endline (Char.escaped tile_drawn.letter);
  print_endline ("Value is: ");
  print_endline (tile_drawn.value|> string_of_int);
  let new_bag = new_bag_helper tile_drawn b.bag in 
  let b1 = {
    bag= new_bag;
    total_tiles = b.total_tiles -1
  }
in (b1,tile_drawn)


let rec tile_value (b:t) (c:char) = 
  match b.bag with 
  | [] -> raise (InvalidChar)
  |h::t -> if h.ti.letter = c then h.ti.value 
  else let b1 = {bag=t; total_tiles = b.total_tiles} in 
  tile_value b1 c 

let rec tile_count (b:t) (c:char) = 
  match b.bag with 
  | [] -> raise (InvalidChar)
  |h::t -> if h.ti.letter = c then h.count
  else let b1 = {bag=t; total_tiles = b.total_tiles} in 
  tile_count b1 c 

let total_count (b:t) =
  b.total_tiles

*)


let j = Yojson.Basic.from_file "bag.json"

type tile = {
  letter : char;
  value : int;
}
type t = 
{mutable bag: tile array;
mutable total_tiles: int }


let tile_count_of_json j= 
{
  letter = (j |> member "char" |> to_string).[0];
  value = j |> member "val" |> to_int
}
let bag_of_json j =
  {
    bag = Array.of_list (j |> member "bag" |> to_list |> List.map
    tile_count_of_json);
    total_tiles = 100
  }

let init_bag:t = bag_of_json j


let next_tile b = 
  if Array.length (b.bag) = 0 then 
  raise (EmptyBag)
  else let tile_index = Random.int b.total_tiles in 
  let tile_drawn = Array.get b.bag tile_index in 
  print_endline ("Tile Drawn is: ");
  print_endline (Char.escaped tile_drawn.letter);
  print_endline ("Value is: ");
  print_endline (tile_drawn.value|> string_of_int);
  b.total_tiles <-  b.total_tiles -1;
  print_endline(b.total_tiles|>string_of_int);
  let new_bag = if tile_index = b.total_tiles then 
    Array.sub b.bag 0 b.total_tiles
  else 
    let arr1 = Array.sub b.bag 0 tile_index in 
    let arr2 = Array.sub b.bag (tile_index+1) (b.total_tiles-tile_index) in
    Array.append arr1 arr2
  in b.bag<- new_bag;
  (*let b1 = {
    bag= new_bag;
    total_tiles = b.total_tiles
  }*)
(b,tile_drawn)


let rec tile_value_helper (tiles: tile array) (c:char) =
  match tiles with 
  | [||] -> raise (InvalidChar)
  | _ -> let ti = (Array.get tiles 0) in if ti.letter = c then 
    ti.value else let rec_bag = Array.sub tiles 1 (Array.length tiles-1) in
  tile_value_helper rec_bag c

let tile_value (b:t) (c:char) = 
  tile_value_helper b.bag c 

let total_count (b:t) =
  b.total_tiles