(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



val output : per_file:bool -> (string, int array) Hashtbl.t -> unit
(** Writes a text summary report to STDOUT. *)
