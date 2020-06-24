
type cell =
  | Dark
  | Empty
  | Filled of char

type direction = Across | Down  (* TODO: use these instead of bools. *)

module CoordMap =
  Map.Make (struct
      type t = int * int
      let compare = compare
    end)

module CoordSet =
  Set.Make (struct
      type t = int * int
      let compare = compare
    end)

(** Explode a string into characters *)
let char_array_of_word s =
  Array.init (String.length s) (String.get s)

(** Fold over a set of 0-based 2D coordinates. *)
let fold_coords nrows ncols f init =
  let xrange = List.init nrows Fun.id in
  let yrange = List.init ncols Fun.id in
  List.fold_left (fun rows x ->
      List.fold_left (fun thisrow y ->
          f thisrow x y
        ) rows yrange
    ) init xrange


module CrosswordGrid = struct

  type t = {
      cells: cell CoordMap.t;
      nrows: int;
      ncols: int
    }

  (** Create an empty grid from a size and list of blacked-out cells. *)
  let create ~nrows ~ncols ~darks =
    (* Need fold_right because of the add function's signature *)
    (* let dark_set = List.fold_right CoordSet.add darks CoordSet.empty in *)
    (* Since 4.02.."usually more efficient than folding add over the list" *)
    let dark_set = CoordSet.of_list darks in
    let cells = fold_coords nrows ncols (fun map i j ->
                    if CoordSet.mem (i, j) dark_set then
                      CoordMap.add (i, j) Dark map
                    else
                      CoordMap.add (i, j) Empty map
                  ) CoordMap.empty
    in 
    { cells; nrows; ncols }

  (** Produce an ASCII picture of the grid. *)
  let to_string { cells; nrows; ncols } =
    fold_coords nrows ncols (fun st i j ->
        let cellstr = match CoordMap.find (i, j) cells with
          | Dark -> "# "
          | Empty -> "_ "
          | Filled c -> (String.make 1 c) ^ " "
        in
        st ^ cellstr ^ (if j = ncols-1 then "\n" else "")
      ) ""

  (** Return the horizontal word that a cell is in, plus its origin *)
  let get_across grid (row, col) =
    let rec scan_left j =  (* Return leftmost index of word *)
      if j < 0 then j+1
      else match CoordMap.find (row, j) grid.cells with
           | Dark -> j+1
           | _ -> scan_left (j-1) in
    let rec scan_right j =  (* Return right upper bound *)
      if j = grid.ncols then j
      else match CoordMap.find (row, j) grid.cells with
           | Dark -> j
           | _ -> scan_right (j+1) in
    let left_index = scan_left col in 
    let size = scan_right col - left_index in
    let the_array = 
      if size > 0 then 
        Array.init size (fun i ->
            CoordMap.find(row, left_index + i) grid.cells)
      else [||]
    in
    (the_array, (row, left_index))  
  
  (** Return the vertical word that a cell is in, plus its origin *)
  let get_down grid (row, col) = 
    let rec scan_up i =  (* Return top index of word *)
      if i < 0 then i+1
      else match CoordMap.find (i, col) grid.cells with
           | Dark -> i+1
           | _ -> scan_up (i-1) in
    let rec scan_down i =  (* Return bottom upper bound *)
      if i = grid.nrows then i
      else match CoordMap.find (i, col) grid.cells with
           | Dark -> i
           | _ -> scan_down (i+1) in
    let top_index = scan_up row in 
    let size = scan_down row - top_index in
    let the_array = 
      if size > 0 then 
        Array.init size (fun i ->
            CoordMap.find(top_index+i, col) grid.cells)
      else [||]
    in
    (the_array, (top_index, col))

  (** get a row in either direction. TODO: make this the only one. *)
  let get_row grid pos direction =
    if direction then get_across grid pos
    else get_down grid pos

  (** Check if a specific cell is empty *)
  let is_empty grid (row, col) = CoordMap.find (row, col) grid = Empty
  
  (* This should be private; doesn't check *)
  let place_word grid (row, col) across word_arr =
    let newcells = 
      List.fold_left (fun map ix ->
          let i, j = if across then (row, col+ix) else (row+ix, col) in
          CoordMap.add (i, j) (Filled word_arr.(ix)) map)
        grid.cells (List.init (Array.length word_arr) Fun.id)
    in
    { cells=newcells; nrows=grid.nrows; ncols = grid.ncols }
  
  (** Check if a word can be legally added; do it if possible. *)
  let try_add_word grid (i, j) across word =
    (* Take any position in the word; 'pos' will be the start. *)
    let grid_arr, pos = if across then get_across grid (i, j)
                        else get_down grid (i, j) in
    let word_arr = char_array_of_word word in
    let rec match_from i =
      if i = Array.length grid_arr then true
      else
        let match_i = match grid_arr.(i) with
          | Empty -> true
          | Filled c -> c = word_arr.(i)
          | Dark -> false in
        match_i && match_from (i+1)  
    in
    if Array.length grid_arr <> Array.length word_arr then None
    else if match_from 0 then
      Some (place_word grid pos across word_arr)
    else None

  (** Add adjacent blank nodes to a "frontier" set *)
  let rec add_frontier_blanks grid frontier pos direction =
    (* a row is a sequence of cells...no coordinates... *)
    (* maybe I need another type for row information *)
    let row, pos = get_row grid pos direction in 
    let row_coords = List.init (Array.length row) (fun i ->
                         if direction then (fst pos, snd pos + i)
                         else (fst pos + i, snd pos)) in
    (* oh, stacks are modified in place. okay. *)
    List.iter (fun (i, j) ->
        if direction then (   (* across, look up and down *)
          if (i-1) >= 0 && CoordMap.find (i-1, j) grid.cells = Empty
          then Stack.push ((i-1, j), false) frontier else ();
          if (i+1) < grid.nrows && CoordMap.find (i+1, j) grid.cells = Empty
          then Stack.push ((i+1, j), false) frontier else () )
        else (  (* down, look left and right *)
          if (j-1) >= 0 && CoordMap.find (i, j-1) grid.cells = Empty
          then Stack.push ((i, j-1), true) frontier else ();
          if (j+1) < grid.ncols && CoordMap.find (i, j+1) grid.cells = Empty
          then Stack.push ((i, j+1), true) frontier else () )
      ) row_coords

end (* CrosswordGrid *)


(* Filter won't work, wordlist might have repeats and we want to remove
 * only one *)
let rec list_remove_one l item =
  match l with
  | [] -> []
  | x::xs -> if x = item then xs
             else x::(list_remove_one xs item)


let rec print_string_list = function
  | [] -> print_string "\n"
  | w::ws -> (
    print_string (w ^ " ");
    print_string_list ws)

(* keep a running set of adjacent (blank, directions)?
 * If some of them get un-blank or are repeats, it's ok, just keep trying *)

let rec pop_until_opt f st =
  match Stack.pop_opt st with
  | Some item -> if f item then Some item else pop_until_opt f st
  | None -> None

let solve grid words =
  let frontier = Stack.create () in
  let rec solve_from (grid: CrosswordGrid.t) words = 
    (* If no blanks left, we won *)
    match pop_until_opt (fun (pos, dir) ->
              CrosswordGrid.is_empty grid.cells pos) frontier
    with
    | None ->
      print_endline "Done! (I hope, is it?)";
      Some grid
    | Some (pos, dir) -> (
      let this_row, pos = CrosswordGrid.get_row grid pos dir in
      print_endline (
          "Now looking to fill around (" ^ string_of_int (fst pos)
          ^ ", " ^ string_of_int (snd pos)
          ^ (if dir then ") across" else ") down"));
      let wordcands = List.filter (fun w ->
                          String.length w = Array.length this_row)
                        words in
      let rec try_candidates wcands =
        (* print_string_list wcands; *)
        match wcands with
        | [] ->
           print_endline "out of candidates, will backtrack...";
           (* print_string (CrosswordGrid.to_string grid); *)
           None (* just a fail of this iteration *)
        | word::rest -> (
          match CrosswordGrid.try_add_word grid pos dir word with
          | Some newgrid -> (
            (* success, find a match and recurse main *)
            CrosswordGrid.add_frontier_blanks newgrid frontier pos dir;
            print_endline ("added word " ^ word);
            print_string (CrosswordGrid.to_string newgrid);
            (* Here's the magic nested recursion. Try to solve the whole thing;
             * if not, keep going with the next candidate. *)
            match solve_from newgrid (list_remove_one words word) with
            | Some grid -> Some grid (* yay. Pop all the way up? *)
            | None -> try_candidates rest )
          | None -> try_candidates rest)
      in
      try_candidates wordcands )
  in
  Stack.push ((0, 0), true) frontier;
  solve_from grid words

(* More difficult than I expected; what if we add one that doesn't match 
 * in another direction? How will it ever find it? *)

let read_words filename =
  let inchan = open_in filename in
  let rec lines acc =
    match (
      try Some (String.trim (input_line inchan))
      with End_of_file -> None )
    with
    | None -> acc
    | Some w -> lines (w::acc)
  in
  List.rev (lines [])

let grid1 = CrosswordGrid.create 13 13 [
                (0, 5); (0, 9);
                (1, 5); (1, 9);
                (2, 9);
                (3, 0); (3, 1); (3, 2); (3,7);
                (4, 3); (4, 8);
                (5, 5); (5, 11); (5, 12);
                (6, 6);
                (7, 0); (7, 1); (7, 7);
                (8, 4); (8, 9);
                (9, 5); (9, 10); (9, 11); (9, 12);
                (10, 3);
                (11, 3); (11, 7);
                (12, 3); (12, 7)
              ]

let words = read_words "puzz1words.txt"

let _ = print_string (CrosswordGrid.to_string grid1)
let _ = solve grid1 words

