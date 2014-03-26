type color =      Blue
                | Red
                | Green
                | Cyan
                | Yellow
                | Magenta

type case =        White
                |  Empty
                |  Color of color

type mode =       Substract
                | Addition

type direction =  Up
                | Down
                | Left
                | Right

type result =   Success of color
              | Failed
              | Win

let is_empty = function
    | Empty -> true
    | _ -> false

let number_of_empty grid =
    let count_line line = Array.fold_left (fun acc case -> if is_empty case then acc + 1 else acc) 0 line in
    Array.fold_left (fun acc line -> count_line line + acc) 0 grid

let init_random_case case_type grid =
    Random.self_init ();
    let x = Random.int 3 in
    let y = Random.int 3 in
    match grid.(y).(x) with
        | Empty -> grid.(y).(x) <- case_type; true
        | _ -> false

let init_grid () =
    let grid = Array.create_matrix 4 4 Empty in
    init_random_case (Color(Blue)) grid;
    init_random_case (Color(Red)) grid;
    grid

let add c1 c2 = match c1, c2 with
  | Empty, _
  | _, Empty -> Failed
  | Color cc1, Color cc2 -> match cc1, cc2 with
    | Red, Blue
    | Blue, Red -> Success(Magenta)
    | Green, Blue
    | Blue, Green -> Success(Cyan)
    | Red, Green
    | Green, Red -> Success(Yellow)
    | Magenta, Green
    | Green, Magenta
    | Yellow, Blue
    | Blue, Yellow
    | Red, Cyan
    | Cyan, Red -> Win
    | _ -> Failed

let sub c1 c2 = match c1, c2 with
  | Empty, _
  | _, Empty -> Failed
  | Color cc1, Color cc2 -> match cc1, cc2 with
    | Yellow, Magenta
    | Magenta, Yellow -> Success(Red)
    | Cyan, Magenta
    | Magenta, Cyan -> Success(Blue)
    | Yellow, Cyan
    | Cyan, Yellow -> Success(Green)
    | _ -> Failed

let get_next_case grid y x =  function
    | Left -> if x != 0 then Some(grid.(y).(x - 1)) else None
    | Right -> if x != 3 then Some(grid.(y).(x + 1)) else None
    | Up -> if y != 0 then Some(grid.(y - 1).(x)) else None
    | Down -> if y!= 3 then Some(grid.(y + 1).(x)) else None

let set_next_case grid value y x = function
    | Left -> if x != 0 then grid.(y).(x - 1) <- value else ()
    | Right -> if x != 3 then grid.(y).(x + 1) <- value else ()
    | Up -> if y != 0 then grid.(y - 1).(x) <- value else ()
    | Down -> if y!= 3 then grid.(y + 1).(x) <- value else ()

let fusion f direction grid =
    let do_case y x =
        let next_case = get_next_case grid y x direction in
        match next_case with
            | Some case -> begin
                match (f case (grid.(y).(x))) with
                    | Success color as value -> set_next_case grid (Color color) y x direction
                    | Failed -> ()
                    | Win -> ()
                end
            | None -> ()
    in Array.iteri (fun y line -> Array.iteri (fun x _ -> do_case y x) line) grid
