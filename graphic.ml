open Graphics

let border = 5
let side = 120
let size = 4 * side + 5 * border

let adition = ref true

let draw_square y x color =
  set_color color;
  if color != white
  then
    fill_rect ((succ x) * border + x * side)
      ((succ y) * border + y * side)
    side side

let draw_background () =
  if !adition then
    begin
      set_color black;
      fill_rect 0 0 (size_x ()) (size_y ());
      set_color (rgb 128 128 128)
    end
  else
    begin
      set_color (rgb 128 128 128);
      fill_rect 0 0 (size_x ()) (size_y ());
      set_color black
    end
  for a = 0 to 4 do
    fill_rect 0 (a * (border + side)) size border;
    fill_rect (a * (border + side)) 0  border size
  done

let test = [|
  [|white;blue;blue;red|];
  [|white;blue;red;white|];
  [|cyan;magenta;white;yellow|];
  [|white;white;white;white|];
           |]

let move = function
  |'2' -> draw_string "down"
  |'4' -> draw_string "left"
  |'6' -> draw_string "right"
  |'8' -> draw_string "up"
  |_ -> draw_string "other"

let toto () =
  open_graph (Printf.sprintf "%dx%d" size size);
  draw_background ();
  Array.iteri (fun y line -> Array.iteri (draw_square y) line ) test;
  loop_at_exit [Key_pressed] (fun event -> move event.key)
