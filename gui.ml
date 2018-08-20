open Graphics
open Random

type point = int*int

(* Representation type for a rectangle on a grid. *)
type rectangle = {ll: point; lr: point; ur: point; ul: point}

(* Representation type for an ellipse. *)
type ellipse = {center:point; horizontal_r:int; vertical_r:int}

(* [create_board size title num_lines o1 o2] creates a [size x size] grid in a
 * window with title [title] with [num_lines] rows and columns. [(o1, o2)]
 * represent the user's intended origin for the board. (For our
 * purposes, (o1,o2) = (0,0), i.e the lower left corner of the window.) *)
let create_board size title num_lines o1 o2=

  Graphics.open_graph (" "^(string_of_int size)^"x"^(string_of_int size));
  Graphics.set_window_title title;

  (let width = size/num_lines in


  for i=0 to num_lines do
    Graphics.moveto (i*width) o2;
    Graphics.lineto (i*width) size;
  done;

  for i=0 to num_lines do
    Graphics.moveto o1 (i*width);
    Graphics.lineto size (i*width);
  done)

let standard_board () = create_board 700 "BattleShip" 10 0 0

(* [tile_to_coord scale (row, col)] converts a tile coordinate [(row, col)] to a
 * coordinate on the x-y axis of the board grid. [scale] is the width of a box
 * on the grid, i.e. size of window / num_lines. For our purposes,
 * [scale] = 700/10 = 70. *)
let tile_to_coord scale (row,col) =
  (col*scale,row*scale)

let ship_hit width (row,col) =
  let draw_triangle w x y =
    Graphics.set_color red;
    Graphics.moveto x y;
    Graphics.lineto (x+w) y ;
    Graphics.lineto (x+w/2) (y+w);
    Graphics.lineto x y ;
  in

  let (x,y) = tile_to_coord width (row,col) in

  draw_triangle width x y


let player_attack width (row,col) hit =
  let hit_ship x y w =
    Graphics.set_color blue;
    Graphics.moveto x y;
    Graphics.lineto (x+w)  (y+w);
    Graphics.moveto (x+w) y;
    Graphics.lineto x (y+w);
  in

  let miss x y w =
    let dark_green = Graphics.rgb 34 139 34 in
    Graphics.set_color dark_green;
    let radius = w/2 in
    let center = (x+w/2,y+w/2) in
    Graphics.draw_circle (fst center) (snd center) radius; ()

  in

  let (x,y) = tile_to_coord width (row,col) in
  if hit then hit_ship x y width else miss x y width

let draw_ship w (start_pos, end_pos) =
  (* Converts the input to a rectangle that spans from [(r1,c1)] to [(r2,c2)]. *)
  let coords_to_rect (r1,c1) (r2,c2) =
    if c1=c2 then
      let (ulx, uly) = tile_to_coord w (r2+1,c2) in
      let (llx, lly) = tile_to_coord w (r1, c1) in

      let (lrx, lry) = (llx + w, lly) in
      let (urx, ury) = (ulx + w, uly) in

      {ll = (llx,lly);
       lr = (lrx,lry);
       ul = (ulx,uly);
       ur = (urx,ury);
      }
    else
      let (llx,lly) = tile_to_coord w (r1, c1) in
      let (lrx, lry) = tile_to_coord w (r2,c2+1) in

      let (ulx, uly) = (llx, lly + w) in
      let (urx, ury) = (lrx, lry + w) in
      {ll = (llx,lly);
       lr = (lrx,lry);
       ul = (ulx,uly);
       ur = (urx,ury);
      }
  in

  (* [rect_to_ellipse r] is the ellipse that can be inscribed within the
   * rectangle [r].*)
  let rect_to_ellipse r =
    let midpt (x1,y1) (x2,y2) =
      ((x1+x2)/2,(y1+y2)/2)
    in

    let center = midpt r.ll r.ur in
    let hr = ((fst r.lr) - (fst r.ll))/2 in
    let vr = ((snd r.ul) - (snd r.ll))/2 in
    {center = center;
    horizontal_r = hr;
    vertical_r = vr;}
  in

  let grid_coords = coords_to_rect start_pos end_pos in
  let e = rect_to_ellipse grid_coords in

  let orange = Graphics.rgb 255 165 0 in
  let colors = [cyan; orange] in
  let rand_color = List.nth colors (Random.int (List.length colors)) in

  Graphics.set_color rand_color;
  Graphics.fill_ellipse (fst e.center) (snd e.center) e.horizontal_r e.vertical_r
