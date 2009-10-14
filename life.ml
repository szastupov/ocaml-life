open Graphics

let csz = 15							(* cell size px *)
let shift = 10							(* Shift from x and y *)
let size = 20							(* greed size *)

let draw_cell x y alive =
  let calc v = shift+(v*csz)-csz/2 in
  let x, y = calc x, calc y
  in
	if alive then
	  set_color 0xfce94f
	else
	  set_color 0xbabdb6;
	fill_rect x y csz csz;
	set_color black;
	draw_rect x y csz csz

let draw_greed gr =
  for x=0 to size-1 do
	for y=0 to size-1 do
	  draw_cell x y gr.(x).(y)
	done
  done

let make_state () =
  let make_line _ =
	Array.make size false
  in
	Array.init size make_line

let neighbours st x y =
  let get x y =
	try if st.(x).(y) then 1 else 0
	with _ -> 0
  in
	get (x-1) y
	+ get (x+1) y
	+ get x (y-1)
	+ get x (y+1)
	+ get (x+1) (y+1)
	+ get (x+1) (y-1)
	+ get (x-1) (y+1)
	+ get (x-1) (y-1)

let recalc l r =
  for x=0 to size-1 do
	for y=0 to size-1 do
	  let count = neighbours l x y in
		if l.(x).(y) && not (count == 2 || count == 3) then
		  r.(x).(y) <- false			(* die *)
		else if not l.(x).(y) && count == 3 then
		  r.(x).(y) <- true				(* born *)
		else
		  r.(x).(y) <- l.(x).(y);		(* stay as is *)
		if r.(x).(y) != l.(x).(y) then
		  draw_cell x y r.(x).(y)
	done
  done

let click l x y =
  let calc v = (v-shift)/csz in
  let x, y = calc x, calc y in
	if x < size && y < size then
	  begin
		l.(x).(y) <- not l.(x).(y);
		draw_cell x y l.(x).(y)
	  end

let start_game () =
  resize_window (csz*size+shift) (csz*size+shift);
  let rec loop l r =
	let ev = wait_next_event [Key_pressed; Button_down] in
	  if not (ev.keypressed && ev.key == 'q') then
		if ev.button then
		  (click l ev.mouse_x ev.mouse_y; loop l r)
		else
		  (recalc l r; loop r l)
  and l = make_state ()
  and r = make_state ()
  in
	draw_greed l;
	loop l r

let _ =
  open_graph "";
  start_game ()
