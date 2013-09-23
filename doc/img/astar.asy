import graph;

size (16cm, 5cm, IgnoreAspect);

marker mark = marker (unitcircle);

void draw_graph (string data, pen pen, string name = "") {
  file in = input (data).line ().csv ();

  string[] columnlabel = in;

  real[][] A = in.dimension (0, 0);
  A = transpose (A);
  real[] x = A[0];
  real[] y = A[1];

  if (name == "") {
    draw (graph (x, y), pen, mark);
  } else {
    draw (graph (x, y), pen, name, mark);
  }
}

draw_graph ("astar-dreml.log", black, "re2ml");
draw_graph ("astar-ulex.log", brown, "ml-ulex");
draw_graph ("astar-perl.log", darkgreen, "perl");
draw_graph ("astar-pcre.log", darkblue, "pcre");

xaxis ("$n$", Bottom, LeftTicks);
xaxis (Top);
yaxis ("$t$[sec]", Left, RightTicks (trailingzero));
yaxis (Right);

add (legend (), point (E), 20E, UnFill);
