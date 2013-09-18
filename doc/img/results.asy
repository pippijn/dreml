import graph;

size (10cm, 8cm, IgnoreAspect);

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

draw_graph ("results.log", black);

xaxis ("$n$", Bottom, LeftTicks);
xaxis (Top);
yaxis ("$t$[sec]", Left, RightTicks (trailingzero));
yaxis (Right);

add (legend (), point (E), 20E, UnFill);
