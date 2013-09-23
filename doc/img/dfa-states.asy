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

draw_graph ("dfa-states-dreml.log", black, "re2ml");
draw_graph ("dfa-states-ulex.log", brown, "ml-ulex");

xaxis ("$n$", Bottom, LeftTicks);
xaxis (Top);
yaxis ("states", Left, RightTicks (trailingzero));
yaxis (Right);

add (legend (), point (E), 20E, UnFill);
