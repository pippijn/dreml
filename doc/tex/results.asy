import graph;

size (16cm, 8cm, IgnoreAspect);

marker mark = marker (unitcircle);

void draw_graph (string data, pen pen, string name) {
  file in = input (data).line ().csv ();

  string[] columnlabel = in;

  real[][] A = in.dimension (0, 0);
  A = transpose (A);
  real[] x = A[0];
  real[] y = A[1];

  draw (graph (x, y), pen, name, mark);
}

draw_graph ("difference.log", black, "$f(n)-f(n-1)$");
//draw_graph ("results.log", dashed, "results");
//draw_graph ("unique-results.log", black, "unique");
//draw_graph ("inner-star-results.log", dashed, "unique");

xaxis ("input length", Bottom, LeftTicks);
xaxis (Top);
yaxis ("results", Left, RightTicks (trailingzero));
yaxis (Right);

add (legend (), point (E), 20E, UnFill);
