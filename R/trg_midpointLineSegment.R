# get coordinates of the midpoint of the line segment

trg_midpointLineSegment <- function(A, B) {
  midpoint = (A + B)/2
  return(c(x = midpoint[1], y = midpoint[2]))
}
