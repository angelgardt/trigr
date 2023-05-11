trg_incenterCoords <- function(A, B, C) {
  a = trigr::trg_dist(B, C)
  b = trigr::trg_dist(A, C)
  c = trigr::trg_dist(A, B)
  I = (a * A + b * B + c * C) / sum(a, b, c)
  return(x = I[1], y = I[2])
}
