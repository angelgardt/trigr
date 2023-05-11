trgIncenterCoords <- function(A, B, C) {
  a = trigr::trgDist(B, C)
  b = trigr::trgDist(A, C)
  c = trigr::trgDist(A, B)
  I = (a * A + b * B + c * C) / sum(a, b, c)
  return(x = I[1], y = I[2])
}
