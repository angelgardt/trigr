trgOrthocenterCoords <- function(A, B, C) {
  m1 = -(C[1] - B[1]) / (C[2] - B[2])
  m2 = -(B[1] - A[1]) / (B[2] - A[2])
  x = ((m1 * A[1] - A[2]) - (m2 * C[1] - C[2])) / (m1 - m2)
  y = m1 * (x - A[1]) + A[2]
  return(c(x = x, y = y))
}
