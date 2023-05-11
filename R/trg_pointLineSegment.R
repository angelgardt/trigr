trg_pointLineSegment <- function(A, B, lambda = 1) {
  x = (A[1] + lambda * B[1]) / (1 + lambda)
  y = (A[2] + lambda * B[2]) / (1 + lambda)
  return(c(x = x, y = y))
}
