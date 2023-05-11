trgCircumcenterCoords <- function(A, B, C) {
  D = 2 * (A[1] * (B[2] - C[2]) + B[1] * (C[2] - A[2]) + C[1] * (A[2] - B[2]))
  x = (sum(A^2) * (B[2] - C[2]) + sum(B^2) * (C[2] - A[2]) + sum(C^2) * (A[2] - B[2])) / D
  y = (sum(A^2) * (C[1] - B[1]) + sum(B^2) * (A[1] - C[1]) + sum(C^2) * (B[1] - A[1])) / D
  return(c(x = x, y = y))
}
