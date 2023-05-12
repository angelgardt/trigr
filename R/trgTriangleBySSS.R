trgTriangleBySSS <- function(a = a,
                             b = b,
                             c = c) {
  if (a + b < c | a + c < b | b + c < a) {
    return("Impossible triangle")
  }
  if (a + b == c | a + c == b | b + c == a) {
    return("Degenerate triangle")
  }
  alpha = acos((b ^ 2 + c ^ 2 - a ^ 2) / (2 * b * c))
  beta = acos((a ^ 2 + c ^ 2 - b ^ 2) / (2 * a * c))
  gamma = pi - alpha - beta

  result = list(triangle = c(
    a = a,
    b = b,
    c = c,
    alpha = alpha,
    beta = beta,
    gamma = gamma
  ))

  return(result)
}
