trgTriangleBySSS <- function(a = a, b = b, c = c) {
  if (a + b < c | a + c < b | b + c < a) {
    return("Impossible triangle")
  }
  if (a + b == c | a + c == b | b + c == a) {
    return("Degenerate triangle")
  }
}
