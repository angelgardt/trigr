trgTriangleByCoords <- function(A, B, C) {
  a = trigr::trgDist(B, C) # BC
  b = trigr::trgDist(A, C) # AC
  c = trigr::trgDist(A, B) # AB
  return(trigr::trgTriangleBySSS(a = a, b = b, c = c))
}
