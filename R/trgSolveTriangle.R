trgSolveTriangle <- function(A = NULL, B = NULL, C = NULL,
                              a = NULL, b = NULL, c = NULL,
                              alpha = NULL, beta = NULL, gamma = NULL) {
  if (!is.null(A) & !is.null(B) & !is.null(C)) {
    trigonum <- trigr::trgTriangleByCoords(A, B, C)
  } else if (!is.null(a) & !is.null(b) & !is.null(c)) {
    trigonum <- trigr::trgTriangleBySSS(a, b, c)
  } else if
}
