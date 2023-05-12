trgDrawTriangle <- function(triangle = NULL, coords = NULL) {

  if (is.null(coords)) { coords <- trigr::trgTriangleCoords(triangle = triangle)}

  ggplot() +
    geom_vline(xintercept = 0, color = "gray") +
    geom_hline(yintercept = 0, color = "gray") +
    geom_line(aes(x = c(A[1], C[1]), y = c(A[2], C[2])), color = "black") +
    geom_line(aes(x = c(A[1], B[1]), y = c(A[2], B[2])), color = "black") +
    geom_line(aes(x = c(B[1], C[1]), y = c(B[2], C[2])), color = "black") +
    geom_point(aes(x = c(A[1]), y = c(A[2])), color = "black") +
    geom_point(aes(x = c(B[1]), y = c(B[2])), color = "black") +
    geom_point(aes(x = c(C[1]), y = c(C[2])), color = "black") +
    geom_point(aes(x = r_[1], y = r_[2]), color = "red") +
    geom_circle(aes(x0 = r_[1], y0 = r_[2], r = r), color = "red") +
    geom_point(aes(x = c(la[1], lb[1], lc[1]), y = c(la[2], lb[2], lc[2])), color = "red") +
    geom_line(aes(x = c(A[1], la[1]), y = c(A[2], la[2])), color = "red") +
    geom_line(aes(x = c(B[1], lb[1]), y = c(B[2], lb[2])), color = "red") +
    geom_line(aes(x = c(C[1], lc[1]), y = c(C[2], lc[2])), color = "red") +
    geom_point(aes(x = R_[1], y = R_[2]), color = "blue") +
    geom_circle(aes(x0 = R_[1], y0 = R_[2], r = R), color = "blue") +
    geom_line(aes(x = c(R_[1], ma[1]), y = c(R_[2], ma[2])), color = "blue") +
    geom_line(aes(x = c(R_[1], mb[1]), y = c(R_[2], mb[2])), color = "blue") +
    geom_line(aes(x = c(R_[1], mc[1]), y = c(R_[2], mc[2])), color = "blue") +
    geom_point(aes(x = c(ma[1], mb[1], mc[1], centroid[1]), y = c(ma[2], mb[2], mc[2], centroid[2])), color = "orange") +
    geom_line(aes(x = c(A[1], ma[1]), y = c(A[2], ma[2])), color = "orange") +
    geom_line(aes(x = c(B[1], mb[1]), y = c(B[2], mb[2])), color = "orange") +
    geom_line(aes(x = c(C[1], mc[1]), y = c(C[2], mc[2])), color = "orange") +
    geom_point(aes(x = orthocenter[1], y = orthocenter[2]), color = "green") +
    geom_point(aes(x = ha[1], y = ha[2]), color = "green") +
    geom_point(aes(x = hb[1], y = hb[2]), color = "green") +
    geom_point(aes(x = hc[1], y = hc[2]), color = "green") +
    geom_line(aes(x = c(A[1], ha[1]), y = c(A[2], ha[2])), color = "green") +
    geom_line(aes(x = c(B[1], hb[1]), y = c(B[2], hb[2])), color = "green") +
    geom_line(aes(x = c(C[1], hc[1]), y = c(C[2], hc[2])), color = "green") +
    geom_line(aes(x = c(orthocenter[1], ha[1]), y = c(orthocenter[2], ha[2])), color = "green", lty = "dashed") +
    geom_line(aes(x = c(B[1], ha[1]), y = c(B[2], ha[2])), color = "black", lty = "dashed") +
    geom_line(aes(x = c(C[1], ha[1]), y = c(C[2], ha[2])), color = "black", lty = "dashed") +
    geom_line(aes(x = c(orthocenter[1], hb[1]), y = c(orthocenter[2], hb[2])), color = "green", lty = "dashed") +
    geom_line(aes(x = c(A[1], hb[1]), y = c(A[2], hb[2])), color = "black", lty = "dashed") +
    geom_line(aes(x = c(C[1], hb[1]), y = c(C[2], hb[2])), color = "black", lty = "dashed") +
    geom_line(aes(x = c(orthocenter[1], hc[1]), y = c(orthocenter[2], hc[2])), color = "green", lty = "dashed") +
    geom_line(aes(x = c(A[1], hc[1]), y = c(A[2], hc[2])), color = "black", lty = "dashed") +
    geom_line(aes(x = c(B[1], hc[1]), y = c(B[2], hc[2])), color = "black", lty = "dashed") +
    coord_fixed() +
    theme_minimal()

}
