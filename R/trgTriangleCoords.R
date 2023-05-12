trgTriangleCoords <- function(A = NULL, B = NULL, C = NULL,
                              triangle = NULL) {
  incenter = trigr::trgIncenterCoords(A, B, C, a, b, c)
  circumcenter= trigr::trgCircumcenterCoords(A, B, C)
  centroid = trigr::trgCentroidCoords(A, B, C)
  orthocenter = trigr::trgOrthocenterCoords(A, B, C)
  ma = trigr::trgMidpointLineSegment(B, C)
  mb = trigr::midpointOnLineSegment(A, C)
  mc = midpointOnLineSegment(A, B)
  ha = pointOnLineSegment(B, C, ha_ab/ha_ac)
  hb = pointOnLineSegment(A, C, hb_ba/hb_bc)
  hc = pointOnLineSegment(A, B, hc_ca/hc_cb)
  la = pointOnLineSegment(B, C, la_ac/la_ab)
  lb = pointOnLineSegment(A, C, lb_bc/lb_ba)
  lc = pointOnLineSegment(A, B, lc_cb/lc_ca)
}
