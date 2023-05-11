trgExcsc <- function(x, units = "rad") {
  if (units == "deg") {
    x = trigr::trgDegToRad(x)
  }
  return(trigr::trgCsc(x) - 1)
}
