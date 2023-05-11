trgSin <- function(x, units = "rad") {
  if (units == "deg") {
    x = trigr::trgDegToRad(x)
  }
  return(sin(x))
}
