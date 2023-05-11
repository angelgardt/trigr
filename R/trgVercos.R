trgVercos <- function(x, units = "rad") {
  if (units == "deg") {
    x = trigr::trgDegToRad(x)
  }
  return(1 - sin(x))
}
