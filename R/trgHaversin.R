trgHaversin <- function(x, units = "rad") {
  if (units == "deg") {
    x = trigr::trgDegToRad(x)
  }
  return(trigr::trgVersin(x) / 2)
}
