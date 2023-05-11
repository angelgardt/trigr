trgHavercos <- function(x, units = "rad") {
  if (units == "deg") {
    x = trigr::trgDegToRad(x)
  }
  return(trigr::trgVercos(x) / 2)
}
