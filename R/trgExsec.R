trgExsec <- function(x, units = "rad") {
  if (units == "deg") {
    x = trigr::trgDegToRad(x)
  }
  return(trigr::trgSec(x) - 1)
}
