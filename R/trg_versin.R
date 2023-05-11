trg_versin <- function(x, units = "rad") {
  if (units == "deg") {
    x = trigr::trg_degToRad(x)
  }
  return(1 - cos(x))
}
