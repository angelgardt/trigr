trg_haversin <- function(x, units = "rad") {
  if (units == "deg") {
    x = trigr::trg_degToRad(x)
  }
  return(trigr::trg_versin(x) / 2)
}
