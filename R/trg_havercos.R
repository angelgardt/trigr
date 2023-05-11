trg_havercos <- function(x, units = "rad") {
  if (units == "deg") {
    x = trigr::trg_degToRad(x)
  }
  return(trigr::trg_vercos(x) / 2)
}
