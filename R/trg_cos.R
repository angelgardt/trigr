trg_cos <- function(x, units = "rad") {
  if (units == "deg") {
    x = trigr::trg_degToRad(x)
  }
  return(cos(x))
}
