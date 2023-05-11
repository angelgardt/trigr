trg_tan <- function(x, units = "rad") {
  if (units == "deg") {
    x = trigr::reg_degToRad(x)
  }
  return(tan(x))
}
