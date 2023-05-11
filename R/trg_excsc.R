trg_excsc <- function(x, units = "rad") {
  if (units == "deg") {
    x = trigr::trg_degToRad(x)
  }
  return(trigr::trg_csc(x) - 1)
}
