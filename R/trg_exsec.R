trg_exsec <- function(x, units = "rad") {
  if (units == "deg") {
    x = trigr::trg_degToRad(x)
  }
  return(trigr::trg_sec(x) - 1)
}
