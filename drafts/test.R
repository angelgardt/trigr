library(tidyverse)
theme_set(theme_bw())


## tests -----

a = NA
b = NA
c = NA
alpha = NA
beta = NA
gamma = NA

## a, b, c -----
t = triangleByThreeSides(12, 10, 4.3)
NISTunits::NISTradianTOdeg(t$alpha)
NISTunits::NISTradianTOdeg(t$beta)
NISTunits::NISTradianTOdeg(t$gamma)

t = triangleByThreeSides(10, 25.32, 28.79)
NISTunits::NISTradianTOdeg(t$alpha)
NISTunits::NISTradianTOdeg(t$beta)
NISTunits::NISTradianTOdeg(t$gamma)


# a, b, gamma -----
t = triangleByTwoSidesAngleBetween(12, 10, NISTunits::NISTdegTOradian(20))
t$c
NISTunits::NISTradianTOdeg(t$alpha)
NISTunits::NISTradianTOdeg(t$beta)

t = triangleByTwoSidesAngleBetween(5, 6, NISTunits::NISTdegTOradian(90))
t$c
NISTunits::NISTradianTOdeg(t$alpha)
NISTunits::NISTradianTOdeg(t$beta)


# b, c, beta -----
t = triangleByTwoSidesAngleOpposite(5, 6, NISTunits::NISTdegTOradian(50.19))
t$a
NISTunits::NISTradianTOdeg(t$alpha)
NISTunits::NISTradianTOdeg(t$gamma)

# c, alpha, beta -----
t = triangleBySideTwoAngles(4.3, NISTunits::NISTdegTOradian(107.27), NISTunits::NISTdegTOradian(52.73))
t$a
t$b
NISTunits::NISTradianTOdeg(t$gamma)

t = triangleBySideTwoAngles(7.81, NISTunits::NISTdegTOradian(39.81), NISTunits::NISTdegTOradian(50.19))
t$a
t$b
NISTunits::NISTradianTOdeg(t$gamma)

# A, B, C -----
A1 = c(1, 4); B1 = c(3, 1); C1 = c(1, 1)
A2 = c(-2, 4); B2 = c(5, -2); C2 = c(-1, -2)
A3 = c(-3, -3); B3 = c(2, -3); C3 = c(-1, 3)
getTriangleByCoords(A1, B1, C1)
getTriangleByCoords(A2, B2, C2)
getTriangleByCoords(A3, B3, C3)


# Plots -----
ggplot() +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  geom_line(aes(x = c(A1[1], C1[1]), y = c(A1[2], C1[2]))) +
  geom_line(aes(x = c(A1[1], B1[1]), y = c(A1[2], B1[2]))) +
  geom_line(aes(x = c(B1[1], C1[1]), y = c(B1[2], C1[2]))) +
  geom_point(aes(x = c(A1[1], C1[1]), y = c(A1[2], C1[2]))) +
  geom_point(aes(x = c(A1[1], B1[1]), y = c(A1[2], B1[2]))) +
  geom_point(aes(x = c(B1[1], C1[1]), y = c(B1[2], C1[2]))) +
  coord_fixed()

ggplot() +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  geom_line(aes(x = c(A2[1], C2[1]), y = c(A2[2], C2[2]))) +
  geom_line(aes(x = c(A2[1], B2[1]), y = c(A2[2], B2[2]))) +
  geom_line(aes(x = c(B2[1], C2[1]), y = c(B2[2], C2[2]))) +
  geom_point(aes(x = c(A2[1], C2[1]), y = c(A2[2], C2[2]))) +
  geom_point(aes(x = c(A2[1], B2[1]), y = c(A2[2], B2[2]))) +
  geom_point(aes(x = c(B2[1], C2[1]), y = c(B2[2], C2[2]))) +
  coord_fixed()

ggplot() +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  geom_line(aes(x = c(A3[1], C3[1]), y = c(A3[2], C3[2]))) +
  geom_line(aes(x = c(A3[1], B3[1]), y = c(A3[2], B3[2]))) +
  geom_line(aes(x = c(B3[1], C3[1]), y = c(B3[2], C3[2]))) +
  geom_point(aes(x = c(A3[1], C3[1]), y = c(A3[2], C3[2]))) +
  geom_point(aes(x = c(A3[1], B3[1]), y = c(A3[2], B3[2]))) +
  geom_point(aes(x = c(B3[1], C3[1]), y = c(B3[2], C3[2]))) +
  coord_fixed()

# solution list -----
solution <- list(
  elements = list(
    a = "undefined",
    b = "undefined",
    c = "undefined",
    alpha = list(rad = "undefined",
                 deg = "undefined"),
    beta = list(rad = "undefined",
                deg = "undefined"),
    gamma = list(rad = "undefined",
                 deg = "undefined"),
    P = "undefined",
    p = "undefined",
    S = "undefined",
    r = "undefined",
    Lr = "undefined",
    Sr = "undefined",
    R = "undefined",
    LR = "undefined",
    SR = "undefined",
    ma = list(
      l = "undefined",
      a = "undefined"
    ),
    mb = list(
      l = "undefined",
      b = "undefined"
    ),
    mc = list(
      l = "undefined",
      c = "undefined"
    ),
    ha = list(
      l = "undefined",
      ab = "undefined",
      ac = "undefined"
    ),
    hb = list(
      l = "undefined",
      ba = "undefined",
      bc = "undefined"
    ),
    hc = list(
      l = "undefined",
      ca = "undefined",
      cb = "undefined"
    ),
    la = list(
      l = "undefined",
      ab = "undefined",
      ac = "undefined"
    ),
    lb = list(
      l = "undefined",
      ba = "undefined",
      bc = "undefined"
    ),
    lc = list(
      l = "undefined",
      ca = "undefined",
      cb = "undefined"
    )
  ),
  coords = list(
    A = "undefined",
    B = "undefined",
    C = "undefined",
    r = "undefined",
    R = "undefined",
    ma = "undefined",
    mb = "undefined",
    mc = "undefined",
    ha = "undefined",
    hb = "undefined",
    hc = "undefined",
    la = "undefined",
    lb = "undefined",
    lc = "undefined"
  )
)










