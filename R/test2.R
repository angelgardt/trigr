A1 = c(1, 4); B1 = c(3, 1); C1 = c(1, 1)
A2 = c(-2, 1); B2 = c(5, -2); C2 = c(1, -3)
A3 = c(-3, -3); B3 = c(2, -4); C3 = c(-1, 3)

tr1 <- getTriangleByCoords(A1, B1, C1)
tr2 <- getTriangleByCoords(A2, B2, C2)
tr3 <- getTriangleByCoords(A3, B3, C3)

# elements
a = tr2$triangle['a']
b = tr2$triangle['b']
c = tr2$triangle['c']
alpha = tr2$triangle['alpha']
beta = tr2$triangle['beta']
gamma = tr2$triangle['gamma']
P = a + b + c
p = P/2
r = sqrt(1/p * (p-a) * (p-b) * (p-c))
S = r * p
Lr = 2 * pi * r
Sr = pi * r^2
R = a * b * c / (4 * S)
LR = 2 * pi * R
SR = pi * R^2
ma_l = sqrt(2 * (b^2 + c^2) - a^2) / 2
ma_a = a / 2
mb_l = sqrt(2 * (a^2 + c^2) - b^2) / 2
mb_b = b / 2
mc_l = sqrt(2 * (a^2 + b^2) - c^2) / 2
mc_c = c / 2
ha_l = b * c / (2 * R)
ha_ab = c * cos(beta)
ha_ac = b * cos(gamma)
hb_l = c * a / (2 * R)
hb_ba = c * cos(alpha)
hb_bc = a * cos(gamma)
hc_l = a * b / (2 * R)
hc_ca = b * cos(alpha)
hc_cb = a * cos(beta)
la_l = 2 * sqrt(b*c*p*(p-a)) / (b + c)
la_ab = b * a / (c + b)
la_ac = c * a / (b + c)
lb_l = 2 * sqrt(a*c*p*(p-b)) / (a + c)
lb_ba = a * b / (c + a)
lb_bc = c * b / (a + c)
lc_l = 2 * sqrt(a*b*p*(p-c)) / (a + b)
lc_ca = a * c / (b + a)
lc_cb = b * c / (a + b)

# coords

A = A2
B = B2
C = C2
r_ = getIncenterCoords(A, B, C, a, b, c) # incenter
R_ = getCircumcenterCoords(A, B, C) # circumcenter
centroid = getCentroidCoords(A, B, C)
orthocenter = getOrthocenterCoords(A, B, C)
ma = midpoint(B, C)
mb = midpoint(A, C)
mc = midpoint(A, B)
ha = pointOnLineSegment(B, C, ha_ab/ha_ac)
hb = pointOnLineSegment(A, C, hb_ba/hb_bc)
hc = pointOnLineSegment(A, B, hc_ca/hc_cb)
la = pointOnLineSegment(B, C, la_ac/la_ab)
lb = pointOnLineSegment(A, C, lb_bc/lb_ba)
lc = pointOnLineSegment(A, B, lc_cb/lc_ca)

# trig funs
rbind(
  rad = c(alpha, beta, gamma),
  deg = radToDeg(c(alpha, beta, gamma)),
  cos = cos(c(alpha, beta, gamma)),
  sin = sin(c(alpha, beta, gamma)),
  sec = sec(c(alpha, beta, gamma)),
  csc = csc(c(alpha, beta, gamma)),
  tan = tan(c(alpha, beta, gamma)),
  cot = cot(c(alpha, beta, gamma))
)


ggplot() +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  geom_line(aes(x = c(A[1], C[1]), y = c(A[2], C[2]))) +
  geom_line(aes(x = c(A[1], B[1]), y = c(A[2], B[2]))) +
  geom_line(aes(x = c(B[1], C[1]), y = c(B[2], C[2]))) +
  geom_point(aes(x = c(A[1]), y = c(A[2]))) +
  geom_point(aes(x = c(B[1]), y = c(B[2]))) +
  geom_point(aes(x = c(C[1]), y = c(C[2]))) +
  geom_point(aes(x = r_[1], y = r_[2]), color = "red") +
  geom_circle(aes(x0 = r_[1], y0 = r_[2], r = r), color = "red") +
  geom_point(aes(x = c(la[1], lb[1], lc[1]), y = c(la[2], lb[2], lc[2])), color = "red") +
  geom_line(aes(x = c(A[1], la[1]), y = c(A[2], la[2])), color = "red") +
  geom_line(aes(x = c(B[1], lb[1]), y = c(B[2], lb[2])), color = "red") +
  geom_line(aes(x = c(C[1], lc[1]), y = c(C[2], lc[2])), color = "red") +
  geom_point(aes(x = R_[1], y = R_[2]), color = "blue") +
  geom_circle(aes(x0 = R_[1], y0 = R_[2], r = R), color = "blue") +
  geom_line(aes(x = c(R_[1], ma[1]), y = c(R_[2], ma[2])), color = "blue") +
  geom_line(aes(x = c(R_[1], mb[1]), y = c(R_[2], mb[2])), color = "blue") +
  geom_line(aes(x = c(R_[1], mc[1]), y = c(R_[2], mc[2])), color = "blue") +
  geom_point(aes(x = c(ma[1], mb[1], mc[1], centroid[1]), y = c(ma[2], mb[2], mc[2], centroid[2])), color = "orange") +
  geom_line(aes(x = c(A[1], ma[1]), y = c(A[2], ma[2])), color = "orange") +
  geom_line(aes(x = c(B[1], mb[1]), y = c(B[2], mb[2])), color = "orange") +
  geom_line(aes(x = c(C[1], mc[1]), y = c(C[2], mc[2])), color = "orange") +
  geom_point(aes(x = orthocenter[1], y = orthocenter[2]), color = "green") +
  geom_point(aes(x = ha[1], y = ha[2]), color = "green") +
  geom_point(aes(x = hb[1], y = hb[2]), color = "green") +
  geom_point(aes(x = hc[1], y = hc[2]), color = "green") +
  geom_line(aes(x = c(A[1], ha[1]), y = c(A[2], ha[2])), color = "green") +
  geom_line(aes(x = c(B[1], hb[1]), y = c(B[2], hb[2])), color = "green") +
  geom_line(aes(x = c(C[1], hc[1]), y = c(C[2], hc[2])), color = "green") +
  geom_line(aes(x = c(orthocenter[1], ha[1]), y = c(orthocenter[2], ha[2])), color = "green", linetype = "dashed") +
  geom_line(aes(x = c(orthocenter[1], hb[1]), y = c(orthocenter[2], hb[2])), color = "green", linetype = "dashed") +
  geom_line(aes(x = c(orthocenter[1], hc[1]), y = c(orthocenter[2], hc[2])), color = "green", linetype = "dashed") +
  coord_fixed() +
  theme_minimal()

