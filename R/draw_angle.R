library(tidyverse)
library(ggforce)

phi <-  3 * pi / 5

# create data (start and end points of lines)
tibble(
  func = factor(
    rep(
      c(
        "sin",
        "cos",
        "sec",
        "csc",
        "tan",
        "cot",
        "versin",
        "vercos",
        "exsec",
        "excsc"
      ),
      each = 2
    ),
    ordered = TRUE,
    levels = c(
      "sin",
      "cos",
      "sec",
      "csc",
      "tan",
      "cot" ,
      "versin",
      "vercos",
      "exsec",
      "excsc"
    )
  ),
  point = rep(c("start", "end"), times = 10),
  class = c(rep("main", times = 6*2), rep("rare", times = 4*2)),
  x = c(
    sin_s = cos(phi),
    sin_e = cos(phi),
    cos_s = 0,
    cos_e = cos(phi),
    sec_s = 0,
    sec_e = 1,
    csc_s = 0,
    csc_e = 0,
    tan_s = 1,
    tan_e = 1,
    cot_s = 0,
    cot_e = cos(phi),
    versin_s = cos(phi),
    versin_e = 1,
    vercos_s = cos(phi),
    vercos_e = cos(phi),
    exsec_s = 1,
    exsec_e = cos(phi),
    excsc_s = 0,
    excsc_e = 0
  ),
  y = c(
    sin_s = 0,
    sin_e = sin(phi),
    cos_s = 0,
    cos_e = 0,
    sec_s = 0,
    sec_e = tan(phi),
    csc_s = 0,
    csc_e = 1 / sin(phi),
    tan_s = 0,
    tan_e = tan(phi),
    cot_s = 1 / sin(phi),
    cot_e = sin(phi),
    versin_s = 0,
    versin_e = 0,
    vercos_s = sin(phi),
    vercos_e = 1,
    exsec_s = tan(phi),
    exsec_e = sin(phi),
    excsc_s = 1 / sin(phi),
    excsc_e = 1
  )
) |>
  # plot
  ggplot() +
  # axes
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  geom_hline(yintercept = 1, color = "gray", linetype = "dashed") +
  # circle
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  # radius vector
  geom_line(data = tibble(x = c(0, cos(phi)),
                          y = c(0, sin(phi))),
            aes(x, y)) +
  geom_arc(aes(
    x0 = 0,
    y0 = 0,
    r = .25,
    start = 3 * pi / 6 - phi,
    end = 3 * pi / 6
  )) +
  # trig funs
  geom_line(aes(x = x,
                y = y,
                color = func,
                linewidth = class)) +
  # point
  geom_point(data = tibble(x = cos(phi),
                           y = sin(phi)),
             aes(x, y)) +
  # create grid
  scale_x_continuous(breaks = seq(-1, 1, by = .5)) +
  scale_y_continuous(breaks = seq(-2, 3, by = .5)) +
  # fix proportions
  coord_fixed(xlim = c(-1, 1),
              ylim = c(-2, 2)) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  # set colors
  scale_color_manual(
    values = c(
      sin = "salmon",
      cos = "royalblue",
      csc = "orchid",
      sec = "turquoise",
      tan = "tan4",
      cot = "orange2",
      versin = "gold",
      vercos = "seagreen",
      exsec = "pink",
      excsc = "green"
    )
  ) +
  scale_linewidth_manual(values = c(main = 1.5, rare = .5)) +
  guides(linewidth = "none")

