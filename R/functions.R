degToRad <- function(deg) {
  return(deg * pi / 180)
}
radToDeg <- function(rad) {
  return(rad * 180 / pi)
}

# cos <- function(x, units = "rad") {
#   if (units == "deg") {
#     x = degToRad(x)
#   }
#   return(cos(x))
# }
# sin <- function(x, units = "rad") {
#   if (units == "deg") {
#     x = degToRad(x)
#   }
#   return(sin(x))
# }
# tan <- function(x, units = "rad") {
#   if (units == "deg") {
#     x = degToRad(x)
#   }
#   return(tan(x))
# }
cot <- function(x, units = "rad") {
  if (units == "deg") {
    x = degToRad(x)
  }
  return(1 / tan(x))
}
sec <- function(x, units = "rad") {
  if (units == "deg") {
    x = degToRad(x)
  }
  return(1 / cos(x))
}
csc <- function(x, units = "rad") {
  if (units == "deg") {
    x = degToRad(x)
  }
  return(1 / sin(x))
}

triangleByThreeSides <- function(a, b, c) {
  alpha = acos((b ^ 2 + c ^ 2 - a ^ 2) / (2 * b * c))
  beta = acos((a ^ 2 + c ^ 2 - b ^ 2) / (2 * a * c))
  gamma = pi - alpha - beta

  result = list(triangle = c(
    a = a,
    b = b,
    c = c,
    alpha = alpha,
    beta = beta,
    gamma = gamma
  ))

  return(result)
}
triangleByTwoSidesAngleBetween <- function(a, b, gamma) {
  c = sqrt(a ^ 2 + b ^ 2 - 2 * a * b * cos(gamma))
  alpha = acos((b ^ 2 + c ^ 2 - a ^ 2) / (2 * b * c))
  beta = pi - alpha - gamma

  result = list(triangle = c(
    a = a,
    b = b,
    c = c,
    alpha = alpha,
    beta = beta,
    gamma = gamma
  ))

  return(result)
}
triangleByTwoSidesAngleOpposite <- function(b, c, beta) {
  D = c / b * sin(beta)

  if (D > 1 | (beta >= pi / 2 & b <= c)) {
    return("Impossible Triangle")

  }

  if (D == 1) {
    gamma = asin(D)
    a = pi - beta - gamma
    a = b * sin(alpha) / sin(beta)

    result = list(triangle = c(
      a = a,
      b = b,
      c = c,
      alpha = alpha,
      beta = beta,
      gamma = gamma
    ))

    return(result)
  }

  if (D < 1) {
    if (b < c) {
      gamma1 = asin(D)
      gamma2 = pi - gamma1
      alpha1 = pi - beta - gamma1
      alpha2 = pi - beta - gamma2
      a1 = b * sin(alpha1) / sin(beta)
      a2 = b * sin(alpha2) / sin(beta)

      result = list(
        triangle1 = c(
          a = a1,
          b = b1,
          c = c1,
          alpha = alpha1,
          beta = beta1,
          gamma = gamma1
        ),
        triangle2 = c(
          a = a2,
          b = b2,
          c = c2,
          alpha = alpha2,
          beta = beta2,
          gamma = gamma2
        )
      )
      return(result)
    }

    if (b >= c) {
      gamma = asin(D)
      alpha = pi - beta - gamma
      a = b * sin(alpha) / sin(beta)

      result = list(triangle = c(
        a = a,
        b = b,
        c = c,
        alpha = alpha,
        beta = beta,
        gamma = gamma
      ))
      return(result)
    }
  }
}
triangleBySideTwoAngles <- function(c, alpha, beta) {
  gamma = pi - alpha - beta
  a = c * sin(alpha) / sin(gamma)
  b = c * sin(beta) / sin(gamma)
  result = list(triangle = c(
    a = a,
    b = b,
    c = c,
    alpha = alpha,
    beta = beta,
    gamma = gamma
  ))
  return(result)
}
triangleByCoords <- function(A, B, C) {
  a = rbind(B, C) |> dist() |> as.vector() # BC
  b = rbind(A, C) |> dist() |> as.vector() # AC
  c = rbind(A, B) |> dist() |> as.vector()# AB
  if (a + b < c | a + c < b | b + c < a) {
    return("Impossible triangle")
  }
  if (a + b == c | a + c == b | b + c == a) {
    return("Degenerate triangle")
  }
  return(triangleByThreeSides(a = a, b = b, c = c))
}

incenterCoords <- function(A, B, C, a, b, c) {
  return((a * A + b * B + c * C) / sum(a, b, c))
}
circumcenterCoords <- function(A, B, C) {
  D = 2 * (A[1] * (B[2] - C[2]) + B[1] * (C[2] - A[2]) + C[1] * (A[2] - B[2]))
  x = ((A[1]^2 + A[2]^2) * (B[2] - C[2]) + (B[1]^2 + B[2]^2) * (C[2] - A[2]) + (C[1]^2 + C[2]^2) * (A[2] - B[2])) / D
  y = ((A[1]^2 + A[2]^2) * (C[1] - B[1]) + (B[1]^2 + B[2]^2) * (A[1] - C[1]) + (C[1]^2 + C[2]^2) * (B[1] - A[1])) / D
  return(c(x, y))
}
centroidCoords <- function(A, B, C) {
  return((A + B + C) / 3)
}
orthocenterCoords <- function(A, B, C) {
  m1 = -(C[1] - B[1]) / (C[2] - B[2])
  m2 = -(B[1] - A[1]) / (B[2] - A[2])
  x = ((m1 * A[1] - A[2]) - (m2 * C[1] - C[2])) / (m1 - m2)
  y = m1 * (x - A[1]) + A[2]
  return(c(x, y))
}

midpointOnLineSegment <- function(A, B) {
  return((A + B)/2)
}
pointOnLineSegment <- function(A, B, lambda) {
  x = (A[1] + lambda * B[1]) / (1 + lambda)
  y = (A[2] + lambda * B[2]) / (1 + lambda)
  return(c(x, y))
}
