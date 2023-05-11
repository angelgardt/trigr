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
