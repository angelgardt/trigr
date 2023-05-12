trgSolveTriangle <- function(A = NULL, B = NULL, C = NULL,
                             a = NULL, b = NULL, c = NULL,
                             alpha = NULL, beta = NULL, gamma = NULL,
                             coords = FALSE, draw = FALSE) {
  if (!is.null(A) & !is.null(B) & !is.null(C)) {
    trigonum <- trigr::trgTriangleByCoords(A, B, C)
  } else if (!is.null(a) & !is.null(b) & !is.null(c)) {
    trigonum <- trigr::trgTriangleBySSS(a, b, c)
  } else {
    print("pass")
  }

  solution <- list()

  elements <- list()
  elements$a = trigonum['a']
  elements$b = trigonum['b']
  elements$c = trigonum['c']
  elements$alpha = trigonum['alpha']
  elements$beta = trigonum['beta']
  elements$gamma = trigonum['gamma']
  elements$perimeter = sum(trigonum[c('a', 'b', 'c')])
  elements$semiperimeter = elements$perimeter / 2
  elements$area = trigr::trgAreaHeron(a = trigonum['a'],
                                      b = trigonum['b'],
                                      c = trigonum['c'])
  elements$inradius = trigr::trgInradius(area = elements$area,
                                         s = elements$semiperimeter)
  elements$incircle = trigr::trgCircle(elements$inradius)
  elements$indisk = trigr::trgDisk(elements$inraduis)
  elements$circumradius = trigr::trgCircumradius(a = trigonum['a'],
                                                 b = trigonum['b'],
                                                 c = trigonum['c'],
                                                 area = elements$area)
  elements$circumcircle = trigr::trgCircle(elements$circumradius)
  elements$circumdisk = trigr::trgDisk(elements$circumradius)
  elements$ma = list(
    l = trigr::trgMedian(a = trigonum['a'],
                         b = trigonum['b'],
                         c = trigonum['c'],
                         side = "a"),
    a = trigonum['a'] / 2
  )
  elements$mb = list(
    l = trigr::trgMedian(a = trigonum['a'],
                         b = trigonum['b'],
                         c = trigonum['c'],
                         side = "b"),
    b = trigonum['b'] / 2
  )
  elements$mc = list(
    l = trigr::trgMedian(a = trigonum['a'],
                         b = trigonum['b'],
                         c = trigonum['c'],
                         side = "c"),
    c = trigonum['c'] / 2
  )
  elements$ha = list(
    l = trigr::trgAltitude(b = trigonum['b'],
                           c = trigonum['c'],
                           R = elements$circumradius,
                           side = "a"),
    b = trigonum['c'] * cos(trigonum['beta']),
    c = trigonum['b'] * cos(trigonum['gamma'])
  )
  elements$hb = list(
    l = trigr::trgAltitude(a = trigonum['a'],
                           c = trigonum['c'],
                           R = elements$circumradius,
                           side = "b"),
    a = trigonum['c'] * cos(trigonum['alpha']),
    c = trigonum['a'] * cos(trigonum['gamma'])
  )
  elements$hc = list(
    l = trigr::trgAltitude(b = trigonum['b'],
                           c = trigonum['c'],
                           R = elements$circumradius,
                           side = "c"),
    a = trigonum['b'] * cos(trigonum['alpha']),
    b = trigonum['a'] * cos(trigonum['beta'])
  )
  elements$la = list(
    l = trigr::trgAngBisector(a = trigonum['a'],
                              b = trigonum['b'],
                              c = trigonum['c'],
                              s = elements$semiperimeter,
                              side = "a"),
    b = prod(trigonum[c('a', 'b')]) / sum(trigonum[c('b', 'c')]),
    c = prod(trigonum[c('a', 'c')]) / sum(trigonum[c('b', 'c')])
  )
  elements$lb = list(
    l = trigr::trgAngBisector(a = trigonum['a'],
                              b = trigonum['b'],
                              c = trigonum['c'],
                              s = elements$semiperimeter,
                              side = "b"),
    a = prod(trigonum[c('a', 'b')]) / sum(trigonum[c('a', 'c')]),
    c = prod(trigonum[c('b', 'c')]) / sum(trigonum[c('a', 'c')])
  )
  elements$lc = list(
    l = trigr::trgAngBisector(a = trigonum['a'],
                              b = trigonum['b'],
                              c = trigonum['c'],
                              s = elements$semiperimeter,
                              side = "c"),
    a = prod(trigonum[c('a', 'c')]) / sum(trigonum[c('a', 'b')]),
    b = prod(trigonum[c('b', 'c')]) / sum(trigonum[c('a', 'b')])
    )

  solution$elements = elements

  if (draw) { coords = TRUE }

  if (coords) {
    coords <- trigr::trgTriangleCoords(A = A, B = B, C = C,
                                       triangle = trigonum)
    solution$coords = coords
    }

  if (draw) { trigr::trgDrawTriangle(coords = coords) }

  return(solution)
}
