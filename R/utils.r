# rescale numeric values to range [a, b] (min-max normalization)
rescale <- function(x, min, max, a = -1, b = 1){
  a + ((x - min) * (b - a))/(max - min)
}



unrescale <- function(x_scaled, xmin, xmax, a = -1, b = 1){
  ((x_scaled - a)*(xmax - xmin) + xmin * (b - a))/(b - a)
}


circ <- function(xc = 0, yc = 0, r = 1, n = 100){
  v <- seq(0, 2 * pi, len = n)
  cbind(x = xc + r * cos(v),
        y = yc + r * sin(v))
}
