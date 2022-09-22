crescimento <- function(N0, lambda, t) {
  n <- length(t)
  N <- c(N0, numeric(n))
  for (i in t) {
    N[i + 1] <- N[i] * lambda[i]
  }
  round(N, 0)
}