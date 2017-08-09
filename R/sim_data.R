sim_1pl <- function(J, K, mu_beta = 0, sigma_beta = 2, seed = NULL) {
  if (!is.null(seed)) { set.seed }

  true_theta <- rnorm(J, 0, 1)
  true_beta  <- rnorm(K, mu_beta, sigma_beta)

  parmat <- expand.grid(true_theta, true_beta)
  names(parmat) <- c("theta", "beta")
  idmat <- expand.grid(1:J, 1:K)
  names(idmat) <- c("j", "k")

  d <- data.table("i" = 1:(J*K), idmat, parmat)
  d[, "y" := rbinom(.N, 1, plogis(theta - beta))]

  res <- list(
    "J" = J,
    "K" = K,
    "N" = J*K,
    "j" = d[, j],
    "k" = d[, k],
    "y" = d[, y]
  )

  return(res)
}

sim_2pl <- function(J, K, mu_beta = 0, sigma_beta = 2, seed = NULL) {
  if (!is.null(seed)) { set.seed }

  true_theta <- rnorm(J, 0, 1)
  true_beta  <- rnorm(K, mu_beta, sigma_beta)
  true_alpha <- rlnorm(K, 0.5, 0.5)

  idmat <- expand.grid(1:J, 1:K)
  names(idmat) <- c("j", "k")

  d <- data.table("i" = 1:(J*K), idmat)
  d[, c("alpha", "beta", "theta") := .(true_alpha[k], true_beta[k], true_theta[j])]

  d[, "y" := rbinom(.N, 1, plogis(alpha*theta - beta))]

  res <- list(
    "J" = J,
    "K" = K,
    "N" = J*K,
    "j" = d[, j],
    "k" = d[, k],
    "y" = d[, y],
    "true_alpha" = d[, unique(alpha)],
    "true_beta"  = d[, unique(beta)],
    "true_theta" = d[, unique(theta)] 
  )

  return(res)
}
