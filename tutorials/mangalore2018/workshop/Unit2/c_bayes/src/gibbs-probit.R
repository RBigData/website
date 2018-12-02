library(mvtnorm)

printf <- function(msg, ...) { cat(sprintf(msg, ...)) }
logger <- function(msg, ...) {
        sys.time <- as.character(Sys.time())
        cat(sys.time, "-", sprintf(msg, ...))
}

# Draw from a truncated version of the density f
# lo and hi are lower and upper truncation points
# pf is the CDF of f and qf is the quantile function
rtruncated <- function (n, lo, hi, pf, qf, ...) {
	qf(pf(lo, ...) + runif(n) * (pf(hi, ...) - pf(lo, ...)), ...)
}

gibbs.probit <- function(y, X, R, burn = 0, thin = 1,
	report.period = R+1, hyper = NULL)
{
	n <- nrow(X)
	d <- ncol(X)

	R.keep <- ceiling((R - burn) / thin)
	idx.keep <- 0

	Beta.hist <- matrix(NA, R.keep, d)
	z.hist <- matrix(NA, R.keep, n)

	# Set default hyperparameter if nothing is passed in
	# We assume mu.prior = 0 in this code
	if (is.null(hyper)) {
		hyper <- list(V.prior = 1000 * diag(d))
	}

	# ----- Pre-compute some necessary quantities -----
	Sigma.Beta <- solve(t(X) %*% X + solve(hyper$V.prior))

	lo <- numeric(n)
	hi <- numeric(n)
	idx1 <- which(y == 1)
	idx0 <- which(y == 0)
	lo[idx0] <- -Inf
	hi[idx0] <- 0
	lo[idx1] <- 0
	hi[idx1] <- Inf

	# ----- Starting value -----
	Beta <- rep(0, d)

	# ----- MCMC -----
	for (r in 1:R)
	{
		if (r %% report.period == 0)
			logger("Starting iteration %d\n", r)

		# ----- Draw [z | Rest] -----
		z <- rtruncated(n, lo = lo, hi = hi, pf = pnorm,
			qf = qnorm, mean = X %*% Beta, sd = 1)

		# ----- Draw [Beta | Rest] -----
		mean.Beta <- Sigma.Beta %*% (t(X) %*% z)
		Beta <- t(rmvnorm(1, mean = mean.Beta, sigma = Sigma.Beta))

		if (r > burn && r %% thin == 0) {
			idx.keep <- idx.keep + 1
			Beta.hist[idx.keep,] <- Beta
			z.hist[idx.keep,] <- z
		}
	}

	list(Beta.hist = Beta.hist, z.hist = z.hist)
}
