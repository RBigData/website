d.beta.binom <- function(x, Pi, rho, m, log = FALSE)
{
	a <- Pi * rho^(-2) * (1 - rho^2)
	b <- (1 - Pi) * rho^(-2) * (1 - rho^2)
	log.ff <- lgamma(m + 1) - lgamma(x + 1) - lgamma(m - x + 1) +
		lgamma(a + x) + lgamma(b + m - x) - lgamma(a + b + m) +
		lgamma(a + b) - lgamma(a) - lgamma(b)
	if (log) return(log.ff)
	else return(exp(log.ff))
}

r.beta.binom <- function(n, Pi, rho, m)
{
	a <- Pi * rho^(-2) * (1 - rho^2)
	b <- (1 - Pi) * rho^(-2) * (1 - rho^2)
	z <- rbeta(n, a, b)
	rbinom(n, size = m, prob = z)
}
