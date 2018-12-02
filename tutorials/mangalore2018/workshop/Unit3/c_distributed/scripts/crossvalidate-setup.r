suppressMessages(library( locfit , quietly = TRUE))
# set.seed( 12345 )

f <- function( x ) {                          # Sombrero function
    rho <- sqrt( x[ 1 ]^2 + x[ 2 ]^2 )
    2 * besselJ( pi * rho, 1 ) / ( pi * rho )
}
x.seq <- y.seq <- seq( -pi, pi, length.out = 100 )
z <- matrix( NA, 100, 100 )                   # Evaluate f on 100x100 grid
for ( i in 1:length( x.seq ) ) {
    for ( j in 1:length( y.seq ) ) {
        z[ i, j ] <- f( c( x.seq[ i ], y.seq[ j ] ) )
    }
}
# image(z, col = rainbow(128))

sigma.true <- 0.1              # Generate observations with error
n <- 1000
x1 <- runif( n, -pi, pi )
x2 <- runif( n, -pi, pi )
y <- numeric( n )
f.true <- numeric( n )
for ( i in 1:n ) {
   y[ i ] <- f( c( x1[ i ], x2[ i ] ) ) + rnorm( 1, 0, sigma.true )
   f.true[ i ] <- f( c( x1[ i ], x2[ i ] ) )
}
dat <- data.frame( y = y, x1 = x1, x2 = x2 )

