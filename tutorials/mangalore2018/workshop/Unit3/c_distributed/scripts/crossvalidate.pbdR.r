library(pbdMPI, quietly = TRUE)
comm.set.seed(12345, diff = FALSE)
source( "crossvalidate-setup.r" )

K <- 5
fold.grp <- ( 1:n - 1 ) %% K + 1
folds <- split( 1:n, f = fold.grp )
y.hat.cv <- numeric( n )
alpha <- seq( 0.02, 0.3, 0.001 )

my.alpha.i <- get.jid( length( alpha ) ) # pbdR
SSPE.cv <- numeric( length( my.alpha.i ) ) # pbdR
for ( j in my.alpha.i ) { # pbdR
    for ( k in 1:K ) {
        idx <- folds[[ k ]]
        fit.fold.out <- locfit( y ~ lp( x1, x2, nn = alpha[ j ] ), data = dat[ -idx, ] )
        y.hat.cv[ idx ] <- predict( fit.fold.out, newdata = dat[ idx, ] )
    }
  SSPE.cv[ j - my.alpha.i[1] + 1 ] <- sum( ( y - y.hat.cv )^2 ) # pbdR
}

SSPE.cv <- do.call( c, allgather( SSPE.cv ) ) # pbdR

if( comm.rank() == 0 ) { # pbdR
    plot( alpha, SSPE.cv )
} # pbdR

finalize()  # pbdR

