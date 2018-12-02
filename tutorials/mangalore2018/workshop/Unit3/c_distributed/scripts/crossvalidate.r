set.seed(12345)
source( "crossvalidate-setup.r" )

K <- 5
fold.grp <- ( 1:n - 1 ) %% K + 1
folds <- split( 1:n, f = fold.grp )
y.hat.cv <- numeric( n )
alpha <- seq( 0.02, 0.3, 0.001 )

SSPE.cv <- numeric( length( alpha ) )

for ( j in 1:length( alpha ) ) {
    for ( k in 1:K ) {
        idx <- folds[[ k ]]
        fit.fold.out <- locfit( y ~ lp( x1, x2, nn = alpha[ j ] ), data = dat[ -idx, ] )
        y.hat.cv[ idx ] <- predict( fit.fold.out, newdata = dat[ idx, ] )
    }
  SSPE.cv[ j ] <- sum( ( y - y.hat.cv )^2 )
}

plot( alpha, SSPE.cv )


