library(phyclust, quietly = TRUE)
library(parallel)

### Load data
data.path <- paste(.libPaths()[1], "/phyclust/data/pony524.phy", sep = "")
pony.524 <- read.phylip(data.path)
X <- pony.524$org
K0 <- 1
Ka <- 2

### Find MLEs
ret.K0 <- find.best(X, K0)
ret.Ka <- find.best(X, Ka)
LRT <- -2 * (ret.Ka$logL - ret.K0$logL)

### The user defined function
FUN <- function(jid){
  X.b <- bootstrap.seq.data(ret.K0)$org

  ret.K0 <- phyclust(X.b, K0)
  repeat{
    ret.Ka <- phyclust(X.b, Ka)
    if(ret.Ka$logL > ret.K0$logL){
      break
    }
  }

  LRT.b <- -2 * (ret.Ka$logL - ret.K0$logL)
  LRT.b
}

### Task pull and summary
ret <- mclapply(1:100, FUN)
LRT.B <- unlist(ret) 
cat("K0: ", K0, "\n",
    "Ka: ", Ka, "\n",
    "logL K0: ", ret.K0$logL, "\n",
    "logL Ka: ", ret.Ka$logL, "\n",
    "LRT: ", LRT, "\n",
    "p-value: ", mean(LRT > LRT.B), "\n", sep = "")
