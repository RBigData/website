library(openblasctl)
openblas_set_num_threads(1) # setting to 1 for demonstration of MPI
suppressPackageStartupMessages(library(fda.usc))
data(phoneme)
learn <- phoneme$learn
l <- c(0, 2^seq(-2, 9, length.out = 30))
nb <- seq(7, 31, by = 2)
out0 <- min.basis(learn, lambda = l, numbasis = nb)
cat("Minimum GCV:", out0$gcv.opt, "\n")
