##### 
hypmeans <- c(100, 75)
a <- length(hypmeans)
n <- 17
N <- a*n
dferror <- N - a
sigma <- 25
alpha <- 0.05
Y <- rep(hypmeans, each = n)
treat <- factor(rep(LETTERS[c(2, 1)], each = n))
summary(aov(Y ~ treat))
summary(aov(Y ~ treat))[[1]][1, 2] -> SShyp
(lambda <- SShyp/sigma^2)

(fst <- qf(1 - alpha, a-1, N-a))
(power <- pf(fst, a-1, N-a, lambda, lower = FALSE))
