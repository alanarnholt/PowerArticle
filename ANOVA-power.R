library(ggplot2)
powerg <- function(mu_vec = c(5, 8, 2), n_vec = c(25, 25, 25), sig = 5, alpha = 0.05)
{
  
  #n_vec <- unlist(strsplit(nvec, ","))
  #mu_vec <- unlist(strsplit(muvec, ","))
  
  #mu_vec <- as.numeric(mu_vec)
  g <- length(mu_vec)
  #n_vec <- as.numeric(n_vec)
  N <- sum(n_vec) # Total sample size
  
  mu <- sum(n_vec*mu_vec)/N # grand mean
  
  lambda <- sum(n_vec*((mu_vec - mu)^2))/sig^2 # Alan changed
  
  
  
  
  # if(gamma < 0){
  #   ll <- -2*sqrt(((N)/(N - g))) + gamma*2
  #   ul <- 10*sqrt(((N)/(N - g)))
  # } else if (gamma >= 0) { 
  #   ll <- -2*sqrt(((N)/(N - g)))
  #   ul <- 20*sqrt(((N)/(N - g))) + gamma*2
  # } 
  
  ll <- 0       # Alan changed
  ul <- qf(.99, g-1, N-g, lambda) # Alan changed
  
  p <- ggplot(data = data.frame(x = c(ll, ul)), aes(x = x))
  
  # alternative <- match.arg(alternative) # Remove later
  
  df_fun1 <- function(x){
    y <- df(x, g - 1, N - g)
    y[x < qf(1 - alpha, g - 1, N - g)] <- NA
    return(y)
  }
  
  df_fun2 <- function(x){
    y <- df(x, g - 1, N - g, lambda)
    y[x < qf(1 - alpha, g - 1, N - g)] <- NA
    return(y)
  } 
  
  df_fun3 <- function(x){
    y <- df(x, g - 1, N - g, lambda)
    y[x > qf(1 - alpha, g - 1, N - g)] <- NA
    return(y)
  }
  
  POWER <- round(pf(qf(1-alpha,  g - 1, N - g),  g - 1, N - g, ncp = lambda, lower.tail = FALSE), 4)
  BETA <- round(1- POWER, 4)
  
  
  #}
  
  
  
  
  p + stat_function(fun = df_fun1, geom = "area", n = 1500, fill = "red", alpha = 0.95) +
    stat_function(fun = df_fun2, geom = "area", n = 1500, fill = "blue", alpha = 0.4) +
    stat_function(fun = df_fun3, geom = "area", n = 1500, fill = "green", alpha = 0.5) +
    stat_function(fun = df, args = list(g - 1, N - g), n = 1500, color = "red") +
    stat_function(fun = df, args = list(g - 1, N - g, lambda), n = 1500, color = "blue") +
    geom_hline(yintercept = 0) +
    theme_bw(base_size = 16) +
    labs(x = "", y = "", 
         title = paste0("Power ","(",POWER,") is the sum of all blue and purple shaded areas"),
         subtitle = "Null distribution is the red density - True distribution is the blue density",
         caption = paste0("Beta ","(",BETA,") is the green shaded area.", " Lambda ", "(",lambda,") is the statistical difference.")) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(plot.subtitle = element_text(hjust = 0.5)) + 
    theme(plot.caption = element_text(hjust = 0.5)) +
    annotate("text", qf(.85, g - 1, N - g), df(qf(.85, g - 1, N - g), g - 1, N - g) + .2, label = expression(F["a, N-a"]), color = "red", parse = TRUE) + 
    annotate("text", qf(.5, g - 1, N - g, lambda), df(qf(.5, g - 1, N - g, lambda), g - 1, N - g, lambda) + .05, label = expression(F["a, N-a, lambda"]), color = "blue", parse = TRUE) 
}
powerg(mu_vec = c(5, 8), n_vec = c(25, 25), sig = 5, alpha = 0.05)



powerg(mu_vec = c(10,9,8,7), n_vec = c(25,25,25,25)*2, sig = 5)

powerg(mu_vec = c(5, 8), n_vec = c(25,25), sig = 4)

lambda <- seq(0, 25, length = 200)
g <- 4
N <- 200
alpha <-  0.1
power <- pf(qf(1-alpha,  g - 1, N - g),  g - 1, N - g, ncp = lambda, lower.tail = FALSE)
plot(lambda, power, type = "l")
abline(h = c(alpha, 0.84, 1), lty = "dashed")



#####
mu_vec = c(5, 8)
n_vec = c(25, 25)
sig = 6
response <- rep(mu_vec, n_vec)
treats <- as.factor(rep(1:length(mu_vec), n_vec))
mod <- aov(response~treats)
summary(mod)
(SSH <- summary(mod)[[1]][1, 2])
(lambda <- SSH/6^2)

(N <- sum(n_vec)) # Total sample size
(mu <- sum(n_vec*mu_vec)/N) # grand mean
(gamma <- sqrt(sum(n_vec*((mu_vec - mu)^2))/sig^2))
