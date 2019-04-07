
# to put plots side by side:
op <- par(mfrow = c(2, 2),
            pty = "s")

# Normal:
x_norm <- 1:10000
normal <-  rnorm(x_norm, 0, 1)
plot(density(normal, bw = 0.5))

# Students t test:
x <- -10:10
t_test1 <- function(x, nu){
  (1/(sqrt(nu*pi))) * ((gamma((nu+1)/2)) / gamma(nu/2)) *((x^2)/nu + 1)^(-(nu+1)/2)
}

par(op)


d1 <- density(normal, bw = 0.5)
d2 <- density(t_test1(x, 1))
d3 <- density(t_test1(x, 5))
d4 <- density(t_test1(x, 15))

d5 <- dt(x, 3)
plot(d2)

plot(range(d1$x, d2$x, d3$x, d4$x), 
     range(d1$y, d2$y, d3$y, d4$y), 
     type = "n", 
     xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue")
lines(d3, col = "black") 
lines(d5, col = "orange") 

