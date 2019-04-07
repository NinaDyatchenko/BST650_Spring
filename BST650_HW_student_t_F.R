
# to put plots side by side:
op <- par(mfrow = c(2, 3),
            pty = "s")

# Normal:
x_norm <- 1:10000
normal <-  rnorm(x_norm, 0, 1)
plot(density(normal, bw = 0.5))

# Students t test:
# doesn't work":
x <- c(-10:10)
t_test1 <- function(x, nu){
  (1/(sqrt(nu*pi))) * ((gamma((nu+1)/2)) / gamma(nu/2)) *((x^2)/nu + 1)^(-(nu+1)/2)
}

# doesn't work":
d1 <- density(normal, bw = 0.5)
d2 <- density(t_test1(x, 3))
d3 <- density(t_test1(x, 8))
d4 <- density(t_test1(x, 15))

d33 <- dt(x, 3)
d8 <- dt(x, 8)
d15 <- dt(x, 15)
d30 <- dt(x, 30)
d100 <- dt(x, 10000)

plot(density(normal, bw = 0.5))
plot(d8, type = "l", col = "orange")
plot(d15, type = "l", col = "red")
plot(d30, type = "l", col = "green")
plot(d33, type = "l", col = "blue")
plot(d100, type = "l", col = "gray")

plot(range(d33$x, d8$x, d30$x, d15$x, d100$x), 
     range(d33$y, d8$y, d30$y, d15$y, d100$y), 
     type = "n", 
     xlab = "x",
     ylab = "Density")
lines(d33, col = "red")
lines(d8, col = "blue")
lines(d15, col = "pink") 
lines(d30, col = "black") 
lines(d100, col = "orange") 
par(op)

