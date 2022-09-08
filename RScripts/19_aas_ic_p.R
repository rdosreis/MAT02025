## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='90%'----

a <- 20
n <- 50
N <- 100
A_max <- N - (n - a)

plot(phyper(q = 0:n, k = n, m = A_max, n = N - A_max),
     type = "s",
     lwd = 2,
     col = "blue",
     lty = 1,
     xlab = "a",
     ylab = expression(Pr(X <= a)),
     main = expression(paste("N = 100, n = 50, ", a[obs] == 20, ", " , hat(A)[S] == 70)))

abline(h = c(0.025), lty = 2)
abline(v = a, lty = 2)



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='90%'----

a <- 20
n <- 50
N <- 100
A_max <- N - (n - a) - 1

plot(phyper(q = 0:n, k = n, m = A_max, n = N - A_max),
     type = "s",
     lwd = 2,
     col = "blue",
     lty = 2,
     xlab = "a",
     ylab = expression(Pr(X <= a)),
     main = expression(paste("N = 100, n = 50, ", a[obs] == 20, ", " , hat(A)[S] == 69)))

abline(h = c(0.025), lty = 2)
abline(v = a, lty = 2)



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

a <- 20
n <- 50
N <- 100
A_max <- N - (n - a)

plot(phyper(q = 0:n, k = n, m = A_max, n = N - A_max),
     type = "s",
     lwd = 2,
     col = "blue",
     lty = 1,
     ylim = c(0,0.2),
     xlab = "a",
     ylab = expression(Pr(X <= a)),
     main = expression(paste("N = 100, n = 50, ", a[obs] == 20)))

A_max <- 65
lines(phyper(q = 0:n, k = n, m = A_max, n = N - A_max),
      type = "s",
      lwd = 2,
      lty = 2,
      col = "blue")
  
A_max <- 60
lines(phyper(q = 0:n, k = n, m = A_max, n = N - A_max),
      type = "s",
      lwd = 2,
      lty = 3,
      col = "blue")

A_max <- 55
lines(phyper(q = 0:n, k = n, m = A_max, n = N - A_max),
      type = "s",
      lwd = 2,
      lty = 4,
      col = "blue")

A_max <- 51
lines(phyper(q = 0:n, k = n, m = A_max, n = N - A_max),
      type = "s",
      lwd = 2,
      lty = 5,
      col = "blue")


y <- phyper(q = 0:a, k = n, m = A_max, n = N - A_max)
x <- seq_along(y)
y2 <- rep(y, each = 2)
y2 <- y2[-length(y2)]
x2 <- rep(x, each = 2)[-1]
x3 <- c(min(x2), x2, max(x2))
y3 <- c(0, y2, 0)

polygon(x3, y3, border = NA, col = "lightsalmon")

abline(h = 0.025, lty = 2)
abline(v = a + 1, lty = 2)


legend("topright",
       lty = 1:5,
       lwd = 2,
       col = "blue",
       legend = c(expression(hat(A)[S] == 70),
                  expression(hat(A)[S] == 65),
                  expression(hat(A)[S] == 60),
                  expression(hat(A)[S] == 55),
                  expression(hat(A)[S] == 51)),
       bty = "n")

legend("bottomright",
       fill = "lightsalmon",
       border = "white",
       legend = expression(phantom() %~~% 0.0225),
       bty = "n")



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

a <- 20
n <- 50
N <- 100
A_min <- a

plot(phyper(q = 0:n, k = n, m = A_min, n = N - A_min),
     type = "s",
     lwd = 2,
     col = "blue",
     lty = 1,
     xlab = "a",
     ylab = expression(Pr(X <= a)),
     main = expression(paste("N = 100, n = 50, ", a[obs] == 20)))

A_min <- 25
lines(phyper(q = 0:n, k = n, m = A_min, n = N - A_min),
      type = "s",
      lwd = 2,
      lty = 2,
      col = "blue")
  
A_min <- 30
lines(phyper(q = 0:n, k = n, m = A_min, n = N - A_min),
      type = "s",
      lwd = 2,
      lty = 3,
      col = "blue")

y <- phyper(q = 0:a, k = n, m = A_min, n = N - A_min)
x <- seq_along(y)
y2 <- rep(y, each = 2)
y2 <- y2[-length(y2)]
x2 <- rep(x, each = 2)[-1]
x3 <- c(min(x2), x2, max(x2))
y3 <- c(0, y2, 0)

polygon(x3, y3, border = NA, col = "lightsalmon")

abline(h = 1 - 0.025, lty = 2)
abline(v = a + 1, lty = 2)

legend("bottomright",
       lty = c(1:3, 0),
       lwd = c(2,2,2, 0),
       col = c(rep("blue", 3), NULL),
       fill = c(rep("white", 3), "lightsalmon"),
       border = rep("white", 4),
       legend = c(expression(hat(A)[I] == 20),
                  expression(hat(A)[I] == 25),
                  expression(hat(A)[I] == 30),
                  expression(phantom() %~~% 0.9922)),
       bty = "n")



## ----echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------------------------------------

# install.packages("samplingbook")
library(samplingbook)

# ?Sprop
Sprop(m = 37, # m = a 
      n = 100, N = 500, level = 0.95)



## ----echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------------------------------------

Sprop(m = 2, n = 30, # n e p pequenos
      N = 500, level = 0.95)


