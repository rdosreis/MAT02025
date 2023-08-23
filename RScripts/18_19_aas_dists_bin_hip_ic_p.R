## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'-------------------

P <- seq(from = 0, to = 100, by = 10)
Q <- 100 - P
PQ <- P*Q
sqrtPQ <- sqrt(PQ)

par(mfrow = c(1,2))
plot(P, PQ, type = "b", lwd = 2, col = "lightsalmon")
abline(h = seq(0, 2500, by = 500), v = P, col = "lightgrey", lty = 2)
plot(P, sqrtPQ, type = "b", lwd = 2, col = "lightsalmon", ylab = expression(sqrt(PQ)))
abline(h = seq(0, 50, by = 10), v = P, col = "lightgrey", lty = 2)
par(mfrow = c(1,1))



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'-------------------

n <- 1:200
P <- 50
Q <- 100 - P
ep <- sqrt((P*Q)/n)

plot(n, ep,
     type = "l",
     lwd = 2,
     col = "steelblue",
     ylab = expression(sqrt(hat(Var)(p))),
     main = "Erro padrão de p em função de n, quando P = 50%")
abline(h = seq(0, 50, by = 5), v = seq(0, 200, by = 50), col = "lightgrey", lty = 2)



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'-------------------

P <- c(0, 0.1, 0.5, 1, 5, seq(from = 10, to = 90, by = 10))
Q <- 100 - P
cv <- sqrt(Q/P)

plot(P, cv,
     type = "b",
     lwd = 2,
     col = "steelblue",
     ylab = expression(sqrt(Q/P)))
abline(h = seq(0, 30, by = 5),
       v = seq(from = 0, to = 90, by = 10),
       col = "lightgrey", lty = 2)



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'-------------------

raiz.n <- c(1:10, seq(20, 100, by = 20))
P <- 1
Q <- 100 - P
cv <- sqrt(Q/P) * (1/raiz.n)

plot(raiz.n, cv,
     type = "b",
     lwd = 2,
     col = "steelblue",
     xlab = expression(sqrt(n)),
     ylab = expression(sqrt(Q/nP)))
abline(h = seq(0, 10, by = 1),
       v = seq(from = 0, to = 100, by = 10),
       col = "lightgrey", lty = 2)





## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'-------------------

plot(dbinom(x = 0:50, size = 50, prob = 0.2),
     type = "h",
     lwd = 2,
     col = "red",
     xlab = "a",
     ylab = "Pr(a)")
legend("topright", legend = c("n = 50", "P = 0.2"), bty = "n")



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='80%'--------------------

par(mfrow = c(1,2))

plot(x = (0:50)/50,
  y = dbinom(x = 0:50, size = 50, prob = 0.2),
     type = "h",
  lwd = 2,
     col = "blue",
     xlab = "p",
     ylab = "Pr(p)")
legend("topright", legend = c("n = 50", "P = 0.2"), bty = "n")

plot(x = (1000*(0:50))/50,
     y = dbinom(x = 0:50, size = 50, prob = 0.2),
     lwd = 2,
     type = "h",
     col = "black",
     xlab = "Np",
     ylab = "Pr(Np)")
legend("topright", legend = c("n = 50", "P = 0.2", "N = 1000"), bty = "n")

par(mfrow = c(1,1))





## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'-------------------

plot(dhyper(k = 50, m = 200, n = 800, x = 0:50),
     type = "h",
     lwd = 2,
     col = "red",
     xlab = "a",
     ylab = "Pr(a, a'| A, A')")
legend("topright",
       legend = c("n = 50", "P = 0.2", "N = 1000", "A = 200", "A' = 800"),
       bty = "n")



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'-------------------

par(mfrow = c(1,2))

plot(x = (0:50)/50,
     y = dhyper(k = 50, m = 200, n = 800, x = 0:50),
     type = "h",
     lwd = 2,
     col = "blue",
     xlab = "p",
     ylab = "Pr(p)")
legend("topright",
       legend = c("n = 50", "P = 0.2", "N = 1000", "A = 200", "A' = 800"),
       bty = "n")

plot(x = (1000*(0:50))/50,
     y = dhyper(k = 50, m = 200, n = 800, x = 0:50),
     lwd = 2,
     type = "h",
     col = "black",
     xlab = "Np",
     ylab = "Pr(Np)")
legend("topright",
       legend = c("n = 50", "P = 0.2", "N = 1000", "A = 200", "A' = 800"),
       bty = "n")

par(mfrow = c(1,1))


## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'-------------------

plot(x = (0:50)/50,
     y = dhyper(k = 50, m = 200, n = 800, x = 0:50),
     type = "h",
     xlim = c(0, 0.5),
     lwd = 2,
     col = "blue",
     xlab = "p",
     ylab = "Pr(p)",
     main = "n = 50, P = 0.2, N = 1000, A = 200, A' = 800, f = 0.05")
lines(x = (0:50)/50,
      y = dbinom(x = 0:50, size = 50, prob = 0.2),
      type = "h",
      lwd = 1,
      lty = 2,
      col = "red")
legend("topright",
       lty = c(1, 2),
       lwd = c(2, 1),
       col = c("blue", "red"),
       legend = c("Hipergeométrica", "Binomial"),
       bty = "n")



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'-------------------

plot(x = (0:400)/400,
     y = dhyper(k = 400, m = 200, n = 800, x = 0:400),
     type = "h",
     lwd = 2,
     xlim = c(0.1, 0.3),
     col = "blue",
     xlab = "p",
     ylab = "Pr(p)",
     main = "n = 400, P = 0.2, N = 1000, A = 200, A' = 800, f = 0.4")
lines(x = (0:400)/400,
      y = dbinom(x = 0:400, size = 400, prob = 0.2),
      type = "h",
      lwd = 1,
      lty = 2,
      col = "red")
legend("topright",
       lty = c(1, 2),
       lwd = c(2, 1),
       col = c("blue", "red"),
       legend = c("Hipergeométrica", "Binomial"),
       bty = "n")



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='90%'--------------------

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



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='90%'--------------------

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



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'-------------------

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



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'-------------------

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



## ----echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------

# install.packages("samplingbook")
library(samplingbook)

# ?Sprop
Sprop(m = 37, # m = a 
      n = 100, N = 500, level = 0.95)



## ----echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------

Sprop(m = 2, n = 30, # n e p pequenos
      N = 500, level = 0.95)


