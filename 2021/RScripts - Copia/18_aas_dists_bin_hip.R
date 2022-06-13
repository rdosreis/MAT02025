## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'------------

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



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'------------

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



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'------------

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



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'------------

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





## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'------------

plot(dbinom(x = 0:50, size = 50, prob = 0.2),
     type = "h",
     lwd = 2,
     col = "red",
     xlab = "a",
     ylab = "Pr(a)")
legend("topright", legend = c("n = 50", "P = 0.2"), bty = "n")



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='80%'-------------

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





## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'------------

plot(dhyper(k = 50, m = 200, n = 800, x = 0:50),
     type = "h",
     lwd = 2,
     col = "red",
     xlab = "a",
     ylab = "Pr(a, a'| A, A')")
legend("topright",
       legend = c("n = 50", "P = 0.2", "N = 1000", "A = 200", "A' = 800"),
       bty = "n")



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'------------

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


## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'------------

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



## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'------------

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


