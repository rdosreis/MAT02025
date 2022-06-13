plot(dhyper(k = 50, m = 20, n = 80, x = 0:50), type = "h", xlim = c(0, 20))
lines(dbinom(x = 0:50, size = 50, prob = 0.2), type = "h", col = "red")
lines(dnorm(x = seq(0,30), mean = 50*0.2, sd = sqrt( (50 * (0.2*0.8)) * ((1000 - 30)/(1000 - 1)) )), col = "blue")


plot(dhyper(k = 500, m = 200, n = 800, x = 0:500), type = "h", xlim = c(0, 200))
lines(dbinom(x = 0:500, size = 500, prob = 0.2), type = "h", col = "red")
lines(dnorm(x = seq(0,200), mean = 500*0.2, sd = sqrt( (500 * (0.2*0.8)) * ((1000 - 500)/(1000 - 1)) )), col = "blue")


P <- seq(from = 0, to = 100, by = 10)
Q <- 100 - P
PQ <- P*Q
sqrtPQ <- sqrt(PQ)

par(mfrow = c(1,2))
plot(P, PQ, type = "b", lwd = 2, col = "steelblue")
abline(h = seq(0, 2500, by = 500), v = P, col = "lightgrey", lty = 2)
plot(P, sqrtPQ, type = "b", lwd = 2, col = "lightsalmon", ylab = expression(sqrt(PQ)))
abline(h = seq(0, 50, by = 10), v = P, col = "lightgrey", lty = 2)
par(mfrow = c(1,1))

n <- 1:200
P <- 50
Q <- 100 - P
ep <- sqrt((P*Q)/n)

plot(n, ep, type = "l", lwd = 1, col = "steelblue")
abline(h = seq(0, 50, by = 5), v = seq(0, 200, by =), col = "lightgrey", lty = 2)


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



plot(dbinom(x = 0:50, size = 50, prob = 0.2),
     type = "h",
     lwd = 2,
     col = "red",
     xlab = "a",
     ylab = "Pr(a)")
legend("topright", legend = c("n = 50", "P = 0.2"), bty = "n")

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


plot(dhyper(k = 50, m = 200, n = 800, x = 0:50),
     type = "h",
     lwd = 2,
     col = "red",
     xlab = "a",
     ylab = "Pr(a, a'| A, A')")
legend("topright",
       legend = c("n = 50", "P = 0.2", "N = 1000", "A = 200", "A' = 800"),
       bty = "n")

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



plot(dhyper(k = 50, m = 20, n = 80, x = 0:50), type = "h", xlim = c(0, 20), lwd = 2)
lines(dbinom(x = 0:50, size = 50, prob = 0.2), type = "h", col = "red", lwd = 1)


lines(dnorm(x = seq(0,30), mean = 50*0.2, sd = sqrt( (50 * (0.2*0.8)) * ((1000 - 30)/(1000 - 1)) )), col = "blue")



a <- 20
n <- 50
N <- 1000
A <- N - (n - a)
Alinha <- N - A

plot(pbinom(q = 0:50, size = n, prob = A/N),
     type = "s",
     lwd = 2,
     col = "red",
     xlab = "a")

plot(phyper(q = 0:50, k = 50, m = A, n = Alinha),
     type = "s",
     lwd = 2,
     col = "red",
     xlab = "a")

A <- N - (n - a) - 450
Alinha <- N - A
lines(phyper(q = 0:50, k = 50, m = A, n = Alinha),
      type = "s",
      lwd = 2,
      col = "orange3",
      xlab = "a")

A <- a + 200
Alinha <- N - A

lines(phyper(q = 0:50, k = 50, m = A, n = Alinha),
     type = "s",
     lwd = 2,
     col = "blue",
     xlab = "a")
abline(h = c(0.025, 0.975), lty = 2)
abline(v = a)
     # ylab = "Pr(a, a'| A, A')")
legend("topright",
       legend = c("n = 50", "P = 0.2", "N = 1000", "A = 200", "A' = 800"),
       bty = "n")



# y <- factor(c(rep(0, 100 - 37), rep(1, 37)), labels = c("n", "s"))
y <- c(rep(0, 100 - 2), rep(1, 2))
cpf <- rep(500, 100)
df <- data.frame(y, cpf)

library(survey)

amostra <- svydesign(ids = ~1, fpc = ~cpf, data = df)
svyciprop(formula = ~I(y == 1), design = amostra, method = "xl")
