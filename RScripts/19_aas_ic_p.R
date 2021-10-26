## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'-----------

a <- 20
n <- 50
N <- 100
A <- N - (n - a)
Alinha <- N - A

plot(phyper(q = 0:50, k = 50, m = A, n = Alinha),
     type = "s",
     lwd = 2,
     col = "blue",
     lty = 2,
     xlab = "a",
     ylab = expression(Pr(X <= a)),
     main = "N = 100, n = 50, a = 20")

A <- a
Alinha <- N - A
lines(phyper(q = 0:50, k = 50, m = A, n = Alinha),
      type = "s",
      lwd = 2,
      lty = 2,
      col = "red")

A <- N*(a/n)
Alinha <- N - A
lines(phyper(q = 0:50, k = 50, m = A, n = Alinha),
      type = "s",
      lwd = 2,
      lty = 1,
      col = "black")

abline(h = c(0.025, 0.975), lty = 2)
abline(v = a)

legend("bottomright",
       lty = c(2, 1, 2),
       lwd = c(2, 2, 2),
       col = c("blue", "black","red"),
       legend = c(expression(hat(A)[I] == 70), expression(hat(A) == 40), expression(hat(A)[S] == 20)),
       bty = "n")



## ----echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

# install.packages("samplingbook")
library(samplingbook)

Sprop(m = 37, n = 100, N = 500, level = 0.95)



## ----echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

Sprop(m = 2, n = 100, N = 500, level = 0.95)


