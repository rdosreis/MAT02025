## ---- echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'------

set.seed(100)

x <- expand.grid(rep(list(LETTERS), 4))

idade <- 1:26 * 2
mu.idade <- mean(idade)
sd.muchap <- 7.5

idade.pares <- expand.grid(rep(list(idade), 4))

ind <- sample(x = 1:dim(idade.pares)[1], size = 100, replace = FALSE)
idade.amostras <- idade.pares[ind, ]

idade.media <- apply(X = idade.amostras, MARGIN = 1, FUN = mean)
intervalo.inf <- idade.media - 1.96*sd.muchap
intervalo.sup <- idade.media + 1.96*sd.muchap
vermelho <- mu.idade < intervalo.inf |  mu.idade > intervalo.sup

plot(idade.media,
     pch = 16,
     col = "lightsalmon",
     ylim = c(min(intervalo.inf), max(intervalo.sup)),
     xlab = "Amostra", ylab = "Idade média",
     main = 'IC 95% "normais" para a média de 100 amostras')
segments(x0 = 1:100, y0 = intervalo.inf,
         x1 = 1:100, y1 = intervalo.sup)
abline(h = mu.idade, lty = 2, lwd = 2, col = "steelblue")
segments(x0 = which(vermelho), y0 = intervalo.inf[which(vermelho)],
         x1 = which(vermelho), y1 = intervalo.sup[which(vermelho)], col = "red")
points(x = which(vermelho), y = idade.media[which(vermelho)], pch = 16)
legend("topright", legend = "Média populacional", lty = 2, lwd = 2, col = "steelblue", bty = "n")



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='70%', out.height='70%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'efeito_vies.png'))



## ---- echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------

f <- function(t){ 1 / sqrt(2*pi) * exp( - t^ 2/ 2) }
low <- function(b){integrate(f, lower = -Inf, upper = -1.96 - b)$value}
upp <- function(b){integrate(f, lower = 1.96 - b, upper = Inf)$value}
low.v <- Vectorize(low)
upp.v <- Vectorize(upp)

B.sigma <- c(seq(0.02,0.1, by = 0.02),
             seq(0.2, 1, by = 0.2), 1.5)
pl <- low.v(B.sigma)
pu <- upp.v(B.sigma)
pt <- low.v(B.sigma) + upp.v(B.sigma)

df <- data.frame(B.sigma, pl, pu, pt)
knitr::kable(x = df,
             format = "markdown",
             digits = c(2, 4, 4, 4),
             align = "cccc",
             col.names = c("$B/\\sigma$",
                           "$< -1,96\\sigma$",
                           "$> 1,96\\sigma$",
                           "Total"),
             caption = "Efeito do viés sobre a probabilidade de um erro maior que $1,96\\sigma$")



## ---- echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'------

plot(x = df$B.sigma,
     y = df$pt,
     type = "b",
     lwd = 2,
     col = "red",
     ylim = c(0, 0.35),
     xlab = expression(B/sigma),
     ylab = expression(paste("Probabilidade de erro ", (hat(mu) - mu) )),
     main = expression(paste("Efeito do viés sobre a probabilidade de um erro maior que 1,96", sigma) ) )
lines(x = df$B.sigma,
       y = df$pl,
       type = "b",
       lwd = 2,
       col = "darkorange")
lines(x = df$B.sigma,
      y = df$pu,
      type = "b",
      lwd = 2,
      col = "pink2")
abline(h = 0.05, lty = 2, lwd = 2, col = "darkgrey")
legend("topleft",
       legend = c(expression("< - 1.96"*sigma), expression("> 1.96"*sigma), "Total"),
       lwd = 2, col = c("darkorange", "pink2", "red"), bty = "n")


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', out.height='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Statistically-Insignificant-forecast.jpg'))


