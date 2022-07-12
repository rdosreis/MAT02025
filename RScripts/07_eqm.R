## ---- echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE---------------------------

f <- function(t){ 1 / sqrt(2*pi) * exp( - t^ 2/ 2) }

low <- function(b){integrate(f, lower = -Inf, upper = -sqrt(1 + b^2) - b)$value}
upp <- function(b){integrate(f, lower = sqrt(1 + b^2) - b, upper = Inf)$value}
low.1 <- Vectorize(low)
upp.1 <- Vectorize(upp)

low <- function(b){integrate(f, lower = -Inf, upper = -1.96*sqrt(1 + b^2) - b)$value}
upp <- function(b){integrate(f, lower = 1.96*sqrt(1 + b^2) - b, upper = Inf)$value}
low.2 <- Vectorize(low)
upp.2 <- Vectorize(upp)

low <- function(b){integrate(f, lower = -Inf, upper = -2.576*sqrt(1 + b^2) - b)$value}
upp <- function(b){integrate(f, lower = 2.576*sqrt(1 + b^2) - b, upper = Inf)$value}
low.3 <- Vectorize(low)
upp.3 <- Vectorize(upp)


B.sigma <- c(seq(0, 0.6, by = 0.1), seq(1, 3, by = 0.5))

pt.1 <- low.1(B.sigma) + upp.1(B.sigma)
pt.2 <- low.2(B.sigma) + upp.2(B.sigma)
pt.3 <- low.3(B.sigma) + upp.3(B.sigma)

df <- data.frame(B.sigma, pt.1, pt.2, pt.3)
df[11:12,4] <- NA

options(knitr.kable.NA = '-')

knitr::kable(x = df,
             format = "markdown",
             digits = c(2, 2, 3, 4),
             align = "cccc",
             col.names = c("$B/\\sigma$",
                           "$\\geq \\sqrt{EQM}$",
                           "$\\geq 1,96\\sqrt{EQM}$",
                           "$\\geq 2,576\\sqrt{EQM}$"),
             caption = "Proporção de casos em que o valor verdadeiro, $\\mu$, não está incluído no intervalo $\\hat{\\mu} \\pm k\\sqrt{EQM}$, para diferentes níveis de viés em $\\hat{\\mu}$")



## ---- echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

B.sigma <- c(seq(0, 0.6, by = 0.1), seq(1, 3, by = 0.5))

pt.1 <- low.1(B.sigma) + upp.1(B.sigma)
pt.2 <- low.2(B.sigma) + upp.2(B.sigma)
pt.3 <- low.3(B.sigma) + upp.3(B.sigma)

df <- data.frame(B.sigma, pt.1, pt.2, pt.3)

plot(x = df$B.sigma,
     y = df$pt.1,
     type = "b",
     lwd = 2,
     col = "red",
     ylim = c(0, 0.6),
     xlab = expression(B/sigma),
     ylab = expression(paste("Probabilidade de erro ", (hat(mu) - mu) )),
     main = expression(paste("Efeito do viés sobre a probabilidade de um erro maior que ",  k*sqrt(EQM) )) )
lines(x = df$B.sigma,
      y = df$pt.2,
      type = "b",
      lwd = 2,
      col = "darkorange")
lines(x = df$B.sigma,
      y = df$pt.3,
      type = "b",
      lwd = 2,
      col = "pink2")
abline(v = 0.5, lty = 2, lwd = 2, col = "darkgrey")
abline(h = c(0.32, 0.05, 0.01), lty = 2, lwd = 2, col = "darkgrey")
legend("topright",
       legend = c(expression("">= sqrt("EQM")), expression("">= 1.96*sqrt("EQM")), expression("">= 2.576*sqrt("EQM"))),
       lwd = 2, col = c("red", "darkorange", "pink2"), bty = "n")



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='60%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'slide-aula-04.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='60%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'acuracia_precisao.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'acuracia_precisao-2.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%'----

knitr::include_graphics(here::here('images', 'lembrando_levantamento_amostra.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', out.height='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Statistically-Insignificant-errorbar.jpg'))


