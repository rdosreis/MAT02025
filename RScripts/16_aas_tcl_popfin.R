## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='60%'--------------

y <- trees$Volume
N <- length(y)
hist(y, probability = T,
     main = "",
     xlab = "Y",
     ylab = "Densidade",
     border = "white",
     col = "#1B9E77")




## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%'-------------

ybar <- numeric(0)

b <- 10000
n <- 1

for (k in 1:b){s <- sample(1:N, n); ybar[k] <- mean(y[s])}

hist(ybar, probability = T,
     main = "",
     xlab = expression(bar(y)),
     ylab = "Densidade",
     border = "white",
     col = "#D95F02",
     xlim = c(10,80))

legend("topright", legend = "n = 1", bty = "n")



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%'-------------

n <- 2

for (k in 1:b){s <- sample(1:N, n); ybar[k] <- mean(y[s])}

hist(ybar, probability = T,
     main = "",
     xlab = expression(bar(y)),
     ylab = "Densidade",
     border = "white",
     col = "#D95F02",
     xlim = c(10,80))

legend("topright", legend = "n = 2", bty = "n")



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%'-------------

n <- 5

for (k in 1:b){s <- sample(1:N, n); ybar[k] <- mean(y[s])}

hist(ybar, probability = T,
     main = "",
     xlab = expression(bar(y)),
     ylab = "Densidade",
     border = "white",
     col = "#D95F02",
     xlim = c(10,80))

legend("topright", legend = "n = 5", bty = "n")



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%'-------------

n <- 15

for (k in 1:b){s <- sample(1:N, n); ybar[k] <- mean(y[s])}

hist(ybar, probability = T,
     main = "",
     xlab = expression(bar(y)),
     ylab = "Densidade",
     border = "white",
     col = "#D95F02",
     xlim = c(10,80))

legend("topright", legend = "n = 15", bty = "n")



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%'-------------

n <- 25

for (k in 1:b){s <- sample(1:N, n); ybar[k] <- mean(y[s])}

hist(ybar, probability = T,
     main = "",
     xlab = expression(bar(y)),
     ylab = "Densidade",
     border = "white",
     col = "#D95F02",
     xlim = c(10,80))

legend("topright", legend = "n = 25", bty = "n")



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%'-------------

n <- 30

for (k in 1:b){s <- sample(1:N, n); ybar[k] <- mean(y[s])}

hist(ybar, probability = T,
     main = "",
     xlab = expression(bar(y)),
     ylab = "Densidade",
     border = "white",
     col = "#D95F02",
     xlim = c(10,80))

legend("topright", legend = "n = 30", bty = "n")



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'aula-revisao', 'tcl-01.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'aula-revisao', 'tcl-02.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='70%', out.height='90%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'aula-revisao', 'tcl-03.png'))


