## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='70%'------------

library(ggplot2)

data("diamonds")

# y <- trees$Volume

y <- diamonds$carat
N <- length(y)
hist(y, probability = T,
     main = "",
     xlab = "Y",
     ylab = "Densidade",
     border = "white",
     col = "#1B9E77",
     breaks = 30)




## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%'-----------

b <- 10000

n <- 1

ybar <- numeric(0)

for (k in 1:b){s <- sample(1:N, n); ybar[k] <- mean(y[s])}

hist(ybar, probability = T,
     main = "",
     xlab = expression(bar(y)),
     ylab = "Densidade",
     border = "white",
     col = "#D95F02", breaks = 30)

legend("topright", legend = "n = 1", bty = "n")

enes <- c(rep(1, b),
          rep(5, b),
          rep(20, b),
          rep(50, b),
          rep(100, b),
          rep(1000, b))
ybares <- ybar



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%'-----------

n <- 5

for (k in 1:b){s <- sample(1:N, n); ybar[k] <- mean(y[s])}

hist(ybar, probability = T,
     main = "",
     xlab = expression(bar(y)),
     ylab = "Densidade",
     border = "white",
     col = "#D95F02", breaks = 30)

legend("topright", legend = "n = 5", bty = "n")

ybares <- c(ybares, ybar)



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%'-----------

n <- 20

for (k in 1:b){s <- sample(1:N, n); ybar[k] <- mean(y[s])}

hist(ybar, probability = T,
     main = "",
     xlab = expression(bar(y)),
     ylab = "Densidade",
     border = "white",
     col = "#D95F02", breaks = 30)

legend("topright", legend = "n = 20", bty = "n")

ybares <- c(ybares, ybar)



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%'-----------

n <- 50

for (k in 1:b){s <- sample(1:N, n); ybar[k] <- mean(y[s])}

hist(ybar, probability = T,
     main = "",
     xlab = expression(bar(y)),
     ylab = "Densidade",
     border = "white",
     col = "#D95F02", breaks = 30)

legend("topright", legend = "n = 50", bty = "n")

ybares <- c(ybares, ybar)



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%'-----------

n <- 100

for (k in 1:b){s <- sample(1:N, n); ybar[k] <- mean(y[s])}

hist(ybar, probability = T,
     main = "",
     xlab = expression(bar(y)),
     ylab = "Densidade",
     border = "white",
     col = "#D95F02", breaks = 30)

legend("topright", legend = "n = 100", bty = "n")

ybares <- c(ybares, ybar)



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%'-----------

n <- 1000

for (k in 1:b){s <- sample(1:N, n); ybar[k] <- mean(y[s])}

hist(ybar, probability = T,
     main = "",
     xlab = expression(bar(y)),
     ylab = "Densidade",
     border = "white",
     col = "#D95F02", breaks = 30)

legend("topright", legend = "n = 1000", bty = "n")

ybares <- c(ybares, ybar)



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%'-----------

df.joy <- data.frame(enes, ybares)
df.joy$y.bar.center <- (df.joy$ybares - mean(y))/sqrt(var(y)*(1 - df.joy$enes/N)*(1/df.joy$enes))
df.joy$enes.char <- as.factor(df.joy$enes)

library(ggjoy)
p <- ggplot(data = df.joy,
            mapping = aes(x = y.bar.center, y = enes.char, fill = enes.char)) + 
  geom_joy(alpha = 0.7) + 
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  scale_fill_brewer(palette = "Greys") +
  theme_joy() +
  labs(x = "z", y = "n", caption = "Processo centralizado.") +
  theme(legend.position = "none")
p




## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'aula-revisao', 'tcl-01.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'aula-revisao', 'tcl-02.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='70%', out.height='90%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'aula-revisao', 'tcl-03.png'))


