## ---- echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE-------------------------------------

df <- data.frame(elementos = LETTERS[1:6], ind = 1:6, idade = c(2,4,6,8,10,12))

knitr::kable(x = df,
             format = "markdown",
             col.names = c("Elementos", "$i$", "$X_i$"),
             align = "ccc")



## ---- echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE-------------------------------------

df2 <- data.frame(numero = c(1:5, "$\\vdots$"),
                  amostra = c(rep("",6)),
                  p = c(rep("",6)),
                  x = c(rep("",6)),
                  xbarra = c(rep("",6)))

knitr::kable(x = df2,
             format = "markdown",
             col.names = c("$S_j$",
                           "Amostras",
                           "$\\pi_j$",
                           "$(x_1, x_2)$",
                           "$\\bar{x}_j$"),
             align = "ccccc")



## ---- echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE-------------------------------------

x <- expand.grid(rep(list(LETTERS[1:6]), 2))

idade <- c(2,4,6,8,10,12)
idade.pares <- expand.grid(rep(list(idade), 2))
idade.media <- apply(X = idade.pares, MARGIN = 1, FUN = mean)

df3 <- data.frame(numero = 1:36,
                  amostra = paste("(", x$Var1, ", ", x$Var2, ")", sep = ""),
                  p = rep(as.character(MASS::fractions(1/36)), 36),
                  xs = paste("(", idade.pares$Var1, ", ", idade.pares$Var2, ")", sep = ""),
                  xbarra = idade.media)

knitr::kable(x = df3,
             format = "markdown",
             col.names = c("$S_j$",
                           "Amostras",
                           "$\\pi_j$",
                           "$(x_1, x_2)$",
                           "$\\bar{x}_j$"),
             align = "ccccc")



## ---- echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

barplot(table(df3$xbarra),
     col = "steelblue",
     border = "white",
     ylab = "Frequência",
     xlab = expression(bar(x)))



## ---- echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

x <- expand.grid(rep(list(LETTERS), 4))

idade <- 1:26 * 2
idade.pares <- expand.grid(rep(list(idade), 4))
idade.media <- apply(X = idade.pares, MARGIN = 1, FUN = mean)

hist(idade.media,
     prob = T,
     main = "",
     col = "steelblue",
     border = "white",
     ylab = "Densidade",
     xlab = expression(bar(x)))

curve(expr = dnorm(x, mean = mean(idade.media), sd = sd(idade.media)),
      lty = 2, lwd = 2,
      col = "red",
      add = T)



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', out.height='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Statistically-Insignificant-bolha.jpg'))


