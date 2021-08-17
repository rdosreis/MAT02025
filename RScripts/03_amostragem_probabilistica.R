## ---- echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE-----------------------------

df <- data.frame(elementos = LETTERS[1:6], ind = 1:6, idade = c(2,4,6,8,10,12))

knitr::kable(x = df,
             format = "markdown",
             col.names = c("Elementos", "$i$", "$X_i$"),
             align = "ccc")



## ---- echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE-----------------------------

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



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', out.height='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Statistically-Insignificant-bolha.jpg'))


