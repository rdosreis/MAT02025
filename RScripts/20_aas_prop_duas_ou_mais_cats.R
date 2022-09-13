## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

library(survey)
data(api)

# ?apisrs
head(apisrs[,1:4])



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

(a <- table(apisrs$stype)) # a_E, a_H, a_M

(p <- prop.table(a)) # p_E, p_H, p_M



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

n <- 200
N <- 6194
f <- n/N
(q <- 1 - p) # q_E, q_H, q_M

round(ep <- sqrt( (1 - f) * ((p*q)/(n - 1)) ), 4)



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

round(li <- p - qnorm(p = 0.975) * ep , 3)
round(ls <- p + qnorm(p = 0.975) * ep + 1/(2*n), 3)



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

# O objeto design
api.des <- svydesign(id = ~1,
                     fpc = ~fpc,
                     data = apisrs)



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

# Estimativa da proporção de escolas
# do tipo "Elementary/Middle/High School"
# e intervalo de confiança de 95%
svyciprop(formula = ~I(stype == "E"),
          design = api.des, method = "mean")
svyciprop(formula = ~I(stype == "H"),
          design = api.des, method = "mean")
svyciprop(formula = ~I(stype == "M"),
          design = api.des, method = "mean")



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

# Método xlogit

svyciprop(formula = ~I(stype == "E"),
          design = api.des, method = "xlogit")
svyciprop(formula = ~I(stype == "H"),
          design = api.des, method = "xlogit")
svyciprop(formula = ~I(stype == "M"),
          design = api.des, method = "xlogit")

# Método likelihood

svyciprop(formula = ~I(stype == "E"),
          design = api.des, method = "likelihood")
svyciprop(formula = ~I(stype == "H"),
          design = api.des, method = "likelihood")
svyciprop(formula = ~I(stype == "M"),
          design = api.des, method = "likelihood")



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

# IC 90%

svyciprop(formula = ~I(stype == "E"), design = api.des,
          method = "mean", level = 0.9)
svyciprop(formula = ~I(stype == "H"), design = api.des,
          method = "xlogit", level = 0.9)
svyciprop(formula = ~I(stype == "M"), design = api.des,
          method = "xlogit", level = 0.9)

# IC 99%

svyciprop(formula = ~I(stype == "E"), design = api.des,
          method = "mean", level = 0.99)
svyciprop(formula = ~I(stype == "H"), design = api.des,
          method = "xlogit", level = 0.99)
svyciprop(formula = ~I(stype == "M"), design = api.des,
          method = "xlogit", level = 0.99)


