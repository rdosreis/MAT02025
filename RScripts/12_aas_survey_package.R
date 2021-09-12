## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------

N <- 676
n <- 50
id <- 1:n
yi <- c(3, 4, 5, 6, 7, 9, 10, 11, 14, 15,
        16, 19, 23, 27, 29, 32, 36, 41, 42)
ni <- c(1, 1, 2, 3, 1, 1, 1, 1, 1, 2, 2,
        1, 1, 2, 1, 1, 1, 4, 23)
y <- rep(x = yi, times = ni)
df.assinatura <- data.frame(id, y, cpf = N)
head(df.assinatura)



## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------

# Estimativa da média
(Y.barra <- mean(df.assinatura$y))

# Estimativa do total
(YT.chapeu <- N * Y.barra)

# Estimativa da variância de Y
(s2 <- var(x = df.assinatura$y)) # ?var

# Estimativa do desvio padrão de Y
(s <- sd(x = df.assinatura$y)) # ?sd
# s <- sqrt(s2)

(f <- n/N) # fração de amostragem

# Estimativa do erro padrão do total
(s.YT.chapeu <- ((N * s)/sqrt(n)) * sqrt(1 - f)) 

# IC 80% para o total de assinaturas
# (utilizando a distribuição normal)
YT.chapeu + c(-1, 1) * qnorm(p = 0.9) *  s.YT.chapeu
round(YT.chapeu + c(-1, 1) * qnorm(p = 0.9) *  s.YT.chapeu)

# IC 80% para o total de assinaturas
# (utilizando a distribuição t)
round(YT.chapeu + c(-1, 1) * qt(p = 0.9, df = 49) *  s.YT.chapeu)





















## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------

# install.packages("survey")
library(survey)

# O objeto design
(ass.des <- svydesign(ids = ~1,
                     fpc = ~cpf,
                     data = df.assinatura))

summary(ass.des)

# Estimativa da média
svymean(x = ~y, design = ass.des)

# Estimativa do total
svytotal(x = ~y, design = ass.des)

# IC 80% para o total de assinaturas
# (utilizando a distribuição normal)
confint(svytotal(x = ~y, design = ass.des),
        level = 0.8)

# IC 80% para o total de assinaturas
# (utilizando a distribuição tl)
confint(svytotal(x = ~y, design = ass.des),
        level = 0.8, df = survey::degf(ass.des))

# IC 95% para o total de assinaturas
confint(svytotal(x = ~y, design = ass.des),
        level = 0.95)



## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------

data(api)

# View(apisrs)
# ?apisrs

# O objeto design
api.des <- svydesign(id = ~1,
                     fpc = ~fpc,
                     data = apisrs)

# Estimativa do total de alunos
# matriculados
svytotal(x = ~enroll, design = api.des)

# Estimativa da média de alunos
# matriculados por escola
svymean(x = ~enroll, design = api.des)



## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------

# O objeto design
api.des2 <- svydesign(id = ~1,
                     weights = ~pw,
                     data = apisrs)

# Estimativa do total de alunos
# matriculados
svytotal(x = ~enroll, design = api.des2)

# Estimativa da média de alunos
# matriculados por escola
svymean(x = ~enroll, design = api.des2)


