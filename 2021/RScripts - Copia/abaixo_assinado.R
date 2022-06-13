N <- 676
n <- 50
id <- 1:n
yi <- c(3, 4, 5, 6, 7, 9, 10, 11, 14, 15, 16, 19, 23, 27, 29, 32, 36, 41, 42)
ni <- c(1, 1, 2, 3, 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 1, 1, 4, 23)
y <- rep(x = yi, times = ni)
df <- data.frame(id, y, cpf)
head(df)

library(survey)

aas <- svydesign(ids = ~id, fpc = ~cpf, data = df)
svytotal(~y, aas)
confint(svytotal(~y, aas), level = 0.8)
confint(svytotal(~y, aas), level = 0.8, df = survey::degf(aas))

f <- n/N
s <- 228.9833
ep <- ((N * sqrt(s))/sqrt(n)) * sqrt(1 - f)
round(19887.92 + c(-1, 1) * qnorm(0.9) * ep, 0)

19888 + c(-1, 1) * (1.28*(676*15.13*sqrt(1 - 0.0740))/sqrt(50)

