## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

library(survey)
data(api)

# ?apisrs
head(apisrs[,1:4])



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

(a <- table(apisrs$stype)) # a_E, a_H, a_M

(p <- prop.table(a)) # p_E, p_H, p_M



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

n <- 200
N <- 6194
f <- n/N
(q <- 1 - p) # q_E, q_H, q_M

round(ep <- sqrt( (1 - f) * ((p*q)/(n - 1)) ), 4)



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

round(li <- p - qnorm(p = 0.975) * ep , 3)
round(ls <- p + qnorm(p = 0.975) * ep + 1/(2*n), 3)



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

# O objeto design
api.des <- svydesign(id = ~1,
                     fpc = ~fpc,
                     data = apisrs)



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

# Estimativa da proporção de escolas
# do tipo "Elementary/Middle/High School"
# e intervalo de confiança de 95%
svyciprop(formula = ~I(stype == "E"),
          design = api.des, method = "mean")
svyciprop(formula = ~I(stype == "H"),
          design = api.des, method = "mean")
svyciprop(formula = ~I(stype == "M"),
          design = api.des, method = "mean")



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

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



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

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









## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

# Dados dos municípios (população)
mun_aas <- readRDS(file = here::here("dados",
                                 "MunicBR_amostra.rds"))

mun_aas



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

# Estimativa populacional
round(mean(mun_aas$Pop_menor_10), 2)

# Estimativa subpopulacionais
library(dplyr)

mun_aas %>% 
  group_by(Regiao) %>% 
  summarize(round(mean(Pop_menor_10), 2))



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

# install.packages(srvyr)
library(srvyr)

mun_des <- mun_aas %>% 
  as_survey_design(ids = 1,
                   fpc = cpf)

summary(mun_des)



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

# Estimativa populacional
mun_des %>% 
  summarize(
    Proporção = survey_mean(Pop_menor_10, vartype = "ci")) %>% 
  round(2)



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

# Estimativas subpopulacionais
mun_des %>% 
  group_by(Regiao, Pop_menor_10) %>% 
  summarize(Proporção = survey_mean()) %>% 
  mutate_at(vars(matches("Proporção")), function(x){round(x, 2)})



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

# Estimativas subpopulacionais
mun_des %>% 
  group_by(Regiao, Pop_menor_10) %>% 
  summarize(Proporção = survey_mean(),
            a = unweighted(n())) %>% 
  mutate_at(vars(matches("Proporção")), function(x){round(x, 2)})



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

# Estimativas subpopulacionais
mun_des %>% 
  group_by(Regiao, Pop_menor_10) %>% 
  summarize(Proporção = survey_mean(vartype = "ci"),
            Total = survey_total(vartype = "ci")) %>% 
  mutate_at(vars(matches("Proporção")), function(x){round(x, 2)}) %>% 
  mutate_at(vars(matches("Total")), function(x){round(x)}) %>% 
  filter(Pop_menor_10 == 1) %>% select(-Pop_menor_10) %>% knitr::kable()


