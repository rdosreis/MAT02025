## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

# Dados dos municípios (população)
mun_aas <- readRDS(file = here::here("dados",
                                 "MunicBR_amostra.rds"))

mun_aas



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

# Estimativa populacional
round(mean(mun_aas$Pop_menor_10), 2)

# Estimativa subpopulacionais
library(dplyr)

mun_aas %>% 
  group_by(Regiao) %>% 
  summarize(round(mean(Pop_menor_10), 2))



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

# install.packages(srvyr)
library(srvyr)

mun_des <- mun_aas %>% 
  as_survey_design(ids = 1,
                   fpc = cpf)

summary(mun_des)



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

# Estimativa populacional
mun_des %>% 
  summarize(
    Proporção = survey_mean(Pop_menor_10, vartype = "ci")) %>% 
  round(2)



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

# Estimativas subpopulacionais
mun_des %>% 
  group_by(Regiao, Pop_menor_10) %>% 
  summarize(Proporção = survey_mean()) %>% 
  mutate_at(vars(matches("Proporção")), function(x){round(x, 2)})



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

# Estimativas subpopulacionais
mun_des %>% 
  group_by(Regiao, Pop_menor_10) %>% 
  summarize(Proporção = survey_mean(),
            a = unweighted(n())) %>% 
  mutate_at(vars(matches("Proporção")), function(x){round(x, 2)})



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

# Estimativas subpopulacionais
mun_des %>% 
  group_by(Regiao, Pop_menor_10) %>% 
  summarize(Proporção = survey_mean(vartype = "ci"),
            Total = survey_total(vartype = "ci")) %>% 
  mutate_at(vars(matches("Proporção")), function(x){round(x, 2)}) %>% 
  mutate_at(vars(matches("Total")), function(x){round(x)}) %>% 
  filter(Pop_menor_10 == 1) %>% select(-Pop_menor_10) %>% knitr::kable()


