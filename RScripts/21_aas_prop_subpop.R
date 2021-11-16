## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

# Dados dos municípios (população)
mun <- readRDS(file = here::here("dados",
                                 "MunicBR_dat.rds"))

# Criando a variável Região
mun$Regiao <- NA

mun$Regiao[mun$SiglaUF %in% c("RS", "SC", "PR")] <- "Sul"
mun$Regiao[mun$SiglaUF %in% c("SP", "MG", "RJ", "ES")] <- "Sudeste"
mun$Regiao[mun$SiglaUF %in% c("MS", "MT", "GO", "DF")] <- "Centro-Oeste"
mun$Regiao[mun$SiglaUF %in% c("RO", "AC", "AM", "PA", "TO", "RR", "AP")] <- "Norte"
mun$Regiao[mun$SiglaUF %in% c("BA", "SE", "AL", "PE", "PB", "PI", "MA", "CE", "RN")] <- "Nordeste"

mun$Regiao <- factor(mun$Regiao)



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

# Sorteio da amostra
set.seed(2810)

cod_amostra <- sample(x = mun$CodMunic,
                      size = 300,
                      replace = F)

mun_amostra <- mun[which(mun$CodMunic %in% cod_amostra),]

# Criando a variável Pop < 10 mil hab.
mun_amostra$Pop_menor_10 <- ifelse(mun_amostra$Pop < 10000, 1, 0)

# cpf
mun_amostra$cpf <- length(mun$CodMunic)




## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

mean(mun_amostra$Pop_menor_10)
by(data = mun_amostra$Pop_menor_10,
   INDICES = mun_amostra$Regiao,
   FUN = mean)



## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

library(survey)

mun_des <- svydesign(ids = ~1,
                     fpc = ~cpf,
                     data = mun_amostra)


svyby(formula = ~Pop_menor_10,
      by = ~Regiao,
      design = mun_des,
      FUN = svyciprop)

svyby(formula = ~Pop_menor_10,
      by = ~Regiao,
      design = mun_des,
      FUN = svyciprop, vartype = "ci")
