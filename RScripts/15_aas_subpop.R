## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------

library(survey)
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



## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------

# Estimativa do total de alunos
# matriculados por tipo de escola
svyby(formula = ~enroll, by = ~stype, design = api.des, FUN = svytotal)

# Estimativa da média de alunos
# matriculados por tipo de escola
svyby(formula = ~enroll, by = ~stype, design = api.des, FUN = svymean)


