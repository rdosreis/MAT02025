## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------

library(survey)
data(api)

# View(apisrs)
# ?apisrs

# O objeto design
api.des <- svydesign(id = ~1,
                     fpc = ~fpc,
                     data = apisrs)

# Estimativa da proporção de escolas
# do tipo "Elementary/Middle/High School"
svyciprop(formula = ~I(stype == "E"), design = api.des)
svyciprop(formula = ~I(stype == "M"), design = api.des)
svyciprop(formula = ~I(stype == "H"), design = api.des)


