## ---- echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------------------------------------------

# install.packages("PracTools")
library(PracTools)

nProp(V0 = (0.05/2)^2, N = 3200, pU = 0.5)

# diferença ao usar z 'arrendondado'
nProp(V0 = (0.05/1.96)^2, N = 3200, pU = 0.5)



## ---- echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------------------------------------------

# install.packages("PracTools")
library(PracTools)

nCont(S2 = 85.6, ybarU = 19, N = 430, CV0 = (0.10/2))

# diferença ao usar z 'arrendondado'
nCont(S2 = 85.6, ybarU = 19, N = 430, CV0 = (0.10/1.96))


