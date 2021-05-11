# encuesta de bienestar financiero
# https://www.consumerfinance.gov/data-research/financial-well-being-survey-data/

# cargar los datos 
mydatat <-  read.csv('https://www.consumerfinance.gov/documents/5614/NFWBS_PUF_2016_data.csv')
names(mydatat)

mydata <- mydatat[c(1, 4:6, 8:13, 19:25, 151:153, 160:162)]

# check missing values and recode into NAs
summary(mydata)
is.na(mydata[ ,2:23]) <- mydata[ ,2:23] <= 0
summary(mydata)

# Cronbach's alpha
library(psych)
names(mydata)
alpha(mydata[ c(21:23) ] )

library(lavaan)
options(max.print=999999)

modeloAFC <- '
# modelo de medida 
swb =~ SWB_1 + SWB_2 + SWB_3
mat =~ MATERIALISM_1 + MATERIALISM_2 + MATERIALISM_3
selfc =~ SELFCONTROL_2 + SELFCONTROL_3
'

fitAFC <- cfa(modeloAFC, data = mydata, estimator = "MLM")
summary(fitAFC, fit.measures = TRUE)
fitMeasures(fitAFC)
inspect (fitAFC, "cor.lv")
inspect (fitAFC, "cor.ov")
standardizedSolution(fitAFC)
MI <- modificationIndices(fitAFC)
subset(MI, mi > 4)

# visualización del modelo con los parámetros estandarizados
library(semPlot)
semPaths(fitAFC, "std", weighted = FALSE, nCharNodes = 7, 
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5)

# CR and AVE with the semTools
library(semTools)
reliability(fitAFC)



# SEM 
modeloSEM <- ' 
# modelo de medida 
swb =~ SWB_1 + SWB_2 + SWB_3
mat =~ MATERIALISM_1 + MATERIALISM_2 + MATERIALISM_3
selfc =~ SELFCONTROL_2 + SELFCONTROL_3

# regresiones 
swb ~ mat + selfc
' 
fitSEM <- sem(modeloSEM, data = mydata, estimator = "MLM")
summary(fitSEM, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)
parameterEstimates(fitSEM, boot.ci.type="bca.simple")
fitMeasures(fitSEM)
inspect (fitSEM, "cor.lv")
standardizedSolution(fitSEM)
MI <- modificationIndices(fitSEM)
subset(MI, mi > 4)

# Visualización del modelo

semPaths(fitSEM, "std", layout = "tree2", weighted = FALSE, nCharNodes = 7, 
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5)

