rm(list = ls())
setwd('C:/Users/UTENTE/Desktop/DM 2023/Esami')
dati = read.csv('spirale_Scurria.csv', stringsAsFactors = T)
str(dati)
# dati$Paziente = NULL
library(dplyr)
library(glmnet)
library(tree)
library(randomForest)
# righe da 17 a 45 e colonne da 23 a 53 
set.seed(932)
foldid = sample(1:6, size = nrow(dati), replace = T)
str(dati)
table(dati$Paziente)
nomi_pixel = colnames(dati)[substr(colnames(dati),1,2) == 'px']
tail(nomi_pixel)
tail(colnames(dati))
dati[,1005]
which(colSums(dati[,nomi_pixel]) == 0)
table(dati$status)
dati$Paziente = NULL

X = model.matrix(status ~ ., data = dati)
mlasso = cv.glmnet(X, dati$status, family = 'binomial', 
                   type.measure = 'class', foldid = foldid, trace.it = T)
plot(mlasso)
non_nulli = which(coef(mlasso, s = mlasso$lambda.min) != 0)
beta = colnames(X)[non_nulli][-1]

righe = substr(nomi_pixel,3,4)
tail(righe)
min(as.numeric(righe))
max(as.numeric(righe))
colonne = substr(nomi_pixel,6,7)
min(as.numeric(colonne))
max(as.numeric(colonne))

A = matrix(0, nrow = 64, ncol = 64)
B = matrix(0, nrow = 64, ncol = 64)
library(ranger)

mtry_seq = seq(10,1000, by = 10)

errori = sapply(mtry_seq, function(m){
  set.seed(329)
  ranger(status ~., data = dati, mtry = m)$prediction.error
})
which.min(errori)

mrf = ranger(status ~., data = dati, mtry = 62, importance = 'impurity')
barplot(table(mrf$variable.importance))

str(mrf$variable.importance)
which(mrf$variable.importance != 0) %>% length
rf_var = names(mrf$variable.importance[mrf$variable.importance >= 0.05])

for(b in beta){
  riga    = as.numeric(substr(b,3,4))
  colonna = as.numeric(substr(b, 6,7))
  A[riga,colonna] = 1
}
image(x = 1:64, y = 1:64, z = A, col = c('white','red'))

for(b in beta){
  riga    = as.numeric(substr(b,3,4))
  colonna = as.numeric(substr(b, 6,7))
  A[riga,colonna] = 1
}


for(v in rf_var){
  riga    = as.numeric(substr(v,3,4))
  colonna = as.numeric(substr(v, 6,7))
  B[riga,colonna] = 1
}
par(mfrow = c(1,2))
image(x = 1:64, y = 1:64, z = A, col = c('white','red'))
image(x = 1:64, y = 1:64, z = B, col = c('white','blue'))

colSums(B)




