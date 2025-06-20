rm(list = ls())
data_ = read.csv('parkinson.csv', stringsAsFactors = T)
str(data_)

library(dplyr)
library(glmnet)
library(tree)
library(randomForest)
library(ranger)


set.seed(932)
foldid = sample(1:6, size = nrow(dati), replace = T)
pixel_idx = colnames(data_)[substr(colnames(data_),1,2) == 'px']
tail(pixel_idx)
tail(colnames(data_))

which(colSums(dati[,pixel_idx]) == 0)
table(data$status)
data$Paziente = NULL

X = model.matrix(status ~ ., data = data_)
mlasso = cv.glmnet(X, data_$status, family = 'binomial', 
                   type.measure = 'class', foldid = foldid, trace.it = T)
plot(mlasso)
not_null = which(coef(mlasso, s = mlasso$lambda.min) != 0)
beta = colnames(X)[not_null][-1]

rows = substr(pixel_idx,3,4)
tail(rows)
min(as.numeric(rows))
max(as.numeric(rows))
columns_ = substr(pixel_idx,6,7)
min(as.numeric(columns_))
max(as.numeric(columns_))

A = matrix(0, nrow = 64, ncol = 64)
B = matrix(0, nrow = 64, ncol = 64)

mtry_seq = seq(10,1000, by = 10)

errors = sapply(mtry_seq, function(m){
  set.seed(329)
  ranger(status ~., data = data_, mtry = m)$prediction.error
})
which.min(errors)

mrf = ranger(status ~., data = data_, mtry = 62, importance = 'impurity')
barplot(table(mrf$variable.importance))

str(mrf$variable.importance)
which(mrf$variable.importance != 0) %>% length
rf_var = names(mrf$variable.importance[mrf$variable.importance >= 0.05])

for(b in beta){
  row    = as.numeric(substr(b,3,4))
  col = as.numeric(substr(b, 6,7))
  A[row,col] = 1
}
image(x = 1:64, y = 1:64, z = A, col = c('white','red'))

for(b in beta){
  row    = as.numeric(substr(b,3,4))
  col = as.numeric(substr(b, 6,7))
  A[row,col] = 1
}


for(v in rf_var){
  row = as.numeric(substr(v,3,4))
  col = as.numeric(substr(v, 6,7))
  B[riga,colonna] = 1
}
par(mfrow = c(1,2))
image(x = 1:64, y = 1:64, z = A, col = c('white','red'))
image(x = 1:64, y = 1:64, z = B, col = c('white','blue'))

colSums(B)




