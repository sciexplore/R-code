# Ordinary Least Squares Regression
linmod <- function(X, y, f=as.formula("y ~ .")){
  df = data.frame(X, y)
  linmod <- lm(f ,data=df)
  return(linmod)
}

# General linear model for binomial outcome
logreg <- function(X, y, f=as.formula("y ~ .")){
  df <- data.frame(X, y)
  logreg <- glm(f, df, family=binomial)   
  return(logreg)
}

gauss_glmnet <- function(X, y){ 
  
  df <- data.frame(X, y)
  gauss_glmnet <- cv.glmnet(x=df, y, family = "gaussian", parallel=TRUE)
  return(gauss_glmnet)
}

binomial_glmnet <- function(X, y){
   y <- outcome
   binomial_glmnet <- cv.glmnet(x=df, y, family = "binomial",parallel=TRUE)
   return(binomial_glmnet)
}
sqrtgauss_glmnet <- function(X, y){
  sqrty <- as.matrix(sqrt(y))
  df <- data.frame(X, sqrty)
  gauss_glmnet <- cv.glmnet(df, sqrty, family = "gaussian", parallel=TRUE)
  return(gauss_glmnet)
}

loggauss_glmnet <- function(X,y){
  logy <- as.matrix(log(y+1))
  df <- data.frame(X, logy)
  gauss_glmnet <- cv.glmnet(df, logy, family = "gaussian", parallel=TRUE)
  return(gauss_glmnet)
}

log10gauss_glmnet <- function(X, y){ 
  log10y <- as.matrix(log10(y+1))
  df <- data.frame(X,log10y) 
  gauss_glmnet <- cv.glmnet(df, log10y, family = "gaussian",parallel=FALSE)
  return(gauss_glmnet) 
}

pois_glmnet <- function(X, y){
  df <- data.frame(X, y) 
  pois_glmnet <- cv.glmnet(df, y,family = "poisson",parallel=TRUE)
  return(pois_glmnet)
}

# Recursive Partitioning
rpout <- function(X, y, f=as.formula("y ~ .")){
  df <- as.data.frame(x)
  rpout = rpart(f, data = df, control=rpart.control(
    minbucket=2, cp=0.0001, maxdepth=5, maxsurrogate = 0)) 
  return(rpout)
}

# Random Forests
rf <- function(X, y){
  y <- as.matrix(y)
  df <- as.matrix(cbind(X,y))
  rf <- randomForest(x=df, y=y, ntree=ntree, mtry=20, 
                     nodesize=1000, sampsize=500000)
  return(rf)
}
parallel_rf <- function(X, y, ncore=1){
  y <- as.matrix(y)
  df <- as.matrix(cbind(X,y))
  rf <- foreach(ntree=rep(round(200/ncore), ncore), 
                .combine=randomForest::combine, .multicombine=TRUE, 
                .packages='randomForest') %dopar% {
                randomForest(x=df, y=y, ntree=ntree,
                             mtry=20, nodesize=1000, sampsize=500000)}
  return(rf)
}

binomial_rf <- function(X, y, ncore=1){
  y <- as.factor(outcome)
  df <- as.matrix(cbind(X, y))
  rf <- foreach(ntree=rep(round(200/ncore), ncore), 
                .combine=randomForest::combine, .multicombine=TRUE, 
                .packages='randomForest') %dopar% {
                   randomForest(x=df, y=y, ntree=ntree,
                                mtry=20, nodesize=1000, sampsize=500000)}
  return(rf)
}

# GBM
gauss_gbm <- function(X, y, f=as.formula("y ~ .")){

  df = data.frame(cbind(X,y))
  gauss_gbm <- gbm(f, data = df, n.cores=ncore, cv.folds=5, 
                   distribution='gaussian', n.trees=100, 
                   shrinkage=0.05, interaction.depth=2, n.minobsinnode=10)
  return(gauss_gbm)
}

pois_gbm <- function(X, y, f=as.formula("y ~ .")){
  df = data.frame(cbind(X, y))
  pois_gbm <- gbm(f, data = df,n.cores=ncore, cv.folds=5,
                  distribution='poisson', n.trees=100, 
                  shrinkage=0.05, interaction.depth=2, n.minobsinnode=10)
  return(pois_gbm)
}



