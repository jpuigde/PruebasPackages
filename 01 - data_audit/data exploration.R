require(data.table)
require(ggplot2)
require(DT)
require(speedglm)
require(lars)
require(caret)


# SPEED GLM----
# install.packages("speedglm") 
require(speedglm)
dades <- fread ("../data/spring-train.csv")
importance <- fread("../data/spring-importance_for_exploration.csv")
dades <- dades[,c("ID",importance[1:15,variable],"target"),with=F]
for (i in seq_len(ncol(dades)) )  set(dades,which(is.na(dades[[i]])),i,-1)
dades$target<- as.factor(dades$target)


feature.names <- names(dades)[2:ncol(dades)-1]

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
  if (class(dades[[f]])=="character") {
    levels <- unique(dades[[f]])
    dades[[f]] <- as.integer(factor(dades[[f]], levels=levels))
  }
}

set.seed(1)
ind   <- sample(1:nrow(dades),40000)
train <- dades[ind,]
test  <- dades[-ind,]
str(train)

models <- combn(names(dades)[-c(1,ncol(dades))],2)
i=25
paste(models[,i])

class(test$target)

for(i in seq_len(dim(models)[2]))
{
  if(i==1){ total_list <- list()}
  form <- formula(paste("target ~",paste(models[,i],collapse="+")))
  submodels <- glm(form,binomial(), train)
  pred <- predict(submodels,test)
  pred <- pmax(pmin(pred,1),0)
  
  total_list[[i]] <- list(vars=models[,i],model=submodels,AUC=AUC::auc(AUC::roc(pred,as.factor(test$target))))
  print(i)
}


AUCS <- sapply(total_list,function(x){ x$AUC })
VARS <- sapply(total_list,function(x){ x$vars })
MODELS <- lapply(total_list,function(x){ x$model })
selectedVARS <- VARS[,AUCS>quantile(AUCS,0.75)]
selectedMODELS <- MODELS[AUCS>quantile(AUCS,0.75)]
i=1

par(mfrow=c(1,2))
for(i in seq_len(ncol(selectedVARS)))
{
  t1 <- preProcess(test[,selectedVARS[,i],with=F], method = c("center", "scale","BoxCox") )
  t1 <- predict(t1,test[,selectedVARS[,i],with=F])
  nrow(t1)
  class(pred)
  squash::squashgram(t1[,1],t1[,2],test$target,mean)
  pred <- predict(selectedMODELS[[i]],test)
  squash::squashgram(t1[,1],t1[,2],pred,mean)
  readline(prompt = paste(selectedVARS[,i],collapse=" ") )
}
par(mfrow=c(1,1))





