# 
# h2oSimplePerformance <- function(current.Model,data,strTatget="target",measure="F1",groups=10)
# {
#   require(data.table)
#   scores <- h2o.predict(object = current.Model, newdata=data )[,3]
#   
#   data.perf <- h2o.performance(scores, data[,strTatget], measure=measure, thresholds= seq(0,1,1/groups))
# 
#   df <- data.table(t(unlist(data.perf@model)))
#   df[,grep("confusion",colnames(df)):=NULL]
#   setnames(df,colnames(df), paste0("train_",colnames(df)))
#   response <- list()
#   response$df <- df
#   response$data.perf<-data.perf
#   response
# }

# prova <- h2oSimplePerformance( model[[1]],train_hex,strTatget=response,measure="F1",groups=10 )
# prova
# prova$df
# prova$data.perf


modelGrafic <- function(current.Model, 
                        data.train, 
                        data.validate = NULL,
                        data.test = NULL,
                        strTatget = "target",
                        groups=10, 
                        percents=FALSE, 
                        measure="F1",
                        plot = c("roc","gain","importance")
                        )
{

  #h2o models suported:h2o.randomForest, h2o.gbm 
  #h2o models pending:h2o.glm, h2o.deeplearning 
  #h2o incompativle reserche models :h2o.kmeans, h2o.prcomp
  
  require(ggplot2)
  require(data.table)
  require(h2o)
  
#   train.perf.simple <- h2oSimplePerformance(current.Model,data.train,strTatget=strTatget,measure=measure,groups=groups )
#   df.train <- train.perf.simple$df
#   data.train.perf <- train.perf.simple$data.perf
    
  scores.train <- h2o.predict(object = current.Model, newdata=data.train )[,3]
  
  data.train.perf <- h2o.performance(scores.train, data.train[,strTatget], measure=measure, thresholds= quantile.H2OParsedData(scores.train,probs = seq(0, 1,1/groups)))
  
  df.train <- data.table(t(unlist(data.train.perf@model)))
  df.train[,grep("confusion",colnames(df.train)):=NULL]
  setnames(df.train,colnames(df.train), paste0("train_",colnames(df.train)))

#-------data.validate-------------
  if(!is.null(data.validate))
  {
#     validate.perf.simple <- h2oSimplePerformance(current.Model,data.validate,strTatget=strTatget,measure=measure,groups=groups )
#     df.validate <- validate.perf.simple$df
#     data.validate.perf <- validate.perf.simple$data.perf
    scores.validate <- h2o.predict(object = current.Model, newdata =data.validate )[,3]

    data.validate.perf <- h2o.performance(scores.validate,data.validate[,strTatget], measure=measure, thresholds= seq(0,1,1/groups))
    
    df.validate <- data.table(t(unlist(data.validate.perf@model)))
    df.validate[,grep("confusion",colnames(df.validate)):=NULL]
    setnames(df.validate,colnames(df.validate), paste0("validate_",colnames(df.validate)))
  }
  else{
    df.validate<-"none"
  }
#-------data.test-------------
  if(!is.null(data.test))
  {
#     test.perf.simple <- h2oSimplePerformance(current.Model,data.test,strTatget=strTatget,measure=measure,groups=groups )
#     df.test <- test.perf.simple$df
#     data.test.perf <- test.perf.simple$data.perf
    
    scores.test <- h2o.predict(object = current.Model, newdata =data.test )[,3]
    data.test.perf <- h2o.performance(scores.test,data.test[,strTatget], measure=measure, thresholds= seq(0,1,1/groups))
    
    df.test <- data.table(t(unlist(data.test.perf@model)))
    df.test[,grep("confusion",colnames(df.test)):=NULL]
    setnames(df.test,colnames(df.test), paste0("test_",colnames(df.test)))
  }
  else{
    df.test<-"none"
  }
  

# roc curve----
if("roc" %in% plot)
{ 
  data.roc.train <- as.data.frame(cbind(TPR=data.train.perf@roc$TPR ,FPR=data.train.perf@roc$FPR))
  
  roc.curve <- ggplot()+ 
    geom_line(data=data.roc.train , aes(y=TPR,x=FPR,colour="train"))+
    ggtitle(paste0("Roc curve\n Auc : ", data.train.perf@model$auc))
#-------data.validate-------------
  if(!is.null(data.validate))
  {
    data.roc.validate <- as.data.frame(cbind(TPR=data.validate.perf@roc$TPR ,FPR=data.validate.perf@roc$FPR))
    
    roc.curve<- roc.curve+ 
                  geom_line(data=data.roc.validate , aes(y=TPR,x=FPR,colour="validate"))+
                  ggtitle(paste0("Roc curve\n train Auc : ",
                                  data.train.perf@model$auc,
                                  "\n validate Auc : ",data.validate.perf@model$auc)
      )
  }
#-------data.test-------------
  if(!is.null(data.test) & !is.null(data.validate))
  {
    data.roc.test <- as.data.frame(cbind(TPR=data.test.perf@roc$TPR ,FPR=data.test.perf@roc$FPR))
    
    roc.curve<- roc.curve+ 
                    geom_line(data=data.roc.test , aes(y=TPR,x=FPR,colour="test"))+
                    ggtitle(paste0("Roc curve\n train Auc : ",
                                   data.train.perf@model$auc,
                                   "\n validate Auc : ",data.validate.perf@model$auc,
                                   "\n test Auc : ",data.test.perf@model$auc)
               )
  }
  print( roc.curve )
}


# gain curve-----
if("gain" %in% plot)
{
  gain.curve.train <- rbind(c(0,0),data.train.perf@gains[,c(1,4)])

    if(groups==10)
    {
      gain.curve.train <- rbind(c(0,0),data.train.perf@gains[,c(1,4)])
    }else{
      gain.curve.train <- rbind(c(0,0),h2o.gains( data.train[,strTatget],scores.train,groups=groups)[,c(1,4)])
    }

  gain.curve <- ggplot()+ 
    geom_line(data=gain.curve.train,aes(y=Cume.Pct.Total.Lift,x=Quantile,colour="train"))+
    ggtitle("Cumulative gain curve")
  
#-------data.validate-------------
            if(!is.null(data.validate))
            {
              if(groups==10)
              {
                gain.curve.validate <- rbind(c(0,0),data.validate.perf@gains[,c(1,4)])
              }else{
                h2o_gains.validate <- h2o.gains( data.validate[,strTatget],scores.validate,groups=groups)
                gain.curve.validate <- rbind(c(0,0),h2o_gains.validate[,c(1,4)])
              }
              gain.curve<- gain.curve  + geom_line(data=gain.curve.validate,aes(y=Cume.Pct.Total.Lift,x=Quantile,colour="validate"))
            }
#-------data.test-------------
            if(!is.null(data.test) & !is.null(data.validate))
            {
              if(groups==10)
              {
                gain.curve.test <- rbind(c(0,0),data.test.perf@gains[,c(1,4)])
              }else{
                h2o_gains.test <- h2o.gains( data.test[,strTatget],scores.test,groups=groups)
                gain.curve.test <- rbind(c(0,0),h2o_gains.test[,c(1,4)])
              }
              gain.curve<- gain.curve  + geom_line(data=gain.curve.test,aes(y=Cume.Pct.Total.Lift,x=Quantile,colour="test"))
            }
  print( gain.curve )            
}


# importance vars-------
if ("importance" %in% plot)
{
  if( !is.null(current.Model@model$varimp))
  {
        if(class(current.Model)=="H2OGBMModel")
    {
          var.inf.train <- data.frame(Influence=current.Model@model$varimp$Percent.Influence,var=rownames(current.Model@model$varimp))
          var.inf.train = var.inf.train[with(var.inf.train, order(var.inf.train$Influence)), ]
          var.inf.train = var.inf.train[var.inf.train$Influence!=0, ]
          var.inf.train$var <- factor(var.inf.train$var, levels = var.inf.train$var, ordered = TRUE)
    }
    if(class(current.Model)=="H2OSpeeDRFModel")
    {
      var.inf.train <-as.data.frame(t(current.Model@model$varimp)[,1])
      var.inf.train$var <- rownames(var.inf.train)
      colnames(var.inf.train) <- c("Influence","var")
        
      var.inf.train = var.inf.train[with(var.inf.train, order(var.inf.train$Influence)), ]
      var.inf.train = var.inf.train[var.inf.train$Influence!=0, ]
      var.inf.train$var <- factor(var.inf.train$var, levels = var.inf.train$var, ordered = TRUE)
    }

    var.inf <- ggplot() +
      geom_bar(data=var.inf.train, aes(x=var, y=Influence), binwidth = 0.05, fill=rgb(0.85,0.72,0.2,0.5), color="black", stat="identity") +
      coord_flip() +
      ggtitle("Model") +
      ylab("Relative importance") +
      xlab("Variables")
    
    print( var.inf )
  }
}

#------------------------------------------
  if (class(current.Model)=="H2OGBMModel")
  {
      dt.result <- data.table(model          = class(current.Model)[1],
                              distribution   = current.Model@model$params$distribution,
                              n.trees        = current.Model@model$params$n.trees,
                              interaction.depth=current.Model@model$params$interaction.depth,
                              shrinkage      = current.Model@model$params$shrinkage,
                              n.minobsinnode =current.Model@model$params$n.minobsinnode,
                              mtries         = "-",
                              n.bins         = current.Model@model$params$n.bins,
                              nfolds         = current.Model@model$params$nfolds,
                              df.train,
                              df.validate,
                              df.test
                              )
  }

  if (class(current.Model)=="H2OSpeeDRFModel")
  {
    dt.result <- data.table(model          = class(current.Model)[1],
                            distribution   = current.Model@model$params$type,
                            n.trees        = current.Model@model$params$ntree,
                            interaction.depth=current.Model@model$params$depth,
                            shrinkage      = "-",
                            n.minobsinnode = "-",
                            mtries         = current.Model@model$params$mtries,
                            n.bins         = current.Model@model$params$nbins,
                            nfolds         = current.Model@model$params$nfolds,
                            df.train,
                            df.validate,
                            df.test
                            )
  }

#------------------------------------------
return(dt.result)

}