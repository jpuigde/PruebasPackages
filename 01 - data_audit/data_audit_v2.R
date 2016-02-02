require(data.table)
require(ggplot2)
require(DT)
require(rbenchmark)
require(corrplot)

# SEt 1----
da.variable.types <- function(data,max.cat=50){
  
  if( !is.data.frame( data )){ stop("data mast be data.frame or data.table") }
  
#   benchmark(data[,sapply(.SD, class)],  # relative = 16.6
#             sapply(data,class)  ) # relative = 1
  numerical_ind <- sapply(data,class) %in% c("numeric","integer")
  numerical_vars <- names(data)[numerical_ind]
  
#   benchmark(data[,sapply(.SD, function(x) {length(unique(x))<50})], # relative = 1.131
#             sapply(data,function(x) {length(unique(x))<50}) ) # relative = 1
  
  cat_or_num_ind <- sapply(data,function(x) {length(unique(x)) < max.cat})
  categorical_vars <- names(data)[!numerical_ind | cat_or_num_ind]
  
  return(list(numerical_vars=numerical_vars,categorical_vars=categorical_vars))
  
}

da.correlation.matrix <-function(data){
  if( !is.data.frame( data )){ stop("data mast be data.frame or data.table") }
  data <- as.data.table(data)
  type_vars <- da.variable.types(data=data)
  
  corrplot(cor(data[,type_vars$numerical_vars,with=F]))
}

da.in_out.categories <- function(data1,data2,max.cat=50,set.names = c("data1","data2")){
  if( all( names(data1) != names(data2) ) ) { stop("names() or data1 and data2 must be the same ")}
  type_vars1 <- da.variable.types(data1,max.cat)
  type_vars2 <- da.variable.types(data2,max.cat)
  if( all( type_vars1$categorical_vars != type_vars2$categorical_vars ) ) { stop("categorical types of data1 and data2 do not mach, try to incres or decrese max.cat ")}
  
  
  values_of_categorical_data1 <- lapply(data1[,type_vars1$categorical_vars,with=F],unique)
  values_of_categorical_data2 <- lapply(data2[,type_vars2$categorical_vars,with=F],unique)
  
  vars_in <- list()
  vars_out <- list()
  
  for(var in names(values_of_categorical_data1) )
  {
    vars_out[[var]] <- setdiff(values_of_categorical_data1[[var]],values_of_categorical_data2[[var]])
    vars_in[[var]] <- setdiff(values_of_categorical_data2[[var]],values_of_categorical_data1[[var]])
  }
  
  return(list(vars_in=vars_in,vars_out=vars_out))
}

da.plot.in_out.categories <- function(in_out_list){
  lengths_in <- sapply( in_out_list$vars_in,length)
  lengths_out <- sapply( in_out_list$vars_out,length)
  lengths_df <- rbind(cbind(var=names(lengths_in)   , length = lengths_in  , set = "out"),
                      cbind(var=names(lengths_out ) , length = lengths_out , set = "in" ))
  rownames(lengths_df) <- NULL
  lengths_df <- data.frame(lengths_df,stringsAsFactors = F)
  lengths_df$color <- factor(as.numeric(lengths_df$length) > 0)
  levels(lengths_df$color) <- c("same","more")
  
  ggplot( lengths_df ) +
    geom_tile( aes( x = set , y = var , fill  = color ) , alpha = 0.4)  +
    geom_text( aes( x = set , y = var , label = length ) )+
    scale_fill_manual(values=c("green","red"))
}

da.numeric.summary.comparation <- function(data1,data2,set.names=c("data1","data2"),fun_list=NULL,ignored_variables=NULL,max.cat=50){
  if( all( names(data1) != names(data2) ) ) { stop("names() or data1 and data2 must be the same ")}
  type_vars1 <- da.variable.types(data1,max.cat)
  type_vars2 <- da.variable.types(data2,max.cat)
  if( all( type_vars1$numerical_vars   != type_vars2$numerical_vars   ) ) { stop("numeric types of data1 and data2 must me equal ")}
  
  total_data = rbind(cbind(data1[,type_vars1$numerical_vars,with=F],data.table(set=set.names[1])),
                     cbind(data2[,type_vars1$numerical_vars,with=F],data.table(set=set.names[2])))
  
  total_data[,c(ignored_variables):=NULL]
  
  metrics <- list()
  for(i in 1:length(fun_list))
  {
    metrics[[names(fun_list)[i]]] <- total_data[,lapply(.SD,fun_list[[i]]),by=set]
  }
return(metrics)
}

da.plot.numeric.summary.comparation <- function(metrics_summary){
  m.metrics_summary <- melt(metrics_summary,id="set")
  m.control <- melt(sapply(metrics_summary,function(x){ sapply(x[,-1,with=F], function(x){   ifelse( pmax(x[1],x[2]) !=0 , abs( (x[1]-x[2])/pmax(x[1],x[2]) ) ,0) } ) }))
  
  metric_table <- merge(m.metrics_summary, m.control,by.x = c("variable" , "L1"),by.y = c("Var1","Var2") )
  metric_table$lavel <- factor( ifelse( metric_table$value.y>0.3,"e > 30%",ifelse(metric_table$value.y<0.1,"e < 10%","10% < e < 30%")) )
  
  g <- ggplot(metric_table)+
    geom_tile(aes(x=paste(L1,set),y=variable,fill=lavel),alpha=0.4,position = "identity")+
    geom_text(aes(x=paste(L1,set),y=variable,label=value.x))+theme(axis.text.x=element_text(angle=90))
  
  return(list( g = g , metric_table = metric_table ))
}

da.variable.numeric.checks<- function(metric_table){
  
  metric_table <- as.data.table(metric_table[,c("variable","L1","lavel")])
  alerted_numeric_vars <- unique( metric_table[,.N,by=.(variable,lavel)][lavel!="e < 10%",variable])
return(alerted_numeric_vars)
  }

# data = dades

da.plot.numeric.variable <- function(data1,data2,set.names=c("data1","data2"),var_list=NULL,max.cat=50)
{
  if( all( names(data1) != names(data2) ) ) { stop("names() or data1 and data2 must be the same ")}
  type_vars1 <- da.variable.types(data1,max.cat)
  type_vars2 <- da.variable.types(data2,max.cat)
  if( all( type_vars1$numerical_vars   != type_vars2$numerical_vars   ) ) { stop("numeric types of data1 and data2 must me equal ")}
  
  if(is.null(var_list))
  {
    total_data = rbind(cbind(data1[,type_vars1$numerical_vars,with=F],data.table(set=set.names[1])),
                       cbind(data2[,type_vars1$numerical_vars,with=F],data.table(set=set.names[2])))
  }else{
    var_list <- names(data1) %in% var_list & names(data1) %in% type_vars1$numerical_vars
    total_data = rbind(cbind(data1[,var_list,with=F],data.table(set=set.names[1])),
                       cbind(data2[,var_list,with=F],data.table(set=set.names[2])))
    
  }
  
  
  
  g <- total_data[,lapply(.SD,function(x){ plot(ggplot(data=NULL)+geom_histogram(aes(x=x,fill=total_data$set),position="identity",alpha=0.3,bins = 30)+xlab(names(.SD))) }) ,.SDcols=names(data1)[var_list] ]
  
  environment()
}

for(i in 2:ncol(total_data)-1)
{
  plot(ggplot(total_data)+geom_histogram(aes_string(x=names(total_data)[i],fill="set"),position="identity",alpha=0.4))
  print(ks.test(total_data[set==set.names[1],get(names(total_data)[i])],total_data[set==set.names[2],get(names(total_data)[i])]))
}

nrow(total_data)



total_data[,.(mean(T2_V9,na.rm =T),sd(T2_V9)),by=set]

total_data[,.(mean(T2_V9,na.rm =T),sd(T2_V9)),by=set]
total_data[,.(mean(T2_V9,na.rm =T),sd(T2_V9)),by=set]

ks_test <- function(dt){ }


x_ <- cumsum(sort(total_data[set==set.names[1],T2_V9]))
y_ <- cumsum(sort(total_data[set==set.names[2],T2_V9]))

ggplot()+geom_line(aes(x=1:length(x_),y=x_,fill="red"),alpha=0.4)+geom_line(aes(x=1:length(y_),y=y_,fill="blue"),alpha=0.4)


dades<-fread("../../data/liberty/train.csv")



Nulls <- function(x) { sum(is.null(x)|is.na(x))}
porcentage_Nulls <- function(x) { round(sum(is.null(x)|is.na(x))/length(x),3) }
min  <- function(x) { min(x, na.rm =T) }
max  <- function(x) { max(x, na.rm =T) }
mean <- function(x) { mean(x, na.rm =T) }
sd   <- function(x) { sd(x, na.rm =T) }
Q1 <- function(x)  { quantile(x,probs =0.01, na.rm =T) }
Q10 <- function(x) { quantile(x,probs =0.1, na.rm =T) }
Q25 <- function(x) { quantile(x,probs =0.25, na.rm =T) }
Q50 <- function(x) { quantile(x,probs =0.50, na.rm =T) }
Q75 <- function(x) { quantile(x,probs =0.75, na.rm =T) }
Q90 <- function(x) { quantile(x,probs =0.90, na.rm =T) }
Q99 <- function(x) { quantile(x,probs =0.99, na.rm =T) }




data1=dades[1:1000]
data2=dades[1001:2000]
set.names=c("train_p","test_p")
ignored_variables=c("Id", "Hazard")



(type_vars <- da.variable.types(dades))
(plot_corr <- da.correlation.matrix(dades))
(in_out_list <- da.in_out.categories(data1,data2))

da.plot.in_out.categories(in_out_list)

fun_list <- list(Nulls=Nulls,
                 porcentage_Nulls=porcentage_Nulls,
                 Q90=Q90)

fun_list2 <- list(Nulls=Nulls,
                 porcentage_Nulls=porcentage_Nulls,
                 Q1=Q1,
                 Q10=Q10,
                 Q25=Q25,
                 Q50=Q50,
                 Q75=Q75,
                 Q90=Q90)

da.numeric.summary.comparation(data1,data2,set.names=c("data1","data2"),fun_list=fun_list,ignored_variables="")
da.numeric.summary.comparation(data1,data2,set.names=c("data1","data2"),fun_list=fun_list,ignored_variables=ignored_variables)
(metrics_summary <- da.numeric.summary.comparation(data1,data2,set.names=c("data1","data2"),fun_list=fun_list2,ignored_variables=ignored_variables))
metric_table <- da.plot.numeric.summary.comparation (metrics_summary)
metric_table$g
metric_table$metric_table

alerted_numeric_vars <- da.variable.numeric.checks(metric_table$metric_table)

var_list=alerted_numeric_vars

da.plot.numeric.variable(data1 = data1,data2 = data2,set.names = c("Train","test"),max.cat = 50)

