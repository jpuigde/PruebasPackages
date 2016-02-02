require(data.table)
require(ggplot2)
require(DT)

# rmarkdown::render("Data Audit.rmd")
# browseURL("Data_Audit.html")
# knit( "Data Audit.rmd","DataAudit.html")
# knitr::knit2html("Data Audit.rmd")
dades<-fread("../01-kagel-liberty/Data/train.csv")
set.seed(1)
ind   <- sample(1:nrow(dades),40000)

# summary(dades)
# str(dades)
# sapply(dades,class)

ind_numeric     <- sapply(dades,class) %in% c("numeric","integer")
Numerical       <- dades[,ind_numeric,with=F] # pillem nomes numeriques
ind_num_or_cat  <- sapply(dades,function(x) {length(unique(x))<50})  #quants valors pren cada categoria?
ind_categorical <- !ind_numeric | ind_num_or_cat
Categorical     <- dades[,ind_categorical,with=F] # pillem nomes categoricas

# Categorical-----
a <- sapply(Categorical,function(x) { sum(is.null(x)|is.na(x))}) # quans nulls hi ha?
categorical_audit <- as.data.frame(t(a)) ; rownames(categorical_audit) <- "Nulls"
a <- sapply(Categorical,function(x) { round(sum(is.null(x)|is.na(x))/length(x),3) })  #i en percentatge?
categorical_audit <- rbind(categorical_audit,a) ; rownames(categorical_audit)[nrow(categorical_audit)] <- "porcentage_Nulls"
a <- sapply(Categorical,function(x) {length(unique(x))})  #quants valors pren cada categoria?
categorical_audit <- rbind(categorical_audit,a) ; rownames(categorical_audit)[nrow(categorical_audit)] <- "valores_unicos"
a <- sapply(Categorical,function(x) {names(sort(table(x,exclude=NULL),decreasing=TRUE)[1])})  #i la moda??
categorical_audit <- rbind(categorical_audit,a) ; rownames(categorical_audit)[nrow(categorical_audit)] <- "Moda"
datatable(t(categorical_audit))

# Numerical----
a <- sapply(Numerical,function(x) { sum(is.null(x)|is.na(x))}) # quans nulls hi ha?
numerical_audit <- as.data.frame(t(a)) ; rownames(numerical_audit) <- "Nulls"
a <- sapply(Numerical,function(x) { round(sum(is.null(x)|is.na(x))/length(x),3) })#i en percentatge?
numerical_audit <- rbind(numerical_audit,a) ; rownames(numerical_audit)[nrow(numerical_audit)] <- "porcentage_Nulls"
a <-data.frame(matrix(as.numeric(unlist(sapply(summary(Numerical),strsplit,":")) [seq(2,2*ncol(Numerical)*6,2)]), 6))
names(a) <- names(numerical_audit) ; rownames(a)<-unlist( sapply(summary(Numerical)[,1],strsplit,":"))[seq(1,12,2)]
numerical_audit <- rbind(numerical_audit,a ) 
a <- round(sapply(Numerical,sd,na.rm = T),3)  # so whatever!!!!
numerical_audit <- rbind(numerical_audit,a) ; rownames(numerical_audit)[nrow(numerical_audit)] <- "standarDeviation"
datatable(t(numerical_audit))

for(i in 1:ncol(Categorical)){ plot(qplot(Categorical[,get(names(Categorical)[i])],xlab=names(Categorical)[i])+scale_x_discrete())} #let's superlot
for(i in 1:ncol(Numerical)){plot( ggplot(data=Numerical)+geom_histogram(aes_string( x=names(Numerical)[i]) ) ) }

# Categorical T1-----
Categorical <- dades[ind,ind_categorical,with=F]
a <- sapply(Categorical,function(x) { sum(is.null(x)|is.na(x))}) # quans nulls hi ha?
categorical_audit <- as.data.frame(t(a)) ; rownames(categorical_audit) <- "Nulls"
a <- sapply(Categorical,function(x) { round(sum(is.null(x)|is.na(x))/length(x),3) })  #i en percentatge?
categorical_audit <- rbind(categorical_audit,a) ; rownames(categorical_audit)[nrow(categorical_audit)] <- "porcentage_Nulls"
a <- sapply(Categorical,function(x) {length(unique(x))})  #quants valors pren cada categoria?
categorical_audit <- rbind(categorical_audit,a) ; rownames(categorical_audit)[nrow(categorical_audit)] <- "valores_unicos"
a <- sapply(Categorical,function(x) {names(sort(table(x,exclude=NULL),decreasing=TRUE)[1])})  #i la moda??
categorical_audit_1 <- rbind(categorical_audit,a) ; rownames(categorical_audit_1)[nrow(categorical_audit_1)] <- "Moda"
datatable(t(categorical_audit_1))

# Categorical T2-----
Categorical <- dades[-ind,ind_categorical,with=F]
a <- sapply(Categorical,function(x) { sum(is.null(x)|is.na(x))}) # quans nulls hi ha?
categorical_audit <- as.data.frame(t(a)) ; rownames(categorical_audit) <- "Nulls"
a <- sapply(Categorical,function(x) { round(sum(is.null(x)|is.na(x))/length(x),3) })  #i en percentatge?
categorical_audit <- rbind(categorical_audit,a) ; rownames(categorical_audit)[nrow(categorical_audit)] <- "porcentage_Nulls"
a <- sapply(Categorical,function(x) {length(unique(x))})  #quants valors pren cada categoria?
categorical_audit <- rbind(categorical_audit,a) ; rownames(categorical_audit)[nrow(categorical_audit)] <- "valores_unicos"
a <- sapply(Categorical,function(x) {names(sort(table(x,exclude=NULL),decreasing=TRUE)[1])})  #i la moda??
categorical_audit_2 <- rbind(categorical_audit,a) ; rownames(categorical_audit_2)[nrow(categorical_audit_2)] <- "Moda"
datatable(t(categorical_audit_2))

# Numerical T1----
Numerical       <- dades[ind,ind_numeric,with=F]
a <- sapply(Numerical,function(x) { sum(is.null(x)|is.na(x))}) # quans nulls hi ha?
numerical_audit <- as.data.frame(t(a)) ; rownames(numerical_audit) <- "Nulls"
a <- sapply(Numerical,function(x) { round(sum(is.null(x)|is.na(x))/length(x),3) })#i en percentatge?
numerical_audit <- rbind(numerical_audit,a) ; rownames(numerical_audit)[nrow(numerical_audit)] <- "porcentage_Nulls"
a <-data.frame(matrix(as.numeric(unlist(sapply(summary(Numerical),strsplit,":")) [seq(2,2*ncol(Numerical)*6,2)]), 6))
names(a) <- names(numerical_audit) ; rownames(a)<-unlist( sapply(summary(Numerical)[,1],strsplit,":"))[seq(1,12,2)]
numerical_audit <- rbind(numerical_audit,a ) 
a <- round(sapply(Numerical,sd,na.rm = T),3)  # so whatever!!!!
numerical_audit_1 <- rbind(numerical_audit,a) ; rownames(numerical_audit_1)[nrow(numerical_audit_1)] <- "standarDeviation"
datatable(t(numerical_audit_1))

# Numerical T2----
Numerical       <- dades[-ind,ind_numeric,with=F] 
a <- sapply(Numerical,function(x) { sum(is.null(x)|is.na(x))}) # quans nulls hi ha?
numerical_audit <- as.data.frame(t(a)) ; rownames(numerical_audit) <- "Nulls"
a <- sapply(Numerical,function(x) { round(sum(is.null(x)|is.na(x))/length(x),3) })#i en percentatge?
numerical_audit <- rbind(numerical_audit,a) ; rownames(numerical_audit)[nrow(numerical_audit)] <- "porcentage_Nulls"
a <-data.frame(matrix(as.numeric(unlist(sapply(summary(Numerical),strsplit,":")) [seq(2,2*ncol(Numerical)*6,2)]), 6))
names(a) <- names(numerical_audit) ; rownames(a)<-unlist( sapply(summary(Numerical)[,1],strsplit,":"))[seq(1,12,2)]
numerical_audit <- rbind(numerical_audit,a ) 
a <- round(sapply(Numerical,sd,na.rm = T),3)  # so whatever!!!!
numerical_audit_2 <- rbind(numerical_audit,a) ; rownames(numerical_audit_2)[nrow(numerical_audit_2)] <- "standarDeviation"
datatable(t(numerical_audit_2))



numerical_audit_1 <- data.table(metric=rownames(numerical_audit_1),numerical_audit_1)
numerical_audit_1_m <- melt(numerical_audit_1,id="metric",value.name = "Train")

numerical_audit_2 <- data.table(metric=rownames(numerical_audit_2),numerical_audit_2)
numerical_audit_2_m <- melt(numerical_audit_2,id="metric",value.name = "Test")
setkeyv(numerical_audit_1_m,c("metric","variable"))
setkeyv(numerical_audit_2_m,c("metric","variable"))

numerical_audit_convined <- numerical_audit_1_m[numerical_audit_2_m]
numerical_audit_convined$variable <- factor(numerical_audit_convined$variable , ordered = T)
numerical_audit_convined$control <- ifelse(pmax(numerical_audit_convined$Train,numerical_audit_convined$Test)==0,0,abs(numerical_audit_convined$Train-numerical_audit_convined$Test)/pmax(numerical_audit_convined$Train,numerical_audit_convined$Test))
# numerical_audit_convined$alarm <- cut(numerical_audit_convined$control,breaks=3,include.lowest=TRUE)
numerical_audit_convined_m <- melt(numerical_audit_convined,id=c("metric","variable","control"),measure.vars = c(),variable.name = "set",value.name = "value")
numerical_audit_convined_m$variable


ggplot(numerical_audit_convined_m,aes(x=paste(metric,set),y=variable,label=value,fill=control,colour=set),position="dodge")+
  geom_tile()+geom_text()








numerical_audit_convined[1:20,]

names(airquality) <- tolower(names(airquality))
aqm <- melt(airquality, id=c("month", "day"), na.rm=TRUE)

acast(aqm, day ~ month ~ variable)





for(i in 1:ncol(Categorical)){ plot(qplot(Categorical[,get(names(Categorical)[i])],xlab=names(Categorical)[i])+scale_x_discrete())} #let's superlot
for(i in 1:ncol(Numerical)){plot( ggplot(data=Numerical)+geom_histogram(aes_string( x=names(Numerical)[i]) ) ) }

# Matrius de correlació!!----
require(corrplot)

corrplot(cor(Numerical),order="hclust")

corr_mat_dades <- cor(Numerical)
hc <- hclust(as.dist(corr_mat_dades))
qplot(x=Var1, y=Var2, data=melt(corr_mat_dades[rev(hc$order),hc$order]), fill=value, geom="tile")+ scale_fill_gradient2(limits=c(-1, 1))

# Matriu ggpairs!!----
require(GGally)
png("plot.png",500*ncol(dades),500*ncol(dades))
ggpairs(dades,
        diag  = list(continuous = "bar", discrete = "bar"),
        upper = list(continuous = "cor", combo = "facethist",discrete ="ratio"),
        lower = list(continuous = "density", combo = "box",discrete ="facetbar"))
dev.off()


# Trensformacions!!----
require(caret)
model_pp <- preProcess(Numerical,method=c("YeoJohnson")) # transformació amb method = c("YeoJohnson","center", "scale") a mes les normailtzem.
model_pp <- preProcess(Numerical,method=c("YeoJohnson","center", "scale")) # tb hi ha method = "BoxCox"
dades_pp <- predict(model_pp,Numerical)
for(i in 1:ncol(dades_pp)){plot( ggplot(data=dades_pp)+geom_histogram(aes_string( x=names(dades_pp)[i]) ) ) }











# !!----
library(party)
library(tree)
identificado="Id"
target="Hazard"

featuresN <- setdiff(names(Numerical) , c(identificado,target))
formulaN  <- formula(paste(target,"~",paste(featuresN,collapse = "+")))
fitN      <- train(formulaN,Numerical,method = "rpart",tuneLength=6)
plot(fitC$finalModel)
text(fitC$finalModel)
fitC$finalModel

Categorical <- cbind(dades[,1:2,with=F],Categorical)
featuresC   <- setdiff(names(Categorical) , c(identificado,target))
formulaC    <- formula(paste(target,"~",paste(featuresC,collapse = "+")))
fitC        <- train(formulaC,Categorical,method = "rpart",tuneLength=6)
plot(fitC$finalModel)
text(fitC$finalModel)
fitC$finalModel


library(oblique.tree)
Numerical <- dades[,ind_numeric,with=F] # pillem nomes numeriques
Numerical <- as.data.frame(Numerical)[,c(-1)]
head(Numerical)
Numerical$Hazard <- factor(Numerical$Hazard)

fitON <- oblique.tree(formula=formulaN,data=Numerical)




data(crabs, package = "MASS")
aug.crabs.data <- data.frame(	g=factor(rep(1:4,each=50)),
                              predict(princomp(crabs[,4:8]))[,2:3])

plot(	aug.crabs.data[,-1],type="n")
text(	aug.crabs.data[,-1],
      col=as.numeric(aug.crabs.data[,1]),
      labels=as.numeric(aug.crabs.data[,1]))

#grow a full oblique tree
ob.tree <- oblique.tree(formula		= g~.,
                        data		= aug.crabs.data,
                        oblique.splits	= "only")
plot(ob.tree);text(ob.tree)


# devtools::install_github('ramnathv/slickCarousel')
# library(htmltools)
# library(htmlwidgets)
# library(slickCarousel)




# install.packages("FactoMineR")





