require(ggplot2)

n=1000
mislead <- sample(n,n/10)
dominio = 40

set.seed(1111)
a = runif(1)
b = runif(1)
x = runif(n)*dominio-dominio/2
y = runif(n)*dominio-dominio/2
r=4

datos <- data.frame( x = x , y = y ,
                     x0 = r*cos(seq(0,2*pi,length.out=n))/(a),
                     y0 = r*sin(seq(0,2*pi,length.out=n))/(b) , z = ifelse( a*a*x*x+b*b*y*y < r*r , 1 , 0 ) )

datos$z <- ifelse ( datos$z==1 | datos$x > datos$y , 1 , 0  )

datos$z[mislead] <- 1 - datos$z[mislead]
datos$missleded <- 0
datos$missleded[mislead] <- 1

ggplot()+
  geom_point(aes(x=datos$x,y=datos$y,shape=as.factor(datos$z),colour=datos$z))+
  geom_path(aes(x=datos$x0,y=datos$y0))

datos$x2 <- ifelse(datos$x>=0, sqrt(datos$x),-sqrt(-datos$x)) 
datos$y2 <- ifelse(datos$y>=0, sqrt(datos$y),-sqrt(-datos$y))

datos$xx <- datos$x*datos$x 
datos$yx <- datos$y*datos$x
datos$yy <- datos$y*datos$y

datos$xxx <- datos$xx*datos$x 
datos$yxx <- datos$yx*datos$x
datos$yxy <- datos$yx*datos$y
datos$yyy <- datos$yy*datos$y

form <- formula(paste("z~",  paste(names(datos)[-c(3:6)],collapse="+")))

require(glmnet)
names(datos)
form
glm_models <- glmnet( as.matrix(datos[,-c(3:5)]),as.factor(datos$z),family="binomial" )

glm_pred <- predict( glm_models , as.matrix(datos[,-c(3:5)]),type="response")
glm_pred_path <- predict( glm_models , as.matrix(datos[,-c(3:5)]))

for(i in 1:ncol(glm_pred)){ print( cvAUC::AUC(glm_pred[,i],as.factor(datos$z) ) ) }

dim(glm_pred)
ggplot()+
  geom_point(aes(x=datos$x,y=datos$y,shape=as.factor(datos$z),colour=as.factor(glm_pred[,84])))+
  geom_path(aes(x=datos$x0,y=datos$y0))+
  geom_path(aes(x=c(-10,10),y=c(-10,10)))

ggplot()+
  geom_jitter(aes(x=glm_pred[,84],y=1,shape=as.factor(datos$missleded),colour=as.factor(datos$z),size=as.factor(1-datos$missleded) ))+scale_size_manual(values=c(3,4))
  table(datos$z,datos$missleded)

  ggplot()+
  geom_jitter(aes(x=glm_pred[datos$missleded == 0,84],y=1,colour=as.factor( datos$z[ datos$missleded==0 ])))


