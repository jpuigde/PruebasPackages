require(ggplot2)

# Generacio de dades artificials amb alguna estructura--- executar fins a plot
n=1000
mislead <- sample(n,n/10) #afegim error de forma aleatoria 100 per maximitzar el drama
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

ggplot()+
  geom_point(aes(x=datos$x,y=datos$y,shape=as.factor(datos$z),colour=datos$z))+
  geom_path(aes(x=datos$x0,y=datos$y0))



require(e1071)

svmModel = svm(x+y~z,data=datos)

tuner <- tune(svm, x+y~z, data = datos, 
              ranges = list(gamma = seq(0.1,2,0.3), cost = seq(0.1,2,0.3)),
              tunecontrol = tune.control(sampling = "bootstrap")
)

tuner$best.parameters
svmModel = svm(x+y~z,data=datos,gamma=tuner$best.parameters$gamma,cost=tuner$best.parameters$cost)
svmModel = svm(x+y~z,data=datos,gamma=1,cost=1,epsilon=2.05)

svmModel
pred <- predict( svmModel , datos )

table(pred,datos$z)

ggplot() +
  geom_point( aes( x = datos$x  , y = datos$y , shape = as.factor( datos$z ) , colour = pred ) ) +
  geom_path(  aes( x = datos$x0 , y = datos$y0 ) )




