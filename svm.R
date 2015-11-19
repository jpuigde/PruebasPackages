require(ggplot2)

n=1000

mislead <- sample(n,n/10)

a = runif(1)
b = runif(1)
x = runif(n)*20-10
y = runif(n)*20-10

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
svmModel = svm(x+y~z,data=datos,)

tuner <- tune(svm, x+y~z, data = datos, 
     ranges = list(gamma = seq(0.1,2,0.1), cost = seq(0.1,2,10)),
     tunecontrol = tune.control(sampling = "bootstrap")
)

tuner$best.parameters
svmModel = svm(x+y~z,data=datos,gamma=tuner$best.parameters$gamma,cost=tuner$best.parameters$cost)

pred <- predict( svmModel , datos )
ggplot() +
  geom_point( aes( x = datos$x  , y = datos$y , shape = as.factor( datos$z ) , colour = pred ) ) +
  geom_path(  aes( x = datos$x0 , y = datos$y0 ) )



# (same as:)
pred <- fitted(svmModel)

# Check accuracy:
head(table(pred, y),6)[,1:3]

# compute decision values and probabilities:
pred <- predict(svmModel, datos, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% svmModel$index + 1])