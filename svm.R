require(ggplot2)

n=2000

a = runif(1)
b = runif(1)
x = runif(n)*20-10
y = runif(n)*20-10

r=4

datos <- data.frame( x = x , y = y ,
                     x0 = r*cos(seq(0,2*pi,length.out=n))/(a),
                     y0 = r*sin(seq(0,2*pi,length.out=n))/(b) , z = as.factor(ifelse( a*a*x*x+b*b*y*y < r*r , 1 , 0 ) ))

ggplot()+
  geom_point(aes(x=datos$x,y=datos$y,shape=datos$z,colour=pred))+
  geom_path(aes(x=datos$x0,y=datos$y0))

require(e1071)
svmModel = svm(x+y~z,data=datos)


print(svmModel)
summary(svmModel)

# test with train data
pred <- predict(svmModel, datos)


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