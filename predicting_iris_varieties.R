install.packages("neuralnet")
library(neuralnet)

data(iris)

iris2<-iris

iris2$setosa<-iris2$Species=="setosa"
iris2$virginica<-iris2$Species=="virginica"
iris2$versicolor<-iris2$Species=="versicolor"

x=nrow(iris2)
iris2.train.idx<-sample(x,size=x*0.7)
iris2.train<-iris2[iris2.train.idx,]
iris2.test<-iris2[-iris2.train.idx,]


iris2.net<-neuralnet(setosa+versicolor+virginica~
                      Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                    data=iris2.train,hidden=c(10,10),rep=5,err.fct="ce",
                    linear.output=F,lifesign="minimal",stepmax=1000000,
                    threshold=0.001)

iris2.prediction<-compute(iris2.net,iris2.test[1:4])
idx<-apply(iris2.prediction$net.result,1,which.max)
predicted<-c('setosa','virginica','versicolor')[idx]

table(predicted,iris2.test$Species)

plot(iris2.net)

