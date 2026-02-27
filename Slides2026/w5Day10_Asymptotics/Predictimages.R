install.packages("devtools")
install.packages("glmnet")
require(devtools)
install_version("ElemStatLearn", version = "2015.6.26.2", repos = "http://cran.us.r-project.org")
library("ElemStatLearn")
library(glmnet)
data(zip.test)

head(zip.test)

image(zip2image(zip.train, 1))
image(zip2image(zip.train, 1), col=gray(256:0/256), zlim=c(0,1), xlab="", ylab="" )  



yt <- zip.train[,1]==9

data <- data.frame(cbind(yt=yt, zip.train[,-1]))

#Fit a linear regression

mod<-lm(yt ~., data=data)

table(yt, predict(mod)>.5)


ytest <- zip.test[,1]==9

datatest <- data.frame(cbind(yt=ytest, zip.test[,-1]))

table(ytest, predict(mod, datatest)>.5)



#Fit a linear regression with regularization (elastic net penalty)

library(glmnet)
Numbers<-as.data.frame(zip.train)
Numbers[,1]<-as.factor(Numbers[,1])
names(Numbers)<-c("number",as.character(seq(1,256)))

ii<-sample(seq(1,dim(Numbers)[1]),1000)


#ggcv<-cv.glmnet(x=as.matrix(Numbers[ii,-1]),Numbers[ii,1],family="multinomial",  type.measure = "mse", nfolds = 20,alpha=1)

plot(ggcv)
#log(ggcv$lambda.1se)


gg<-glmnet(x=as.matrix(Numbers[ii,-1]),Numbers[ii,1],family="multinomial", alpha=1)


par(mfrow=c(4,3))
plot(gg,xvar="lambda")


out<-predict(gg,newx=zip.test[,-1],type="class", s=exp(-6))


table(zip.test[,1],out)




