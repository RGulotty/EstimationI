library(carData)


y<-Prestige$prestige

x1<-cbind(1, Prestige$income)

x2<-cbind(Prestige$education,Prestige$women)

x12<-cbind(x1,x2)

P1<-x1%*%solve(t(x1)%*%x1)%*%t(x1)
P12<-x12%*%solve(t(x12)%*%x12)%*%t(x12)

M1<-diag(102)-P1
M12<-diag(102)-P12


P1%*%y
predict(lm(prestige~income, data=Prestige))
M1%*%y
resid(lm(prestige~income, data=Prestige))


y_x1<-M1%*%y

x2_x1<-M1%*%x2

#RSS method
1-(y%*%M12%*%y)/t(y_x1)%*%y_x1


#SSR method
t(y_x1)%*%x2%*%solve(t(x2)%*%M1%*%x2)%*%t(x2)%*%y_x1/t(y_x1)%*%y_x1


mod1<-lm(prestige~income, data=Prestige)
mod2<-lm(prestige~income+education+women,data=Prestige)


(summary(mod2)$r.squared-summary(mod1)$r.squared)/(1-summary(mod1)$r.squared)