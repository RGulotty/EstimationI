library(tidyverse)

USGasG<-read_csv("http://people.stern.nyu.edu/wgreene/Text/Edition7/TableF2-2.csv")

USGasG<-USGasG%>%
	mutate(lnG = log(GASEXP/(GASP*POP)), 
		lnGlag = lag(lnG))


fm <- lm(lnG  ~ log(GASP) + log(INCOME)+ log(PNC) + log(PUC)+lnGlag, data = USGasG )
summary(fm)
xprimex <- vcov(fm)
sigmasq<-sigma(fm)^2
(bs<-coef(fm))

ggas<- c(0, 1/(1-bs[6]), 0,0,0,bs[2]/(1-bs[6])^2)
gincome <- c(0,0, 1/(1-bs[6]), 0,0,bs[3]/(1-bs[6])^2)

t(ggas)%*%(xprimex)%*%ggas

t(gincome)%*%(xprimex)%*%gincome



summary(fm)


dim(USGasG)


## Example 4.4
## estimates and standard errors (note different offset for intercept)
coef(fm)
sqrt(diag(vcov(fm)))
## confidence interval
confint(fm, parm = "log(INCOME)")
## test linear hypothesis
linearHypothesis(fm, "log(income) = 1")


