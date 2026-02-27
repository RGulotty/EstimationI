library(haven)
library(tidyverse)

#The study interviewed respondents in a pre-election survey that was conducted between August 18, 2020 and November 3, 2020. Election day was November 3, 2020. The study re-interviewed as many as possible of the same respondents in a post-election survey that was conducted between November 8, 2020 and January 4, 2021. 


ANES<-read_dta("~/Dropbox/LinearModels/Homeworks/Homework4/ANES2008_excpt.dta")

ANES<-ANES%>%mutate(y08=1,demft=obamaft)%>%select(y08,black, demft, age)

ANES04<-read_dta("~/Dropbox/LinearModels/anes2004TSdta/anes2004TS.dta")


ANES04<-ANES04%>%mutate(y08=0, black=ifelse(V043299==10,1,0), demft=ifelse(V043039<101&V043039>0,V043039, NA) , age=ifelse(abs(2004-V043249a)<100&V043249a>0, 2004-V043249a, NA) )%>%select(y08,black, demft, age)

ANES0408<-rbind(ANES, ANES04)

summary(lm(demft~y08+y08*black+age,data=ANES0408))

summary(lm(demft~y08+age,data=ANES0408))
summary(ANES0408$demft)

ANES0408$age

unrestricted<-lm(demft~y08+I(1-y08)+I(1-y08):black+y08:black+age-1,data=ANES0408)
summary(unrestricted)

restricted<-lm(demft~y08+I(1-y08)+black+age-1,data=ANES0408)
library(stargazer)
stargazer(unrestricted, restricted, digits=1)
summary(unrestricted)
anova(unrestricted, restricted)

