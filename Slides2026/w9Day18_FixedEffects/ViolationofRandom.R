x<-1:100

y<-case_when(
x<30 ~ x*.05+3,
x<60 ~ x*.05+6,
TRUE ~ x*.05+9)

group<-case_when(
x<30 ~ 1,
x<60 ~ 2,
TRUE ~ 3)


y<-y+rnorm(100, sd=.2)
plot(x,y)
for(id in 1:3){
abline(lm(y[group==id]~x[group==id] ), lty=2)
}
abline(lm(y~x ))