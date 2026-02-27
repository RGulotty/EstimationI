
help(state.x77)
# Using a fitted lm model
states <- as.data.frame(state.x77)

library(interactions)
library(ggplot2)
states <- as.data.frame(state.x77)

fit <- lm( Murder ~  Illiteracy + Frost * Income), data = states)

interact_plot(model = fit, pred = Income, 
			modx = Frost, rug=T, plot.points=T)
library(stargazer)
stargazer(fit)

ggsave("~/Dropbox/LinearModels/LinearModelstexSlides/Day11_Dummy/interplot1.png", width=7, height=4)

interact_plot(model = fit, pred = Income, 
			modx = Frost, linearity.check=T, 
			rug=T, plot.points=T)
ggsave("~/Dropbox/LinearModels/LinearModelstexSlides/Day11_Dummy/interplot2.png", width=7, height=4)