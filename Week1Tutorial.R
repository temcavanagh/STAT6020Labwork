plot(rnorm(10), rnorm(10))

library(ggplot2)
ggplot() + aes(x=rnorm(10), y=rnorm(10)) + geom_point() + geom_smooth(method = 'lm', se = TRUE) + ggtitle("Randomised Chart")