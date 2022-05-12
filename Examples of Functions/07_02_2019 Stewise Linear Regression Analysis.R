## Stepwise regression analysis
## Three important types: Forward Selection, Backward Elimination, and Both

## Lets look at the mtcars data set

head(mtcars)

## Build a simple linear model of the mtcars dataset

FitAll = lm(mpg ~ cyl + disp + hp + drat + qsec + vs + am + gear + carb, data = mtcars)

## Can also use a . instead of every column

FitAll = lm(mpg ~ . , data = mtcars)

## Taking the summary shows that nothing comes off as significant

summary(FitAll)

## Step is the r function for stepwise regression, backward means that everything is in the model to start with then
## one by one things are eliminated to see how the model impoves
## AIC = Aikike Information Critea (important for studying linear models) - The lowers the AIC the better 
## You have to remove one variable at a time do to colinearity which can mask effects
## You stop the stepwise function when the none command is at the top of the list

step(FitAll, direction = "backward")

## When doing a forward model you want a model with no explanatory variables

FitStart = lm(mpg ~ 1, data = mtcars)

step(FitStart, direction = "forward", scope = formula(FitAll))

## You end up with a different linear model when using the forward method vs the reverse method
## The method with the lower AIC is generally a better way to go

## Can also run both which is both forward and backward

c <- step(FitStart, direction = 'both', scope = formula(FitAll))
