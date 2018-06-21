## ------------------------------------------------------------------------
library(multiDA)

vy   <- SRBCT$vy
mX   <- SRBCT$mX
res  <- multiDA(mX=mX, vy=vy, penalty="EBIC", equal.var=TRUE, set.options="exhaustive")

## ------------------------------------------------------------------------
vals <- predict(res, newdata=mX)$vy.pred          
rser <- sum(vals!=vy)/length(vy)
rser

## ------------------------------------------------------------------------
print(res)

## ------------------------------------------------------------------------
plot(res, ranks = 1)

## ------------------------------------------------------------------------
plot(res, ranked=FALSE, features = c("V22", "V122"))

## ------------------------------------------------------------------------
glimpse_multiDA(res)

## ------------------------------------------------------------------------
tidy_res <- tidy_multiDA(res)
head(tidy_res)

## ------------------------------------------------------------------------
augment_res <- augment_multiDA(res)
dim(augment_res) #twice as long!

