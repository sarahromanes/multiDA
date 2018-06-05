## ------------------------------------------------------------------------
library(multiDA)

vy   <- SRBCT$vy
mX   <- SRBCT$mX
res  <- multiDA(mX, vy, penalty="EBIC", equal.var=TRUE, set.options="exhaustive")
vals <- predict(res, newdata=mX)$vy.pred          #vy.pred returns class labels
rser <- sum(vals!=vy)/length(vy)

## ------------------------------------------------------------------------
print(res)

## ------------------------------------------------------------------------
plot(res, ranks = 1:5)

## ------------------------------------------------------------------------
plot(res, ranked=FALSE, features = c("V132", "V123"))

