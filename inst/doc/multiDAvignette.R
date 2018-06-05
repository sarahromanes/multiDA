## ------------------------------------------------------------------------
library(multiDA)

vy   <- SRBCT$vy
mX   <- SRBCT$mX
res  <- multiDA(mX=mX, vy=vy, penalty="EBIC", equal.var=TRUE, set.options="exhaustive")

## ------------------------------------------------------------------------
vals <- predict(res, newdata=mX)$vy.pred          
rser <- sum(vals!=vy)/length(vy)

## ------------------------------------------------------------------------
print(res)

## ------------------------------------------------------------------------
p <- plot(res, ranks = 1)

## ------------------------------------------------------------------------
p1 <- plot(res, ranked=FALSE, features = c("V22", "V122"))
p1

