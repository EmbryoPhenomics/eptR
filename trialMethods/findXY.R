
# Simple xy generation for xyz stage - needs work on flexibility

findXY <- function(xy1, xy2, xNumWells, yNumWells) {
  
  diffX <- xy2[1] - xy1[1]
  
  X <- c(xy1[1], xy2[1])
  Y <- c(xy1[2])
  
  X[3:xNumWells] <- sapply(3:xNumWells, function (x) (x-1) * diffX)
  Y[2:yNumWells] <- sapply(2:yNumWells, function (x) (x-1) * diffX)
  
  xy <- data.frame(x = rep.int(X, times = yNumWells),
                   y = rep(Y, each = xNumWells))
  
  return(xy)

}

test <- findXY(xy1 = c(0,0), xy2 = c(5,0), xNumWells = 8, yNumWells = 12)

# Test results
plot(test$y ~ test$x)
