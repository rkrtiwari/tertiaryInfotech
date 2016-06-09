library(calibrate)

## Normal distribution p-value

# inputs
mean = 7.5
sd = 2
x1 <- 4   # change this value 


x <- seq(0,15, length.out = 1000)
y <- dnorm(x, mean = mean, sd = sd)


y1 <- dnorm(x1, mean = mean, sd = sd)
p <- pnorm(x1, mean = mean, sd = sd, lower.tail = FALSE)

# Coordinates of polygon
xpol <- seq(x1, max(x), length=100)
ypol <- dnorm(xpol, mean = mean, sd = sd)
ypolbase <- rep(0,100)
label <- paste("Colored Area", format(p, digits=2))

par(mfrow=c(1,1))
plot(x,y, xlim = c(0,15), ylim = c(0, 0.25), type = "l", ylab = "Density")
#points(x1, y1, pch = 4)
abline(v=x1)
abline(h=0)
polygon( c(xpol, rev(xpol)), c(ypolbase, rev(ypol)), col = "blue")
textxy(x1+0.5, y1+0.005, labs = format(p, digits=2), cex = 1)


## t-test




