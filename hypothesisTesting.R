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
abline(v=x1)
abline(h=0)
polygon( c(xpol, rev(xpol)), c(ypolbase, rev(ypol)), col = "blue")
textxy(x1+0.5, y1+0.005, labs = format(p, digits=2), cex = 1)


## binomial distribution
nt <- 100
ps <- 0.70
xvals <- 0:nt
yvals <- dbinom(xvals, nt, ps)
plot(xvals, yvals, type = "b", col="red", xlab = "Trial", ylab = "Success Probability")

## t-test
## unpaired data
boxplot(extra ~ group, data = sleep, col="green", xlab="Drug Type", 
        ylab = "Increase in Sleep Hours")

drug1 <- sleep$extra[1:10]
drug2 <- sleep$extra[11:20]
t.test(drug1, drug2)
t.test(drug1, drug2)$conf

## t-test
## paired Data
g1 <- sleep[sleep$group==1,]
g1$group <- as.numeric(g1$group)
g2 <- sleep[sleep$group==2,]
g2$group <- as.numeric(g2$group)
plot(as.numeric(sleep$group), sleep$extra, type = "p", cex = 2, xlim=c(0.5,2.5),
     xlab="Drug Type", ylab = "Increase in Sleep Hours", pch = 16, 
     col = "green", xaxt = "n")
axis(side =1, at = c(1,2))
for (i in 1:nrow(g1)){
  segments(g1$group[i], g1$extra[i], g2$group[i], g2$extra[i], col = "green")
}

drug1 <- sleep$extra[1:10]
drug2 <- sleep$extra[11:20]
t.test(drug1, drug2, paired = TRUE)
t.test(drug1, drug2, paired = TRUE)$conf


### correlation
head(swiss)
round(cor(swiss),2)
with(swiss, plot(Fertility, Examination))

## chi-squared test is used to compare observed frequencies with the 
## frequencies expected under some null hypothesis
eye <- margin.table(HairEyeColor,2)
eye
chisq.test(eye)
chisq.test(eye, p = c(0.37, 0.36, 0.15, 0.12))

## test of variance anova

## Generate the data
set.seed(101)
Y1 <- c(rnorm(n=500, mean = 5, sd = 1), rnorm(n=500, mean = 6, sd = 1), 
        rnorm(n=500, mean = 9, sd = 1 ))
F1 <- factor(rep(c("A", "B", "C"), each = 500))

## Plot the data
plot(density(Y1[F1=="A"], adjust = 1.5), col = "darkgreen", xlim = c(2,12),
     ylim=c(0,0.4), xlab = "Y1", main = "")
points(density(Y1[F1=="B"], adjust = 1.5), type = "l", col = "steelblue")
points(density(Y1[F1=="C"], adjust = 1.5), type = "l", col = "darkred")


## Anova test
aovY1 <- aov(Y1 ~ F1)
summary(aovY1)
TukeyHSD(aovY1)
tapply(Y1, F1, mean)
boxplot(Y1 ~ F1)

## two way anova
warpbreaks
aov1 <- aov(breaks ~ wool + tension + wool:tension, data = warpbreaks)
summary(aov1)
TukeyHSD(aov1)
tapply(warpbreaks$breaks, warpbreaks$wool, mean)
boxplot(breaks ~ wool, data = warpbreaks)
tapply(warpbreaks$breaks, warpbreaks$tension, mean)
boxplot(breaks ~ tension, data = warpbreaks)
