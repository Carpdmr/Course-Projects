xcol <- replicate(1000, rexp(40, rate = .20))
dim(xcol)
.colMeans(xcol, na.rm = FALSE)
X <- .colMeans(xcol, 40, 1000, na.rm = FALSE)
hist(X)
var(X)
mean(X)
sd(X)
colVars(xcol)
Y <- colVars(xcol)
mean(Y)
5/sqrt(40)
(5/sqrt(40))^2
ycol <- replicate(1000,rexp(1, .20))
hist(ycol)
mean(ycol)
var(ycol)
sd(ycol)
hist(ToothGrowth$len)
g1 <- ToothGrowth$len [1:10]; g2 <- ToothGrowth$len [31:40]; g3 <- ToothGrowth$len [11:20]; 
        g4 <- ToothGrowth$len [41:50]; g5 <- ToothGrowth$len [21:30]; g6 <- ToothGrowth$len[51:60]
d1 <- g2-g1; d2 <- g4-g3; d3 <- g6-g5
VC <- g1 + g3 + g5
OJ <- g2 + g4 + g6
t.test(d1)
t.test(d2)
t.test(d3)
hist(d3)
mean(g5); mean(g6)
hist(VC); 
mean(VC); var(VC); sd(VC)
hist(OJ)
mean(OJ); var(OJ); sd(OJ)
mean(g1); mean(g3); mean(g5)
mean(g2); mean(g4); mean(g6)
