
# load ulgee package
library(ulgee)

# prep. 2010
data_2010 <- readxl::read_excel("application/data/censo_2010.xlsx")
data_2010 <- data_2010[-c(1,29:31),]
names(data_2010) <- c("state", "gini", "pwater", "life", "peletro", "psewage", "hdi", "income")
data_2010$time <- 3
data_2010$id <- 1:nrow(data_2010)
data_2010$psewage <- data_2010$psewage/100

# prep. 2000
data_2000 <- readxl::read_excel("application/data/censo_2000.xlsx")
data_2000 <- data_2000[-c(1,29:31),]
names(data_2000) <- c("state", "gini", "pwater", "life", "peletro", "psewage", "hdi", "income")
data_2000$time <- 2
data_2000$id <- 1:nrow(data_2000)
data_2000$psewage <- data_2000$psewage/100

# prep. 1991
data_1991 <- readxl::read_excel("application/data/censo_1991.xlsx")
data_1991 <- data_1991[-c(1,29:31),]
names(data_1991) <- c("state", "gini", "pwater", "life", "peletro", "psewage", "hdi", "income")
data_1991$time <- 1
data_1991$id <- 1:nrow(data_1991)
data_1991$psewage <- data_1991$psewage/100

# prep. data
data <- rbind(data_2010, data_2000, data_1991)

# num. clusters
table(table(data$id))

# summary
FSA::Summarize(psewage ~ time, data)

# box-plot robust
par(mar=c(5.5,5.5,2,2), mfrow=c(1,2))
boxplot(split(LaplacesDemon::logit(data$psewage), data$time), ylab="logit(psewage)", xlab="time", pch=16, col="white", cex.lab=1.5, cex.axis=1.2, names=c("1991", "2000", "2010"))

# dispersion plot
colors <- c("red", "green", "blue")
plot(data$gini, LaplacesDemon::logit(data$psewage), pch=16, col=colors[data$time], ylab="logit(psewage)", xlab="gini", cex.lab=1.5, cex.axis=1.2, lwd=2, xlim=c(0.45,0.68))
legend(0.45, -0.6, c("1991", "2000", "2010"), col=colors, cex=0.8, lwd=2, bty="n")

# add 1991 line
newdata <- data.frame(gini=seq(min(data_1991$gini), max(data_1991$gini), length.out=100))
newdata$pred <- predict(lm(LaplacesDemon::logit(psewage)~gini, data_1991), newdata)
with(newdata, lines(x=gini, y=pred, col="red", lwd=2))

# add 2000 line
newdata <- data.frame(gini=seq(min(data_2000$gini), max(data_2000$gini), length.out=100))
newdata$pred <- predict(lm(LaplacesDemon::logit(psewage)~gini, data_2000), newdata)
with(newdata, lines(x=gini, y=pred, col="green", lwd=2))

# add 2010 line
newdata <- data.frame(gini=seq(min(data_2010$gini), max(data_2010$gini), length.out=100))
newdata$pred <- predict(lm(LaplacesDemon::logit(psewage)~gini, data_2010), newdata)
with(newdata, lines(x=gini, y=pred, col="blue", lwd=2))

# prep. model
y <- data$psewage
X <- model.matrix(~poly(gini,1)*factor(time), data)
time <- data$time; id <- data$id 

# model
fit_1 <- ulgee(y, X, time, id, "EXC", "logit", 1e-6, 40)

# est. 
round(fit_1$mu.coefs, 2)
round(diag(fit_1$vcov), 2)
round(fit_1$pvalues, 4)
round(fit_1$rho, 2)

# interpretation
round(exp(fit_1$mu.coefs[2]*0.01), 3)*100
round(exp((fit_1$mu.coefs[2]+fit_1$mu.coefs[5])*0.01), 3)*100
round(exp((fit_1$mu.coefs[2]+fit_1$mu.coefs[6])*0.01), 3)*100

# diagnostic 
diag_quant(fit_1, X, 100, n=1)
diag_quant(fit_1, X, 100, T, T, n=1)

# sensitivity 
sens_conf(fit_1, 4, 1, 4, 3)

# prep position 
pos <- as.numeric(data$id==3 & data$time==1)
pos <- pos + as.numeric(data$id==24 & data$time==3)
pos <- pos + as.numeric(data$id==24 & data$time==1)
pos <- pos + as.numeric(data$id==25 & data$time==1)
pos <- pos + as.numeric(data$id==10 & data$time==1)
pos <- pos==1

# sensitivity dropp
set.seed(9792)
table <- sens_mrc(fit_1, y, X, 10 , pos)
print(xtable::xtable(table, digits=2), type="latex", include.rownames=F, include.colnames=F)

# asymptotic check
table <- sens_coef(fit_1, X, 500)
titles <- c(expression(paste("Sample standardized of ", hat(alpha))),
            expression(paste("Sample standardized of ", hat(tau))),
            expression(paste("Sample standardized of ", hat(beta)[2])),
            expression(paste("Sample standardized of ", hat(beta)[3])),
            expression(paste("Sample standardized of ", hat(gamma)[2])),
            expression(paste("Sample standardized of ", hat(gamma)[3])))
par(mar=c(5.5,5.5,2,2), mfrow=c(1,2))
for(i in 1:6){
  pcoef_i <- (table[,i]-mean(table[,i]))/sd(table[,i])
  qqnorm(pcoef_i, xlab="N(0,1) quantile", ylab=titles[i], pch=16, cex.lab=1.5, cex.axis=1.2, main="")
  abline(a=0, b=1, xlab="", ylab="", lty=2, lwd=2)
}
