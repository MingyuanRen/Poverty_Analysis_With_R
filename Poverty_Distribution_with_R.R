read.csv("dat47.csv")
dat47


expenditure <- ts(dat47[3], frequency = 4, start = 1950)
expenditure

plot(expenditure, main = "Consumption Expenditure from 1950 to 2020",
     xlab="Time", ylab="Consumption Expenditure in thousands of dollars")

log_expenditure <- log(expenditure)
plot(log_expenditure, main = "Consumption Expenditure from 1950 to 2020(Log-Scale)",
     xlab="Time", ylab="Consumption Expenditure in thousands of dollars")


# annualized growth rate
simple_growth <- diff(expenditure)/lag(expenditure, -1)
simple_growth <- (1+simple_growth)^4 - 1
simple_growth <- simple_growth * 100
simple_growth
average_simple <- mean(simple_growth)
average_simple
plot(simple_growth, main="Annualized Growth Rates for Different Quarters", xlab= "Time", 
     ylab = "Growth Rate(%)")
#abline(h=0, col= "Red", lwd = 2, lty = 4)
abline(h=average_simple, col = "DarkBlue", lwd =2, lty=4)
abline(h=0, col="darkred", lwd=2,lty=4)
legend("topleft", bty="n", c("Annualized Growth Rate", "1950-2020 average", "0% Line"), 
       col = c(1, "darkblue", "darkred"), lty = c(1,2,3), lwd = 2)


#annualized log growth rate
log_return <- log(expenditure)
log_return <- diff(log_return)*100
log_return
average_log <- mean(log_return)
average_log
plot(log_return, main="Annualized Growth Rates (Log-Scale)", xlab= "Time", 
     ylab = "Growth Rate(%)")
abline(h=average_log, col= "Blue", lwd = 2, lty = 4)
abline(h=0, col="darkred", lwd=2,lty=4)
legend("topleft", bty = "n", c("Annualized Growth Rate (Log-Scale)", "1950-2020 average", "0% Line"), 
       col = c(1, "darkblue", "darkred"), lty = c(1,2,3), lwd = 2)


# linear and quadratic in simple expenditure
t <- time(expenditure)
coefT <- coef(lm(expenditure~t))
linear_trend <- coefT[1] + coefT[2]*t
plot(expenditure, main = "Consumption Expenditure from 1950 to 2020",
     xlab="Time", ylab="Consumption Expenditure in thousands of dollars")
lines(linear_trend, col=2, lty=2, lwd=2)


# quadratic 
t2 <- t^2
coefT2 <- coef(lm(expenditure~t+t2))
quadratic_trend <- coefT2[1] + coefT2[2]*t + coefT2[3]*t2
lines(quadratic_trend, col="darkblue", lty=2,lwd=2)
legend("topleft", bty="n", c("Expenditure", "Linear Trend", "Quadratic Trend"), col=c(1, 2, "darkblue"),
       lty=c(1, 2,3), lwd=2)


# linear and quadratic in log expenditure
log_expenditure
lcoefT <- coef(lm(log_expenditure~t)) ## linear coefficients
lcoefT2 <- coef(lm(log_expenditure~t+t2)) ## quadratic coefficients
ltrend <- lcoefT[1] + lcoefT[2]*t ## linear trend
ltrend2 <- lcoefT2[1] + lcoefT2[2]*t + lcoefT2[3]*t2 ## quadratic trend
plot(log_expenditure, lwd=1, main="Consumption Expenditure from 1950 to 2020(Log-Scale)",
     ylab="log(expenditure)") 
lines(ltrend, col=2, lty=2, lwd=3)
lines(ltrend2, col=4, lty=3, lwd=3)
legend("topleft", c("log(expenditure)", "Linear Trend","Quadratic Trend"), col=c(1,2,4), lty=1:3,
       lwd=c(2,4,4), bty='n')

# detrended series
CSI <- log_expenditure - ltrend2
dec <- decompose(CSI, filter = rep(1/5, 5))
c <- dec$trend

plot(CSI, main = "Detrended Quarterly Log Expenditure and Its Cycle", ylab = "log(expenditure)", lwd=2)
legend("topleft", "Detrended log(expenditure)", col=1, lty=1, lwd=2, bty = "n")
max(CSI)
min(CSI)


lines(c, col=2, lty=2, lwd=2)
legend("topleft", c("Detrended log(expenditure)", "Cycle"), col=1:2, lty=1:2, lwd=2, bty = "n")

a <- subset(c, !is.na(c))
max(a)
min(a)

low_frequency <- ltrend2 + c
plot(low_frequency, lwd = 2, main = "The Low Frequency Component of Log Expenditure", ylab = "log(Expenditure)")

s <- dec$figure
barplot(s, main="Log Consumption Expenditure Seasonal Component", xlab='Quarter', names.arg = c("First", "Second", "Third", "Fourth"))

#part c
new <- ts(dat111[3], frequency = 4, start = 1950)
log_new <- log(new)

lcoefT_new <- coef(lm(log_new~t)) ## linear coefficients
lcoefT2_new <- coef(lm(log_new~t+t2)) ## quadratic coefficients
ltrend_new <- lcoefT_new[1] + lcoefT_new[2]*t ## linear trend
ltrend2_new <- lcoefT2_new[1] + lcoefT2_new[2]*t + lcoefT2_new[3]*t2 ## quadratic trend
plot(log_new, lwd=1, main="Consumption Expenditure from 1950 to 2020(Log-Scale)",
     ylab="log(expenditure)") 
lines(ltrend_new, col=2, lty=2, lwd=3)
lines(ltrend2_new, col=4, lty=3, lwd=3)
legend("topleft", c("log(expenditure)", "Linear Trend","Quadratic Trend"), col=c(1,2,4), lty=1:3,
       lwd=c(2,4,4), bty='n')

# detrended series
CSI_new <- log_new - ltrend2_new
dec_new <- decompose(CSI_new, filter = rep(1/5, 5))
c_new <- dec_new$trend


plot(log_expenditure, log_new, main = "Comovement between two log expenditure", 
     xlab="log(expenditure)stats 77", ylab="log(expenditure) stats 111")



plot(c, c_new, pch=21, col=1, bg=1,
     main="Comovement Between the two cyclical component",
     xlab="log(expenditure)stats 77", ylab="log(expenditure) stats 111")



plot(c_new, main="log Expenditure and its cycle for dat111", ylab = "log(expendiure) for dat111")
plot(c, c_new, pch=21, col=1, bg=1,
     main="Comovement Between the two series",
     
     xlab="log(expenditure)stats 77", ylab="log(expenditure) stats 111")






