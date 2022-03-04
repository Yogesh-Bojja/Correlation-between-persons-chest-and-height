df <- read.csv("bdims.csv")
df <- df[, c("che.di", "hgt")]

sum(is.na(df$che.di))
sum(is.na(df$hgt))

plot(hgt ~ che.di, data = df, xlab = "che.di", ylab = "hgt")

df$x2 <- df$che.di**2
df$y2 <- df$hgt**2
df$xy <- df$che.di*df$hgt
sum_x <- sum(df$che.di)
sum_y <- sum(df$hgt)
sum_x2 <- sum(df$che.di^2)
sum_y2 <- sum(df$hgt^2)
sum_xy <- sum(df$che.di*df$hgt)
n <- length(df$hgt)
Lxx <- sum_x2-((sum_x^2)/n)
Lyy <- sum_y2-((sum_y^2)/n)
Lxy = sum_xy - (((sum_x)*(sum_y))/n)
b = Lxy/Lxx
Lxx <- sum(df$che.di^2) - ((sum(df$che.di)^2)/n)
print(Lxx)
Lyy <- sum(df$hgt^2) - ((sum(df$hgt)^2)/n)
print(Lyy)
Lxy <- sum(df$che.di*df$hgt) - ((sum(df$che.di)*sum(df$hgt))/n)
print(Lxy)
b = Lxy/Lxx
print(b)
a = mean(df$hgt) - b*mean(df$che.di)
print(a)

plot(hgt ~ che.di, data = df, xlab = "che.di", ylab = "hgt")
abline(a = a, b = b, col= "red")

bdims_model <- lm(hgt~che.di, data = df)
summary(bdims_model)


#h0 : data do not fit the regression model
#ha : data fits the regression model
R2 <- (b*Lxy)/Lyy
F = R2/(1-R2)*(505/1)
anova(bdims_model)
pf(q=326.95, 1, 505, lower.tail = FALSE)
#as P value is less that 0.05 there is statistically significance that the data fit a linear regression model

#h0 : alpha = 0
#ha : alpha != 0
confint(bdims_model)
#as CI does not capture the hypothesis we reject the null hyp. there is statistical significance that alpha has some value

#h0 : beta = 0
#ha : beta != 0
# by p value
t <- 2.151/sqrt(53.8/3803.421)
2*pt(q=t,df=505,lower.tail = FALSE)
#p-value is less that 0.0 hence reject null.
#by CI
confint(bdims_model)
#CI for che.di doesnot capture the null hyp.There was statistically significant evidence that che.di was positively related to hgt.

#plot by model
plot(hgt ~ che.di, data = df, xlab = "che.di", ylab = "hgt")
abline(bdims_model, col= "red")

#check the assumptions for linear regression
plot(bdims_model)


#correlation
r <- cor(df$hgt, df$che.di)
print(r)

#h0 : r=0
#ha : r!=0
#by pvalue
t <- r*sqrt((n-2)/(1-r^2))
print(t)
2*pt(q = t,df = 50 - 2,lower.tail=FALSE)
#p<0.05 hence reject null hyp. hence there is positive relation between the hgt and che.di
#by CI
library(psychometric)
r <- Lxy/sqrt(Lxx*Lyy)
CIr(r=r, n=n, level = 0.95)
#reject null hyp by CI method.

