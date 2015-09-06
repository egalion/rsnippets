baskball <- read.table("blogpost - Two-Way Analysis of Variance with R - baskball.txt", header = TRUE)
baskball
attach(baskball)
tapply(Made, Time, mean)
tapply(Made, Shoes, mean)

# test for homoskedasticity

bartlett.test(baskball$Made, baskball$Shoes)
bartlett.test(baskball$Made, baskball$Time)

# p > 0.05 hence we don't reject the null hypothesis
# or we can see if the qchisq is bigger than the 
# Bartlett's K-squared

qchisq(0.950, 1) # where 1 is the degrees of freedom from 
# the Bartlett test

# or we can use another test

fligner.test(baskball$Made, baskball$Shoes)
fligner.test(baskball$Made, baskball$Time)

library(car)

# with one independent variable
leveneTest(Made ~ Shoes, data = baskball)
# with two independent variables
leveneTest(Made ~ Shoes*Time, data = baskball)


int <- aov(Made ~ Time*Shoes)
summary(int)
noint <- aov(Made ~ Time + Shoes)
summary(noint)
boxplot(Made ~ Time)
boxplot(Made ~ Shoes)
summary(aov(lm(Made ~ Time + Shoes + Time:Shoes)))
summary(aov(lm(Made ~ Time*Shoes)))
anova(aov(Made ~ Time*Shoes))
par(mfrow = c(1,2))
plot(Made ~ Time + Shoes)
par(mfrow = c(1,1))
interaction.plot(Time, Shoes, Made)
# Test for normality
qqnorm(lm(Made ~ Time + Shoes + Time:Shoes)$res)


model.tables(aov(Made ~ Time*Shoes))

