rm(list = ls())
library(tidyr)
library(ggplot2)
library(gridExtra)
library(scales)

growth <- as.data.frame(matrix(1, ncol = 3, nrow = 55))

head(growth)
growth$V1 <- seq(1:55)
attach(growth)
growth <- transform(V1, country1 = 1000*(1+0.1)^V1,
                    country2 = 10000*(1+0.05)^V1)
detach(growth)
colnames(growth) <- c("n_year", "country1", "country2")
head(growth)

comparison <- c(growth$country1 - growth$country2)
GDPdifference <- data.frame(seq(1:55), comparison)
colnames(GDPdifference) <- c("n_year", "GDPdiff")
head(GDPdifference)

df.growth <- gather(growth, key = "country", value = "GDP", 2:3)
str(df.growth)



# This option options(scipen = 3) is very important. Without it the 
# graph will have on the axis 1e+05, etc.

options(scipen = 3)

# Note: for ggplot we can first load the library scales
# library(scales). Then add
# + scale_x_continuous(labels = comma)

# We also remove the axes with "axes = F" and add them
# later with axis(1, pos = 0) and axis(2, pos = 0)

# Note: if we want our axes to start at 0, we use the options
# xaxs = "i", yaxs = "i"

# Note: if we want to do it in ggplot, we add
# + scale_x_continuous(expand=c(0,0))
# 
# par(mfrow = c(3,1), mar = c(3,3,1,1))
# 
# with(GDPdifference, plot(GDPdiff ~ n_year, xlab = "", 
#      ylab = "Разлика в БВП, лв.",
#      axes = F, xlim = c(0, 60), ylim = c(-30000, 40000), 
#      main = "Разлика в БВП \n на глава от населението \n между Страна 1 и Страна 2", type = "l", cex.main = 1.0))
# axis(1, pos=0)
# axis(2, pos=0) 
# abline(v = 50, lty = 3, col = "darkblue")
# 
# with(growth, plot(country1 ~ n_year, type = "l"))
# with(growth, lines(country2, col = "red"))
# abline(v = c(35,50), col = c("orange", "blue"), lty = 3)
# 
# with(growth, plot(log(country1) ~ n_year, type = "l"))
# with(growth, lines(log(country2), col = "red"))
# abline(v = c(35,50), col = c("orange", "blue"), lty = 3)

plot1 <- ggplot(data = GDPdifference, aes(n_year, GDPdiff)) + geom_line() + 
  geom_vline(xintercept = c(35, 50), colour = c("orange", "blue"), 
             linetype = "longdash") + 
  scale_x_continuous(expand=c(0,0)) +
  geom_abline(intercept=0, slope=0, colour = "grey") +
  xlab("") + ylab("Разлика в БВП, лв.") + 
  ggtitle("Разлика в БВП на глава от населението \n на Страна 1 и Страна 2") +
  theme_bw() +
  theme(axis.text.y=element_text(angle=45, hjust=1, vjust=0.5))

plot2 <- ggplot(df.growth, aes(x = n_year, y = GDP, colour = country)) +
  geom_line() + 
  geom_vline(xintercept = c(35, 50), colour = c("orange", "blue"), 
             linetype = "longdash") + 
  scale_x_continuous(expand=c(0,0)) +
  xlab("") + ylab("БВП на глава от населението, лв.") +
  ggtitle("Промяна в БВП на глава от населението, \n абсолютни стойности, лв.") +
  scale_colour_manual("", labels = c("Страна 1", "Страна 2"), 
                      values = c("darkgreen", "red")) +
  theme_bw() + theme(legend.position = c(0.12, 0.91), 
                     axis.text.y=element_text(angle=45, hjust=1, vjust=0.5))

plot3 <- ggplot(df.growth, aes(x = n_year, y = GDP, colour = country)) +
  geom_line() + 
  geom_vline(xintercept = c(35, 50), colour = c("orange", "blue"),
             linetype = "longdash") +
  scale_x_continuous(expand=c(0,0)) +
  xlab("Година") + ylab("БВП на глава от населението, логаритмична скала") +
  ggtitle("Промяна в БВП на глава от населението, \n логаритмична скала на БВП") +
  scale_colour_manual("", labels = c("Страна 1", "Страна 2"), 
                      values = c("darkgreen", "red")) +
  theme_bw() + theme(legend.position = c(0.12, 0.91),
                     axis.text.y=element_text(angle=45, hjust=0, vjust=0.5)) +
  scale_y_continuous(trans=log2_trans() #, # or coord_trans(y="log2")              
                    # breaks = trans_breaks("log2", function(x) 2^x),
                    # labels = trans_format("log2", math_format(2^.x))
                     )


grid.arrange(plot1, plot2, plot3, ncol = 1)

# matplot is a useful function to plot data in wide format

# matplot(x = growth[ ,1], y = growth[ , 2:3], type = "l", col = c("red", "blue"), lty = 1)
# abline(v = 50, col = "darkgreen", lty = 3, lwd = 2)
# legend(x = 1, y = 190000,  legend = c("Страна 1", "Страна 2"), 
#       col = c("red", "blue"), fill = c("red", "blue"), cex = 0.8)
# 


