getwd()
setwd("/home/alex29/Плот/mydata/")

# try it with the dygraphs library

library(dygraphs)
library(tidyr)

# Create a script to set the values easily for GDP and growth
# easily. We use two functions - assign and get. The first is
# needed, because the variables, defined in the function are in
# the local environment and not in the global. The second is
# needed, because assign() accepts the first argument as
# character string. What get() does is to take a character
# string and look if object with that name exists. 

scriptvalues <- function(c1gdp = 1000,  c1growth = 10, 
                         c2gdp = 10000, c2growth = 5, nyr = 55) {
  c1growth <- c1growth / 100
  c2growth <- c2growth / 100
  values <- c("c1gdp", "c1growth", "c2gdp", "c2growth", "nyr")
  values_list <- list("c1gdp" = "Country 1 GDP per capita at start",
                      "c1growth" = "Country 1 growth per year",
                      "c2gdp" = "Country 2 GDP per capita at start",
                      "c2growth" = "Country 2 growth per year",
                      "nyr" = "Years of growth"
                      )
  for (i in values) {
    assign(i, get(i), envir = globalenv())
    if (grepl("growth", i)) {
      n <- paste0(get(i)*100, "%.")
    } else {
      n <- paste0(get(i), ".")
    }
    cat(paste(values_list[[i]], "=", n), "\n")
    
  }
  cat("\nIf you want to change the parameters, 
      run the scriptvalues() function 
      with the parameters in the order 
      above.")
}


scriptvalues(1000, 10, 10000, 5, 55)

growth.df <- as.data.frame(matrix(1, nrow = nyr, ncol = 3))
colnames(growth.df) <- c("n_year", "country1", "country2")
growth.df$n_year <- seq(1:nyr)

growth.df <- transform(growth.df, country1 = c1gdp*(1+c1growth)^n_year, country2 = c2gdp*(1+c2growth)^n_year)
head(growth.df)

growth.df.ts <- ts(data = growth.df, start = 1950, end = 1950 + nrow(growth.df)-1, frequency = 1)
str(growth.df.ts)
plot.ts(growth.df.ts[ ,2:3], plot.type = "single")
abline(v = 1999, col = "green", lty = 3)
plot.ts(log(growth.df.ts[ ,2:3]))

library(xts)
library(lubridate)
library(dygraphs)



as.POSIXct(growth.df$n_year, origin = "1950-01-01")

growth.df$years <- seq(from = 1950, to = 2004, by = 1)
growth.df$years <- ymd(paste0(growth.df$years, "-01-01"))
growth.xts.df <- xts(growth.df[ ,2:3], order.by=growth.df$years)
plot.xts(growth.xts.df)
plot.xts()

dygraph(growth.xts.df)
dygraph(log(growth.xts.df))


matplot(format(growth.df[ ,4], "%Y"), 
        growth.df[ ,2:3], type = "l", lty = 1)
box()
legend("topleft", names(growth.df)[2:3], col = 1:2, lty = 1, cex = 0.7)
