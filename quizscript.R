# животно, растение, държава, река, град.

keycodes <- c("123", "456178", "4762", "9517682", "49560")
orderedkeys <- keycodes[order(nchar(keycodes))]
library(rvest)
html <- html("https://bg.wikipedia.org/wiki/Списък_на_страните_по_население")
countriestable <- html_table(html)
str(countriestable)
countrytab <- as.vector(countriestable[[1]][2])
str(countrytab)
head(countrytab)
View(countrytab)

library(stringi)

temp <- character(0)
for (i in 1:nrow(countrytab)) {
  temp[i] <- stri_trim(countrytab[i,1], side = "both")
}

head(temp)

countrieslist <- temp[nchar(temp) < 8 & nchar(temp) > 2]
str(countrieslist)
class(countrieslist)
countrieslist <- tolower(countrieslist)

norepcountries <- character(0)
for (i in 1:length(countrieslist)) {
  tmp <- unlist(strsplit(countrieslist[i], ""))
  if (anyDuplicated(tmp) == 0 ) {
    norepcountries[i] <- countrieslist[i]
  }
}

norepcountries

norepcountries <- norepcountries[complete.cases(norepcountries)]

countries3l <- norepcountries[nchar(norepcountries) == 3]
countries4l <- norepcountries[nchar(norepcountries) == 4]
countries5l <- norepcountries[nchar(norepcountries) == 5]
countries6l <- norepcountries[nchar(norepcountries) == 6]
countries7l <- norepcountries[nchar(norepcountries) == 7]

# take all 7-letter countries, split by character and create
# a vector of characters. Add each of these vectors as a 
# separate entry in a list.

c7l <- lapply(countries7l, function(x) unlist(strsplit(x, "")))

# take the 7-digit key. Split by character and add each character
# as name to the vector in the list with 7-letter countries.

for (i in 1:length(c7l)) {
  names(c7l[[i]]) <- as.vector(unlist(strsplit(orderedkeys[5], "")))
}


keys3 <- unlist(strsplit(orderedkeys[1], "")) 
keys4 <- unlist(strsplit(orderedkeys[2], ""))
keys5 <- unlist(strsplit(orderedkeys[3], ""))
keys6 <- unlist(strsplit(orderedkeys[4], ""))
keys7 <- unlist(strsplit(orderedkeys[5], ""))

# create a list with all keys
allkeys <- list()
for (i in 3:7) {
  allkeys[[i-2]] <- get((paste("keys", i, sep = "")))
}

allkeys


c7l[[1]]
keys3

## function to check how it works
# rep("_", 3)
# srchwrd <- rep("_", length(keys3))
# for (i in 1:length(c7l[[1]])) {
#   for (j in 1:length(keys3)) {
#     if (names(c7l[[1]][i]) == keys3[j]) {
#       print(c7l[[1]][i])
#       srchwrd[j] <- unlist(c7l[[1]][i]) 
#     } 
#   } 
# }

# start of function to print all words

for (n in 1:length(c7l)) {
  cat("\n", c7l[[n]], "\n")
  for (k in 1:length(allkeys)) {
    rep("X", 3)
    srchwrd <- rep("_", length(allkeys[[k]]))
    for (i in 1:length(c7l[[n]])) {
      for (j in 1:length(allkeys[[k]])) {
        if (names(c7l[[1]][i]) == allkeys[[k]][j]) {
          
          srchwrd[j] <- unlist(c7l[[n]][i]) 
        } 
      } 
    }
    #  cat(c7l[[1]], "  --  ")
    cat(srchwrd, "\n")
  }
}
  
cat(unlist(c7l[[1]]))
cat(srchwrd)


reki5 <- html("https://bg.wikipedia.org/w/index.php?title=%D0%9A%D0%B0%D1%82%D0%B5%D0%B3%D0%BE%D1%80%D0%B8%D1%8F:%D0%A0%D0%B5%D0%BA%D0%B8_%D0%BF%D0%BE_%D0%B0%D0%B7%D0%B1%D1%83%D1%87%D0%B5%D0%BD_%D1%80%D0%B5%D0%B4&pagefrom=%D0%A7%D1%83%D0%BF%D1%80%D0%B5%D0%BD%D1%81%D0%BA%D0%B0+%D1%80%D0%B5%D0%BA%D0%B0#mw-pages")
reki5node <- html_node(reki5, ".mw-category")
html_text(reki5node)
strsplit(html_text(reki5node), "\n")

readLines("http://www.textfiles.com/music/ktop100.txt")
cat(c("a","b","c"))


