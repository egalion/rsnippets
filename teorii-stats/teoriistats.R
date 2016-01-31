rezultati <- read.csv("results.csv")

head(rezultati)

table(rezultati$group)

#function to replace multiple text strings
mgsub <- function(pattern, replacement, x, ...) {
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}


rezultati$group <- mgsub(c("100","101","102","41","59","42","61"), c("инд","инд","инд","Марк","Марк","МИО","Ик"), rezultati$group)


rezcomplete <- rezultati[complete.cases(rezultati$exam), ]

table(rezcomplete$group)

summary(rezcomplete$exam)
sd(rezcomplete$exam)
mean(rezcomplete$exam) - sd(rezcomplete$exam)

1-pbinom(12, 30, 0.25)

for (i in 7:12) {
  j <- 1-pbinom(i, 30, 0.25)
  cat("При", i, "верни отговора вероятността да са познати случайно е", round((j*100), digits = 1),"%.\n") 
}

table(rezcomplete$group)

quantile(rezcomplete$exam)
sd(rezcomplete$exam)


aggregate(rezcomplete$exam, list(група = rezcomplete$group), summary)
aggregate(rezcomplete$exam, list(rezcomplete$group), sd)

tapply(rezcomplete$exam, rezcomplete$group, summary)



boxplot(rezcomplete$exam, ylab="Резултат от изпита", ylim = c(0,6))

boxplot(exam ~ group, data = rezcomplete, 
ylab = "Точки", 
main = "Резултати от изпита по икономически теории",
#names = c("инд.", "41", "42", "59", "61"),
col = c("yellow", "orange", "lightgreen", "lightblue"))

ggplot(data = rezcomplete, aes(x=group, y=exam)) + geom_boxplot()

ggplot(data = rezcomplete, aes(x=group, y=exam)) + 
  geom_boxplot(aes(fill=group)) +
  xlab("групи") + ylab("точки") + 
  ggtitle("Резултати от изпита") +
  guides(fill=guide_legend(title="Групи")) # + geom_jitter()
  
table(rezcomplete$exam)
length(which(rezcomplete$exam == 1.8))
