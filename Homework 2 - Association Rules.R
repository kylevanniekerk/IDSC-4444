# Part 2
## A
install.packages(arules)
library(arules)
moviesb <- read.csv("movies_binary.csv")
moviem <- as.matrix(moviesb)
movies <- as(moviem, "transactions")
itemInfo(movies)
## B
itemFrequency(movies)
itemFrequencyPlot(movies, ylim = c(0,1), main = "Support % of the top 8 movies", col = "steelblue3", topN = 8)
## C
subset(movies, items %ain% c("Up.2009.", "LionKing.The.1994."))
## D
rules<- apriori(movies, parameter = list(supp = 0.76, conf = 0.7, minlen = 2, maxlen = 3, target = "rules"))
inspect(rules)
## F
inspect(sort(rules, by = "lift"))
## G
install.packages(arulesViz)
library(arulesViz)
plot(rules, measure = c("support", "lift"), shading = "confidence")
## I
rules1 <- apriori(movies, parameter = list(supp = 0.5, conf = 0.5, target = "rules", minlen = 2))
inspect(rules1)
## J
inspectDT(rules1)
## K
plot(rules1, measure = c("support", "lift"),shading = "confidence")
