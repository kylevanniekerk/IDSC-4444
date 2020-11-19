library(stats)
library(factoextra)
nba1 <- read.csv("nba_2016.csv")
summary(nba)

ggplot(data = nba, aes(x = Age, y = PER, color = Yrs.Experience )) +
  geom_point(size = 2, ylim = c(0,100)) +
  ggtitle("Relationship between age and PER by Yrs Experience") +
  labs(x = "Age" , y = "PER")

normalize = function(x){
  return ((x - min(x))/(max(x) - min(x)))}
normalize(c(nba1$tm, nba1$Player, nba1$Age, nba1$MP, nba1$PER, nba1$TS., nba1$FTr, nba1$ORB., nba1$DRB., nba1$AST., nba1$STL., nba1$BLK.))

dist_matrix <- dist(nba, method = "euclidean")
head.matrix(as.matrix(dist_matrix))

h1 = hclust(dist_matrix, method = "ward.D")

plot(h1, hang = -1, cex = 0.4)

rect.hclust(h1, k = 4, border = 2:5)

cuth1 <- cutree(h1, k = 4)
table(cuth1)

install.packages("plyr")
library(plyr)

nba1$cuth1 <- cuth1
ddply(nba1, .(cuth1), summarize, PER_M=mean(PER), Yrs=mean(Yrs.Experience), ast=mean(AST.))

table(nba1$Tm, nba1$cuth1)

ddply(nba1, .(nba1$Tm), summarize, PER_M=mean(PER), Yrs=mean(Yrs.Experience), ast=mean(AST.))

k1 = kmeans(nba1, centers = 5, nstart = 10)

str(k1)

WSS <- c()

for (i in 1:40) {
  k = kmeans(nba1, centers = i, nstart = 10)
  wss = k$tot.withinss
  WSS[i] <- wss}

plot(1:10, WSS, type = "b", col = "red", ylab = "WSS", xlab = "K", ylim = c(0,18) )

bSS <- c()

for (i in 1:40) {
  k = kmeans(nba1, centers = i, nstart = 10)
  wss = k$tot.withinss
  bss = k$tot.withinss
  WSS[i] <- wss
  bSS[i] <- bss}

plot(1:40, WSS, type = "b", col = "red", ylab = "WSS and BSS", xlab = "K", ylim = c(0,18)  ) 
lines(1:40, BSS,type="o",col="blue")