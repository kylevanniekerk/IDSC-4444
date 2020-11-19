## Part 1 
## A
laptopsales<- read.csv("LaptopSales.csv")
## B
summary(laptopsales)
which(is.na(laptopsales))
## C
View(laptopsales)
mean(laptopsales$Retail.Price)
median(laptopsales$Retail.Price)
## D
library(dplyr)
laptopsales %>% filter(Integrated.Wireless.=="No") %>% summarise(mean(Retail.Price))
View(laptopsales %>% filter(Integrated.Wireless.=="Yes"))
## E
laptopsales %>% summarise(max(Retail.Price))
## F
length(laptopsales[which(laptopsales$HD.Size..GB.<150),"Configuration"])
## G
is.numeric(laptopsales$Retail.Price)
sum(laptopsales$Retail.Price)

##PART 2
View(laptopsales)
library(ggplot2)
summary(laptopsales)
## A
laptopsales$custdist[laptopsales$CustomerStoreDistance >= 5000] <- "High"
laptopsales$custdist[laptopsales$CustomerStoreDistance < 5000 & laptopsales$CustomerStoreDistance > 2000] <- "Middle"
laptopsales$custdist[laptopsales$CustomerStoreDistance <= 2000] <- "Low"

ggplot(data = laptopsales, aes(x = CustomerStoreDistance, fill = custdist)) +
  geom_histogram(alpha=1) +
  ggtitle(" Distribution of Customer Store Distance by their count ") +
  labs(x = "Customer Store Distance", y = "Count")
## B

ggplot(data = laptopsales, aes(y=Retail.Price)) + 
  geom_boxplot(outlier.color = "orange", outlier.size = 3, outlier.shape = 17) +
  ggtitle("Boxplot of Retail Price") +
  labs(y="Retail Price")
## C

ggplot(data = laptopsales, aes(x=HD.Size..GB., y=Retail.Price, group=HD.Size..GB., fill = HD.Size..GB.)) + 
  geom_boxplot(outlier.color = "orange", outlier.size = 3, outlier.shape = 17) +
  ggtitle("Boxplot of Retail Price by HD Size") +
  labs(y="Retail Price", x= "HD Size")

## D
#### A
ggplot(data = laptopsales, aes(x= Battery.Life..Hours., y=Retail.Price, color = Battery.Life..Hours.)) +
  geom_point(size=2)+
  ggtitle("Relationship between Retail Price and the battery life of laptops") +
  labs(x= "Battery Life", y= "Retail Price")

#### B
laptopsales$batcat[laptopsales$Battery.Life..Hours.>=6]<- "Strong"
laptopsales$batcat[laptopsales$Battery.Life..Hours.==5]<- "Mild"
laptopsales$batcat[laptopsales$Battery.Life..Hours.<=4]<- "Weak"

ggplot(data = laptopsales, aes(x=Retail.Price, colour = batcat, fill = batcat)) +
  geom_histogram(alpha=0.8) +
  ggtitle(" Distribution of Retail Price by Categorical battery lives ") +
  labs(x = "Retail Price", y = "Battery Life Category")