
packages <- c("data.table", "ggplot2", "ggcorrplot", "ggpubr")
lapply(packages, require, character.only = TRUE)
library(ggplot2)

a=getwd()
a<-"C:/Users/Ashutosh/Documents/Miniproject"
setwd(a)

stocks=read.csv("Dataset.csv",stringsAsFactors = T)
View(stocks)

#################################
summary(stocks)

head(stocks)
tail(stocks)

nrow(stocks)
ncol(stocks)

colnames(stocks)

unique(stocks[,2]) # we have 9 companies

#################################

#graph=ggplot(data=stocks,aes(x=Symbol))
#graph+geom_histogram(stat="count")
#graph+geom_density()

#Exploratory Data Analysis

#General Analysis
summary(stocks)


# Frequency of each Symbol
aggregate(stocks$Series,
          by = list(rank=stocks$Symbol),
          FUN = length)

g1 <- ggplot(stocks, aes(x = Symbol)) +
  geom_bar(position = "dodge")+
  scale_fill_brewer(palette = "Set2")
ggarrange(g1, labels = c("A"), ncol = 1, nrow = 1)

# Αpply bw theme
theme_set(theme_bw())
#1 Distribution of quantitative variables
g1 <- ggplot(stocks, aes(x = Turnover)) +
  geom_density(aes(fill=Turnover), alpha=0.8,fill = "tan3") +
  labs(x = "Turnover", y = "Frequency",
       title = "Distribution of Turnover") + scale_x_continuous()+xlim(0,1e+15)

g2 <- ggplot(stocks, aes(x = Volume)) +
  geom_density(aes(fill=Volume), alpha=0.8,fill = "cyan4") +
  labs(x = "Volume", y = "Frequency",
       title = "Distribution of Volume")+xlim(0,0.3e+08)

g3 <- ggplot(stocks, aes(x = VWAP)) +
  geom_density(aes(fill=VWAP), alpha=0.8,fill = "yellowgreen") +
  labs(x = "VWAP", y = "Frequency",
       title = "Distribution of VWAP")+xlim(0,2500)

ggarrange(g1, g2, g3, labels = c("A", "B", "C"), ncol = 1, nrow = 3)

######################################################

g <- ggplot(stocks, aes(x=Turnover, y=VWAP)) +
  geom_smooth(method="lm") +geom_count(col="black", show.legend=F)
plot(g)

g <- ggplot(stocks, aes(x=Volume, y=VWAP)) +
  geom_smooth(method="lm") +geom_count(col="black", show.legend=F)
plot(g)

g <- ggplot(stocks, aes(x=Volume, y=Turnover)) +
  geom_smooth(method="lm") +geom_count(col="black", show.legend=F)
plot(g)

#####################################

ggplot(stocks,
       aes(x = Volume, y = Turnover,
           color = Symbol)) + geom_point(alpha = .6) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Volume by TurnOver") + ylim(0,5.0e+15)+xlim(0,2e+08)

######################################
#1 Volume per symbol
Volume_per_symbol <- aggregate(stocks$Volume, by=list(stocks$Symbol), FUN=mean)
colnames(Volume_per_symbol) <- c('Symbol', 'Volume') # Change column names
Volume_per_symbol <- Volume_per_symbol[order(Volume_per_symbol$Volume), ] # Sort

ggplot(Volume_per_symbol, aes(x=Symbol, y=Volume)) +
  geom_bar(stat="identity", width=.5, fill="blue") +
  labs(title="Mean Volume shared by Each Company", subtitle = "") +
  scale_y_continuous()

# Turnover per symbol
Turnover_per_symbol <- aggregate(stocks$Turnover, by=list(stocks$Symbol), FUN=mean)
colnames(Turnover_per_symbol) <- c('Symbol', 'Turnover') # Change column names
Turnover_per_symbol <- Turnover_per_symbol[order(Turnover_per_symbol$Turnover), ] # Sort

ggplot(Turnover_per_symbol, aes(x=Symbol, y=Turnover)) +
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  labs(title="Mean Turnover shared by Each Company", subtitle = "") +
  scale_y_continuous()

#4 Density Plots
g1 <- ggplot(stocks, aes(Volume)) +
  geom_density(aes(fill=factor(Symbol)), alpha=0.8) +
  labs(title="Density plot", subtitle="Volume Grouped by Symbol",
       x="Volume", fill="Symbol")+xlim(0,1e+07)
ggarrange(g1, labels = c("A"), ncol = 1, nrow = 1)

g2 <- ggplot(stocks, aes(Turnover)) +
  geom_density(aes(fill=factor(Symbol)), alpha=0.8) +
  labs(title="Density plot", subtitle="Turnover Grouped by Symbol",
       x="Turnover", fill="Symbol")+xlim(0,5e+14)

ggarrange(g2, labels = c("A"), ncol = 1, nrow = 1)

#################################

g1 <- ggplot(stocks, aes(x = Volume, y = High, color = Symbol)) +
  geom_point() + scale_y_continuous() +
  labs(title = "High stock prices wrt Volume", x = "Volume", y = "High")

g2 <- ggplot(stocks, aes(x = Volume, y = Low, color = Symbol)) +
  geom_point() + scale_y_continuous() +
  labs(title = "Low stock prices wrt Volume", x = "Volume", y = "Low") + 
  scale_color_brewer(palette = "Set1")

ggarrange(g1, g2, labels = c("A", "B"), ncol = 1, nrow = 2)

################################

g1 <- ggplot(stocks, aes(x = Turnover, y = High, color = Symbol)) +
  geom_point() + scale_y_continuous() +
  labs(title = "High stock prices wrt Turnover", x = "Turnover", y = "High")+ylim(0,2000)+xlim(0,5e+15)

g2 <- ggplot(stocks, aes(x = Turnover, y = Low, color = Symbol)) +
  geom_point() + scale_y_continuous() +
  labs(title = "Low stock prices wrt Turnover", x = "Turnover", y = "Low") + 
  scale_color_brewer(palette = "Set1")+ylim(0,2000)+xlim(0,5e+15)

ggarrange(g1, g2, labels = c("A", "B"), ncol = 1, nrow = 2)


##################################

m<-aggregate(stocks$Volume, by=list(stocks$Symbol), FUN=mean)
View(m)

##################################


###################################

#4 Violin plot

g2 <- ggplot(stocks, aes(Symbol, High)) +
  geom_violin() + scale_y_continuous() +
  labs(title="Symbol vs High", subtitle="Violin Plot",
       x="Symbol", y="High")+ylim(0,2000)

ggarrange(g2, labels = c("A"), ncol = 1, nrow = 1)

g2 <- ggplot(stocks, aes(Symbol, Low)) +
  geom_violin() + scale_y_continuous() +
  labs(title="Symbol vs Low", subtitle="Violin Plot",
       x="Symbol", y="Low")+ylim(0,2000)

ggarrange(g2, labels = c("A"), ncol = 1, nrow = 1)


