data <- read.csv(file.choose(), header = T)
ot <- data.frame(data)
summary(ot)

#1.Univariate Bar Plot
data %>%
  ggplot(aes(x=DayofMonth)) +
  geom_bar(fill='Blue',color='Black') +
  ggtitle("Flight Frequency for particular date of month")

#2.Interactive Geomap
library(dplyr)
ot1<-ot %>%
  group_by(OriginStateName) %>%
  summarise(count=n())
library(highcharter)
highchart() %>%
  hc_add_series_map(usgeojson, ot1,
                    name = 'OriginStateName',
                    value = 'count',
                    joinBy = c('woename', 'OriginStateName')) %>%
  hc_mapNavigation(enabled = T)

#3.BiVariate Scatter Plot
plot(data$DepDelayMinutes, data$ArrDelayMinutes, col='red',
     xlab= 'Dep Delay Min',
     ylab= 'Arr delay Min',
     main ='Graph between Arrival and Departure delay minutes')

#4.Multivariate Scatter plot
library(scatterplot3d)
scatterplot3d(data$DepDelayMinutes, data$ArrDelayMinutes, data$Distance,
              color="steelblue",
              main="Depature delay vs Arrival Delay vs Distance",
              xlab = "DepDelayMinutes",
              ylab = "ArrDelayMinutes",
              zlab = "Distance")
#5.Interactive Boxplot
hcboxplot(data$Distance,data$OriginState,color="Red")

#6.psych plot
library("psych")
pairs.panels(data[27:33])

#7.Network Plot
library(igraph)
df <- data.frame(ot$OriginState, ot$DestState)
net1 <- graph.data.frame(df, directed = T)
V(net1)$label <- V(net1)$name
V(net1)$degree <- degree(net1)
plot(net1, vertex.color = rainbow(53),
     edge.arrow.size= 0.1,
     layout = layout.kamada.kawai(net1),
     main = "Origin and Destination states Connectivity")


