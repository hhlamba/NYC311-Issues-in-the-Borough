install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("waffle")
library(waffle)
####################################################################
# DATA SELECTION AND CLEANING 
####################################################################
Clean_New_York <- function(file,df){
  file <- file.choose()
  df <- read.csv(file, header= TRUE, stringsAsFactors = FALSE)
  nyc <- df[,-c(4,5,8,9,10,11,12,13,14,15,16,18,19,22,23,24,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,53)]
  nyc <- nyc[-as.numeric(rownames(subset(nyc, is.na(nyc$Latitude) == TRUE))),]
  nyc <- nyc[nyc$Borough != "Unspecified",]
  unique(nyc$Complaint.Type)
  nyc$complaint_group <- nyc$Complaint.Type
  nyc$complaint_group[nyc$Complaint.Type == "Noise - Street/Sidewalk" 
                      | nyc$Complaint.Type ==  "Noise - Commercial" 
                      | nyc$Complaint.Type ==  "Noise - Vehicle" 
                      | nyc$Complaint.Type ==  "Noise - House of Worship" 
                      | nyc$Complaint.Type == "Noise - Park"] <- "Noise"
  nyc$complaint_group[nyc$Complaint.Type == "Posting Advertisement" 
                      | nyc$Complaint.Type ==  "Graffiti"] <- "Spraying on the Wall" 
  nyc$CrtDte <- strptime(nyc$Created.Date, "%m/%d/%Y %H:%M:%S %p")
  nyc$ClsDte <- strptime(nyc$Closed.Date , "%m/%d/%Y %H:%M:%S %p" )
  #date.time.formatted <- strptime(nyc$Created.Date, "%m/%d/%Y %H:%M:%S %p")
  #nyc$CrtDte <- as.POSIXct(date.time.formatted, origin = "1960-01-17")
  #date.time.formatted <- strptime(nyc$Closed.Date , "%m/%d/%Y %H:%M:%S %p" )
  #nyc$ClsDte <- as.POSIXct(date.time.formatted, origin = "1960-01-17")
  rownames(nyc) <- NULL
  nyc <- nyc[-as.numeric(rownames(subset(nyc, is.na(nyc$CrtDte) == TRUE))),]
  rownames(nyc) <- NULL
  nyc <- nyc[-as.numeric(rownames(subset(nyc, is.na(nyc$ClsDte) == TRUE))),]
  rownames(nyc) <- NULL
  return(nyc)
}
nyc <- Clean_New_York()
nyc$CrtDte <- strptime(nyc$Created.Date, "%m/%d/%Y %I:%M:%S %p")
nyc$ClsDte <- strptime(nyc$Closed.Date, "%m/%d/%Y %I:%M:%S %p")
nyc$hour <- as.numeric(substr(nyc$CrtDte,12,13))
nyc$CRT_DOW <- factor(weekdays(as.Date(substr(nyc$CrtDte, 1,10))), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
nyc$resolution_hours <- difftime(nyc$ClsDte, nyc$CrtDte, units = "hours")

####################################################################
# POSTER PLOT 1 - CIRCULAR PLOT BOROUGH CALLS
####################################################################

M <- aggregate(nyc$Complaint.Type
          , list(nyc$Borough)
          , FUN = length)

colnames(M) <- c("Borough", "value")
M <- M[order(-M$value),]
sum(M$value)
M$Perc <- 0
M$Perc <- round(M$value * 100 /  360389)
M$Perc

# Create dataset

data <- M

# Add lines to the initial dataset
empty_bar=4
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$Borough), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$Borough=rep(levels(data$Borough), each=empty_bar)
data=rbind(data, to_add)
data=data %<% arrange(Perc)
data$id=seq(1, nrow(data))

# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# Make the plot
p = ggplot(data, aes(x=as.factor(id), y=value, fill=Borough)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-10000,120000) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  geom_text(data=label_data, aes(x=id, y=value+1000, label= Borough, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p

####################################################################
# POSTER PLOT 2 - CIRCULAR PLOT COMPLAINTS
#####################################################################

M <- aggregate(nyc$Complaint.Type
               , list(nyc$complaint_group)
               , FUN = length)

colnames(M) <- c("Complaints", "value")
M <- M[order(-M$value),]
sum(M$value)

M$Perc <- 0
M$Perc <- round(M$value * 100 /  360389)
sum(M$Perc)

M <- M[M$Perc > 5,]
# Create dataset

data <- M

# Add lines to the initial dataset
empty_bar=4
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$Complaints), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$Complaints=rep(levels(data$Complaints), each=empty_bar)
data=rbind(data, to_add)
data=data %<% arrange(Perc)
data$id=seq(1, nrow(data))

# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# Make the plot
p = ggplot(data, aes(x=as.factor(id), y=value, fill=Complaints)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-10000,120000) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  geom_text(data=label_data, aes(x=id, y=value+1000, label= Complaints, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p



####################################################################
# POSTER PLOT 3 - DENSITY PLOTS
####################################################################

M <- aggregate(nyc$complaint_group
               , list(nyc$Borough, factor(months(as.Date(substr(nyc$CrtDte, 1,10))),levels = month.name))
               , FUN = length)




M <- M[order(M$Group.1),]
M
colnames(M) <- c("Borough", "Month", "value")
aggregate(nyc$Complaint.Type
          , list(nyc$Borough)
          , FUN = length)


M$Perc <- 0
M$Perc[M$Borough == "BRONX"] <- round(M$value[M$Borough == "BRONX"] * 100 /  49035)
M$Perc[M$Borough == "BROOKLYN"] <- round(M$value[M$Borough == "BROOKLYN"] * 100 /  118609)
M$Perc[M$Borough == "MANHATTAN"] <- round(M$value[M$Borough == "MANHATTAN"] * 100 /  76758)
M$Perc[M$Borough == "QUEENS"] <- round(M$value[M$Borough == "QUEENS"] * 100 /  100665)
M$Perc[M$Borough == "STATEN ISLAND"] <- round(M$value[M$Borough == "STATEN ISLAND"] * 100 /  15322)
M$Perc[M$Borough == "BRONX"]
M$Perc[M$Borough == "BROOKLYN"]
M$Perc[M$Borough == "QUEENS"]
M$Perc[M$Borough == "MANHATTAN"]
M$Perc[M$Borough == "STATEN ISLAND"]


#***IMP
plot(density(M$Perc[M$Borough == "BRONX"]), col ="red", xaxt = "n", ylim = c(0,0.5))
lines(density(M$Perc[M$Borough == "BROOKLYN"]), col = "blue")
lines(density(M$Perc[M$Borough == "QUEENS"]), col = "orange")
lines(density(M$Perc[M$Borough == "MANHATTAN"]), col = "darkgreen")
lines(density(M$Perc[M$Borough == "STATEN ISLAND"]), col = "darkcyan")
axis(side = 1, at = seq(1,12,1), labels = month.name, pos = 0, las = 2)



####################################################################
# POSTER PLOT 4 - Weekly Distributions
####################################################################


nyc$CRT_DOW <- factor(weekdays(as.Date(substr(nyc$CrtDte, 1,10))), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

M <- aggregate(nyc$complaint_group
               , list(nyc$CRT_DOW, nyc$complaint_group)
               , FUN = length)
colnames(M) <- c("Weekdays", "Complaints","NoofCalls")
N <- M[M$Complaints == "Noise" | M$Complaints == "Illegal Parking" | M$Complaints == "Blocked Driveway" | M$Complaints== "Derelict Vehicle",]
N <- N[order(N$Complaints, N$Weekdays),]
N <- N[,c(2,1,3)]
N


par(bty = "n", xpd = NA)

plot(1:7, N$NoofCalls[N$Complaints == "Noise"], col = "#822121", type = "l", xaxt = "n", yaxt = "n",lwd = 2, las = 1, ylab = NA, xlab = NA, ylim = c(0,25000))
points(1:7, N$NoofCalls[N$Complaints == "Noise"], col = "#822121", pch = 16, cex = 2)
lines(1:7, N$NoofCalls[N$Complaints == "Illegal Parking"], xaxt = "n", col = "red", type = "l", lwd = 2)
points(1:7, N$NoofCalls[N$Complaints == "Illegal Parking"], col = "red", pch = 16, cex = 2)
lines(1:7, N$NoofCalls[N$Complaints == "Blocked Driveway"], xaxt = "n", col = "blue", type = "l", lwd = 2)
points(1:7, N$NoofCalls[N$Complaints == "Blocked Driveway"], col = "blue", pch = 16, cex = 2)
lines(1:7, N$NoofCalls[N$Complaints == "Derelict Vehicle"], xaxt = "n", col = "green", type = "l", lwd = 2)
points(1:7, N$NoofCalls[N$Complaints == "Derelict Vehicle"], col = "green", pch = 16, cex = 2)

axis(side = 1, at= seq(1,7,1), labels = levels(N$Weekdays), pos = seq(1,7,1) )
axis(side = 2, at = seq(0,26000,1000), labels = seq(0,26000,1000), pos = 0.75, las = 1, hadj =1)

####################################################################
# POSTER PLOT 5 - Hourly Distributions
####################################################################

M <- aggregate(nyc$complaint_group
          , list(nyc$hour, nyc$complaint_group)
          , FUN = length)
colnames(M) <- c("Hours", "Complaints","NoofCalls")
M <- M[order(M$Complaints, M$Hours),]
M <- M[,c(2,1,3)]
N <- M[M$Complaints == "Noise" | M$Complaints == "Illegal Parking" | M$Complaints == "Blocked Driveway" | M$Complaints== "Derelict Vehicle",]

par(bty = "n", xpd = 'NA')
plot(N$Hours[N$Complaints == "Noise"], N$NoofCalls[N$Complaints == "Noise"], xaxt = "n",yaxt = 'n',   col = "#822121", type = "l", lwd = 2, las = 1, xlab = NA, ylab = NA)
points(N$Hours[N$Complaints == "Noise"], N$NoofCalls[N$Complaints == "Noise"], col = "#822121", pch = 16, cex = 2)
lines(N$Hours[N$Complaints == "Illegal Parking"], N$NoofCalls[N$Complaints == "Illegal Parking"], xaxt = "n", col = "red", type = "l", lwd = 2)
points(N$Hours[N$Complaints == "Illegal Parking"], N$NoofCalls[N$Complaints == "Illegal Parking"], col = "red", pch = 16, cex = 2)
lines(N$Hours[N$Complaints == "Blocked Driveway"], N$NoofCalls[N$Complaints == "Blocked Driveway"], xaxt = "n", col = "blue", type = "l", lwd = 2)
points(N$Hours[N$Complaints == "Blocked Driveway"], N$NoofCalls[N$Complaints == "Blocked Driveway"], col = "blue", pch = 16, cex = 2)
lines(N$Hours[N$Complaints == "Derelict Vehicle"], N$NoofCalls[N$Complaints == "Derelict Vehicle"], xaxt = "n", col = "green", type = "l", lwd = 2)
points(N$Hours[N$Complaints == "Derelict Vehicle"], N$NoofCalls[N$Complaints == "Derelict Vehicle"], col = "green", pch = 16, cex = 2)

axis(side = 1, at = seq(0,23,1), labels = seq(0,23,1), pos = -1, las = 1, hadj = 1)
axis(side = 2, at = seq(0,17000,1000), labels = seq(0,17000,1000), pos = -1, las = 1, hadj =1)




####################################################################
# POSTER PLOT 6 - Hourly Distributions
####################################################################

M <- aggregate(nyc$resolution_hours
               , list(nyc$complaint_group)
               , FUN = mean)
M <- M[order(-M$x),]
colnames(M) <- c("Complaints", "Mean Resolution Hours")
N <- M[M$Complaints == "Noise" | M$Complaints == "Illegal Parking" | M$Complaints == "Blocked Driveway" | M$Complaints== "Derelict Vehicle",]

barplot(N$`Mean Resolution Hours`)
N <- N[order(-N$`Mean Resolution Hours`),]
N$`Mean Resolution Hours` <- as.numeric(N$`Mean Resolution Hours`)
N$Complaints <- factor(N$Complaints, levels <- N$Complaints)

N <- nyc[nyc$complaint_group == "Noise" | nyc$complaint_group == "Illegal Parking" | nyc$complaint_group == "Blocked Driveway" | nyc$complaint_group== "Derelict Vehicle",]


ggplot(N, aes(x=as.factor(N$complaint_group), y=N$resolution_hours)) + geom_boxplot(fill="slateblue", alpha=0.2) + ylim(0,6)


####################################################################
# POSTER PLOT 7 - Waffle PLots
####################################################################

# open vs close
prop.table(table(nyc$Status)) * 100
waffle(prop.table(table(nyc$Status))*100)



########################################################################################################################################
#****************************************END OF POSTER PLOTS********************************
########################################################################################################################################












####################################################################
# Addtional Plots
####################################################################

# Mapimpact

YMIN <- min(nyc$Latitude)
YMAX <- max(nyc$Latitude)

XMIN <- min(nyc$Longitude)
XMAX <- max(nyc$Longitude)



unique(nyc$complaint_group)

?map_data
us <- map_data('state')
dummyDF <- data.frame(state.name, stringsAsFactors=FALSE)
dummyDF$state <- tolower(dummyDF$state.name)
map.simple <- NULL
map.simple <- ggplot(nyc, aes(map_id = nyc)) 
map.simple <- map.simple + geom_map(map = us, fill = "white", color = "black") 
map.simple <- map.simple + expand_limits(x = nyc$Longitude, y= nyc$Latitude) 
map.simple <- map.simple + coord_map() + xlim (XMIN, XMAX) + ylim(YMIN, YMAX)
map.simple <- map.simple + ggtitle("Basic Map of United States") 
map.simple <- map.simple + geom_point(aes(x= nyc$Longitude, y = nyc$Latitude, fill = nyc$complaint_group))
map.simple
                                      
map('state', region = c('new york', c('Queens', 'Bronx', 'Brooklyn', "Manhattan", "Staten Island")), xlim = c(XMIN, XMAX), ylim = c(YMIN, YMAX)) 
points(nyc$Longitude, nyc$Latitude)



noiseSub <- nyc[nyc$complaint_group == "Noise",]
noiseSub$Complaint.Type <- gsub("Noise - ", "",noiseSub$Complaint.Type)

fill_color <- c( "#FF8900", "#DDFB00", "#A702C4", "#0583C0", "#2F2F2FF")
noiseSub$col[noiseSub$Complaint.Type == "Street/Sidewalk"] <- fill_color[1]
noiseSub$col[noiseSub$Complaint.Type == "Commercial"] <- fill_color[2]
noiseSub$col[noiseSub$Complaint.Type == "House of Worship"] <- fill_color[3]
noiseSub$col[noiseSub$Complaint.Type == "Vehicle"] <- fill_color[4]
noiseSub$col[noiseSub$Complaint.Type == "Park"] <- fill_color[5]


noisyPlot <- ggplot(noiseSub, aes(map_id = noiseSub$City))
noisyPlot <- noisyPlot + geom_map(map = noiseSub, color="black")
noisyPlot <- noisyPlot + expand_limits(x=noiseSub$Longitude, y=noiseSub$Latitude)
noisyPlot <- noisyPlot + coord_map() + xlim (min(noiseSub$Longitude),max(noiseSub$Longitude)) + ylim(min(noiseSub$Latitude),max(noiseSub$Latitude))
noisyPlot <- noisyPlot + ggtitle("Le Noisy Plot")
noisyPlot <- noisyPlot + geom_point(aes(x=noiseSub$Longitude, y=noiseSub$Latitude, color= col), size=0.8)
noisyPlot


BlockD <- nyc[nyc$Complaint.Type == "Blocked Driveway",]

BD_map <- NULL
BD_map <- ggplot(BlockD, aes(map_id=BlockD$City))
BD_map <- BD_map + geom_map(map=BlockD, color="black")
BD_map <- BD_map + expand_limits(x=BlockD$Longitude, y=BlockD$Latitude)
BD_map <- BD_map + coord_map() + xlim(min(BlockD$Longitude), max(BlockD$Longitude)) + ylim(min(BlockD$Latitude), max(BlockD$Latitude))
BD_map <- BD_map + ggtitle("Blocked Driveway")
BD_map <- BD_map + geom_point(aes(x=BlockD$Longitude, y=BlockD$Latitude
                                  , alpha= 0.2)
                              , color="#821122"
                              , size=0.5)
BD_map



Illy <- nyc[nyc$Complaint.Type == "Illegal Parking", ]

IllyM <- ggplot(Illy, aes(map_id=Illy$City))
IllyM <- IllyM + geom_map(map=Illy, color="black")
IllyM <- IllyM + expand_limits(x=Illy$Longitude, y=Illy$Latitude)
IllyM <- IllyM + coord_map() + xlim(min(Illy$Longitude), max(Illy$Longitude)) + ylim(min(Illy$Latitude), max(Illy$Latitude))
IllyM <- IllyM + ggtitle("Illy illy")
IllyM <- IllyM + geom_point(aes(x=Illy$Longitude, y=Illy$Latitude
                                , alpha= 0.2)
                            , color="#005C55"
                            , size=0.5) 
IllyM






noiseSub$col[noiseSub$Complaint.Type == "Noise - Street/Sidewalk"] <- "#FF0000FF" 
noiseSub$col[noiseSub$Complaint.Type == "Noise - Commercial"] <- "#FF5500FF" 
noiseSub$col[noiseSub$Complaint.Type == "Noise - Vehicle"] <- "#FFAA00FF" 
noiseSub$col[noiseSub$Complaint.Type == "Noise - Park"] <- "#FFFF00FF" 
noiseSub$col[noiseSub$Complaint.Type == "Noise - House of Worship"] <- "#FFFF80FF"

noisyPlot <- ggplot(noiseSub, aes(map_id = noiseSub$City))
noisyPlot <- noisyPlot + geom_map(map = noiseSub, color="black")
noisyPlot <- noisyPlot + expand_limits(x=noiseSub$Longitude, y=noiseSub$Latitude)
noisyPlot <- noisyPlot + coord_map() + xlim (min(noiseSub$Longitude),max(noiseSub$Longitude)) + ylim(min(noiseSub$Latitude),max(noiseSub$Latitude))
noisyPlot <- noisyPlot + ggtitle("Le Noisy Plot")
noisyPlot <- noisyPlot + geom_point(aes(x=noiseSub$Longitude, y=noiseSub$Latitude
                                        , alpha=0.2
)
, color= noiseSub$col
, size=0.5)
noisyPlot



########################### Circular Bar Plot for all Boroughs #########################


# Create dataset
M <- aggregate(nyc$Complaint.Type
               , list(nyc$complaint_group, factor(nyc$Borough ,levels = c("BROOKLYN", "QUEENS", "MANHATTAN", "BRONX", "STATEN ISLAND")))
               , FUN = length)
M
colnames(M) <- c( "Complaints", "Borough", "value")
M <- M[order(M$Borough,-M$value),]
M <- M[,c(2,1,3)]
aggregate(nyc$Complaint.Type
          , list(nyc$Borough)
          , FUN = length)
M$Perc <- 0
M$Perc[M$Borough == "BRONX"] <- round(M$value[M$Borough == "BRONX"] * 100 /  49035)
M$Perc[M$Borough == "BROOKLYN"] <- round(M$value[M$Borough == "BROOKLYN"] * 100 /  118609)
M$Perc[M$Borough == "MANHATTAN"] <- round(M$value[M$Borough == "MANHATTAN"] * 100 /  76758)
M$Perc[M$Borough == "QUEENS"] <- round(M$value[M$Borough == "QUEENS"] * 100 /  100665)
M$Perc[M$Borough == "STATEN ISLAND"] <- round(M$value[M$Borough == "STATEN ISLAND"] * 100 /  15322)

M <- M[M$Perc > 5,]




data <- M

# Add lines to the initial dataset
empty_bar=0
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$Borough), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$Borough=rep(levels(data$Borough), each=empty_bar)
data=rbind(data, to_add)
data=data %>% arrange(Borough)
data$id=seq(1, nrow(data))

# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# Make the plot
p = ggplot(data, aes(x=as.factor(id), y=Perc, fill=Borough)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-5,100) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  geom_text(data=label_data, aes(x=id, y=Perc+10, label= Complaints, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p


##############################################################################
# Date Related Plots
##############################################################################

par(mar = c(5,4,3,2), bty = "o")
M <- sort(table(nyc$CRT_DOW), decreasing = TRUE, xpd = FALSE)
plot(M, type= "l", ylim = c(0, 60000), las = 1)

points(1, M[1], pch = 16, cex = 2, col = "red")
points(2, M[2], pch = 16, cex = 2,col = "red")
points(3, M[3], pch = 16, cex = 2,col = "red")
points(4, M[4], pch = 16, cex = 2,col = "red")
points(5, M[5], pch = 16, cex = 2, col = "red")
points(6, M[6], pch = 16, cex = 2, col = "red")
points(7, M[7], pch = 16, cex = 2, col = "red")

nyc$CLS_DOW <- weekdays(as.Date(substr(nyc$ClsDte, 1,10)))



M <- sort(table(nyc$CLS_DOW), decreasing = TRUE, xpd = FALSE)
segments(1,M[1],2, M[2], lty = 1, lwd = 2, col = "grey")
segments(2,M[2],3, M[3], lty = 1, lwd = 2, col = "grey")
segments(3,M[3],4, M[4], lty = 1, lwd = 2, col = "grey")
segments(4,M[4],5, M[5], lty = 1, lwd = 2, col = "grey")
segments(5,M[5],6, M[6], lty = 1, lwd = 2, col = "grey")
segments(6,M[6],7, M[7], lty = 1, lwd = 2, col = "grey")

points(1, M[1], pch = 16, cex = 2, col = "green")
points(2, M[2], pch = 16, cex = 2, col = "green")
points(3, M[3], pch = 16, cex = 2, col = "green")
points(4, M[4], pch = 16, cex = 2, col = "green")
points(5, M[5], pch = 16, cex = 2, col = "green")
points(6, M[6], pch = 16, cex = 2, col = "green")
points(7, M[7], pch = 16, cex = 2, col = "green")

legend( "topright", pch = 16, cex = 2, legend = c("Open","Close"), col = c("red","green"))
#####################################################
# Month
#####################################################

M <- aggregate(nyc$complaint_group
               , list(nyc$Borough, factor(months(as.Date(substr(nyc$CrtDte, 1,10))),levels = month.name))
               , FUN = length)




M <- M[order(M$Group.1),]
M
colnames(M) <- c("Borough", "Month", "value")
aggregate(nyc$Complaint.Type
          , list(nyc$Borough)
          , FUN = length)


M$Perc <- 0
M$Perc[M$Borough == "BRONX"] <- round(M$value[M$Borough == "BRONX"] * 100 /  49035)
M$Perc[M$Borough == "BROOKLYN"] <- round(M$value[M$Borough == "BROOKLYN"] * 100 /  118609)
M$Perc[M$Borough == "MANHATTAN"] <- round(M$value[M$Borough == "MANHATTAN"] * 100 /  76758)
M$Perc[M$Borough == "QUEENS"] <- round(M$value[M$Borough == "QUEENS"] * 100 /  100665)
M$Perc[M$Borough == "STATEN ISLAND"] <- round(M$value[M$Borough == "STATEN ISLAND"] * 100 /  15322)
M$Perc[M$Borough == "BRONX"]
M$Perc[M$Borough == "BROOKLYN"]
M$Perc[M$Borough == "QUEENS"]
M$Perc[M$Borough == "MANHATTAN"]
M$Perc[M$Borough == "STATEN ISLAND"]

par(mar = c(5,4,4,2), bty = "n")
fill_color <- terrain.colors(5)
plot(1:12, M$Perc[M$Borough == "BRONX"], col = fill_color[1], pch = 16, ylim = c(0,12), type = "l", xaxt = "n", lwd = 2)
points(1:12, M$Perc[M$Borough == "BROOKLYN"], col = fill_color[2], pch = 16, type = "l", lwd = 2)
points(1:12, M$Perc[M$Borough == "MANHATTAN"], col = fill_color[3], pch = 16, type = "l", lwd = 2)
points(1:12, M$Perc[M$Borough == "QUEENS"], col = fill_color[4], pch = 16, type = "l", lwd = 2)
points(1:12, M$Perc[M$Borough == "STATEN ISLAND"], col = fill_color[5], pch = 16, type = "l", lwd = 2)
axis(side = 1, at = seq(1,12,1), labels = month.name, pos = seq(1,12,1))





