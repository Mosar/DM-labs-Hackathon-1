#################################### 
# READING PAKISTAN DATA
#################################### 

# Installing package for _readHTMLTable_ function
install.packages("XML")
library(XML)


# Parsing tables from site
url <- "http://securitydata.newamerica.net/drones/pakistan/analysis?page=0"
data <- as.data.frame(readHTMLTable(url, stringsAsFactors = FALSE))

for(i in 1:19){
  url <- paste("http://securitydata.newamerica.net/drones/pakistan/analysis?page=",i,sep="")
  data <- rbind(data, as.data.frame(readHTMLTable(url, stringsAsFactors = FALSE)))
  print(i) #see your progress
}


# Renaming columns
names(data)<-c("Date", "Location", "Total.killed", "Militants.killed", "Civilians.killed","Unknown.killed","Target.organization")


# Adding new variables: wday (week day), mday (day and month), year

for (i in 1:nrow(data)){
  data$wday[i] <- unlist(strsplit(data$Date[i], ", "))[1]
  data$mday[i] <- unlist(strsplit(data$Date[i], ", "))[2]
  data$year[i] <- unlist(strsplit(data$Date[i], ", "))[3]
}

# Adding _month_ variable
for (i in 1:nrow(data)){
  data$month[i] <- unlist(strsplit(data$mday[i], " "))[1]
}

# Computing means from killed people data
# New variables: total for Total.killed, militants for Militants.killed etc.

for (i in 1:nrow(data)){
  words <- strsplit(data$Total.killed[i], " ") #divide string by spaces
  words_vector <- unlist(words) # unlist it
  indices <- grep("[0-9]", words_vector ) # find indices of numeric elements
  data$total[i] <- mean(as.numeric(words_vector[indices])) #make real number from characters and average it
  
  data$militants[i] <- mean(as.numeric(unlist(strsplit(data$Militants.killed[i], " "))[grep("[0-9]", unlist(strsplit(data$Militants.killed[i], " ")) )]))
  data$civilians[i] <- mean(as.numeric(unlist(strsplit(data$Civilians.killed[i], " "))[grep("[0-9]", unlist(strsplit(data$Civilians.killed[i], " ")) )]))
  data$unknown[i] <- mean(as.numeric(unlist(strsplit(data$Unknown.killed[i], " "))[grep("[0-9]", unlist(strsplit(data$Unknown.killed[i], " ")) )]))
}

####################################################
# parsing coordinates of locations
####################################################

# scan
coord <- scan("http://securitydata.newamerica.net/drones/pakistan/analysis?page=0", what="character", nmax=388, sep = "^", skip=338)
coord <- gsub("(^\\s+.*\\[)|(\\s+$)|(\\s+\\];)|(\\])","", coord, perl=TRUE)
coord<- gsub("(,<h2>.*00\\\">)|(</span></a>.*Location</strong>)|(<li>)",",", coord, perl=TRUE)
coord<- gsub(".</li></ul>,.*,","",coord,perl=T)
coord=gsub("(:\\s)|(</li>)","'",coord,perl=T)
write(, file="my.csv")
coordinates <- as.data.frame(read.table("my.csv", header=FALSE, sep=",", quote="\"'", dec=".", col.names =c ("latitude", "longitude","mday","year","Location","Total.killed"), colClasses = c("numeric", "numeric","character","character","character","character"), fill = FALSE))

# merge coodrinates with data
data_loc <- aggregate(cbind(total,militants,civilians,unknown) ~ Location, FUN=sum, na.rm=TRUE, na.action=NULL, data=data)
coord_loc <- aggregate(cbind(longitude, latitude) ~ Location, FUN=mean, data=coordinates)
coord_data <- merge(data_loc, coord_loc, by="Location")
# create __proportion of innocent victims__ ("bombing fail") variable:
coord_data$fail <- (coord_data$civilians) /  (coord_data$total)


######################
#plotting with ggplot2 library using Open street Map layer as overlay
#that is a simple example of large drawing skills in R. 
#######################



#that is packages we need 
install.packages("ggmap")
library(ggmap)
install.packages("ggplot2")
library(ggplot2)

#getting base point layer
g<-ggplot(data=coord_data,aes(longitude,latitude))
#getting overlay;zoom value is an empiric number,lon,lat is a coordinates of
# map center. using any avaliable open source as overlay is ok.coordinate
#system is wgs-84;projection is none,but could be assigned.
map<-get_map(location=c(lon = 70.4,lat = 33.5),source="google",zoom=8)
mapPoints<-ggmap(map)

# Final plot with size of the bombing locations representing
# total number of people killed and color representing proportion of 
# killed civilians
p <- mapPoints+geom_point(aes(x=longitude,y=latitude,size=total,color=fail),data=coord_data)
p + scale_colour_gradient(low="blue", high="red")
