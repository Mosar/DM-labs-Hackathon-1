#################################### 
# READING PAKISTAN DATA
#################################### 

# Installing package for _readHTMLTable_ function
install.packages("XML")
library(XML)


# Parsing tables from sited
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


# Table for Locations popularity
loc_data <- as.data.frame(table(data$Location))
loc_data <- loc_data[order(loc_data$Freq), ]


# See deaths in different week days and month for militans and civilians
aggregate(militants ~ wday, FUN=mean, data)
aggregate(civilians ~ wday, FUN=mean, data)

aggregate(militants ~ month, FUN=mean, data)
aggregate(civilians ~ month, FUN=mean, data)
