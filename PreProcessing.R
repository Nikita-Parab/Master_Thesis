#Calling Libraries
library(rjson)
library(rlist)
library(tidyr)
library(chron)
library(dplyr)
library(ggplot2)
setwd("C:/Users/nikita parab/Desktop/SEMESTER 3/ICT Solution/Raw_Data/all-rnr-annotated-threads/ferguson-all-rnr-threads")
events <- list("charliehebdo","ferguson","germanwings-crash","gurlitt","ottawashooting","putinmissing","sydneysiege","prince-toronto")
for (z in 1:length(events)) {
  non_rumor_dir <- list.dirs(path = paste("C:/Users/nikita parab/Desktop/SEMESTER 3/ICT Solution/Raw_Data/all-rnr-annotated-threads/",events[z],"-all-rnr-threads/non-rumours",sep = ""), full.names = TRUE, recursive = FALSE)
  non_rumor_file <- list.dirs(path = paste("C:/Users/nikita parab/Desktop/SEMESTER 3/ICT Solution/Raw_Data/all-rnr-annotated-threads/",events[z],"-all-rnr-threads/non-rumours",sep =  ""), full.names = FALSE, recursive = FALSE)
  rumor_dir <- list.dirs(path = paste("C:/Users/nikita parab/Desktop/SEMESTER 3/ICT Solution/Raw_Data/all-rnr-annotated-threads/",events[z],"-all-rnr-threads/rumours",sep = ""), full.names = TRUE, recursive = FALSE)
  rumor_file <- list.dirs(path = paste("C:/Users/nikita parab/Desktop/SEMESTER 3/ICT Solution/Raw_Data/all-rnr-annotated-threads/",events[z],"-all-rnr-threads/rumours",sep = ""), full.names = FALSE, recursive = FALSE)

#***************************************************NON-RUMOR*****************************************************
#creating a list to get the source tweets from files
source_tweet <- list()
u <- list()

for (i in 1:length(non_rumor_file)) {
  a <- paste(non_rumor_dir[i],"/source-tweets/",non_rumor_file[i],".json", sep = "")
  source_tweet[i] <- a
}

k <- 1
j <- list()
for (x in 1:length(source_tweet)) {
  #Extracting timestamp of the source tweet
  s <- source_tweet[[x]]
  source <- rjson::fromJSON(file = s)
  s_list  <- source["created_at"]
  source_df <- as.data.frame(s_list)
  
  #Extracting timestamp of the reaction tweets
  r <- paste(non_rumor_dir[[x]],"reactions", sep = "/")
  file_names <- list.files(path = r ,pattern = "*.json", full.names = TRUE)
  reaction <- lapply(file_names, function(n) rjson::fromJSON(file=n))
  if(length(reaction)== 0){
    j[[k]] <- r
    k <- k+1
    next
  }
  created_at <- sapply(1:length(reaction), function(y) reaction[[y]][["created_at"]])
  reaction_df <- as.data.frame(created_at)
  
  #Combining timestamps of the source and the reaction tweets
  combine <- rbind(source_df, reaction_df)
  
  #Separating timestamp
  separated <- separate(combine, created_at, c("Day","Month","Date","Hour","Minute","Second","Plus","Year"), extra = 'drop')
  separated <- separated[,c(-1,-7)]
  separated <- unite(separated, "Date", c("Month", "Date","Year"), sep = "/")
  separated <- unite(separated, "Time", c("Hour","Minute","Second"), sep = ":")
  united <- chron( dates= separated$Date, times = separated$Time, format = c(dates = "Month/Day/Year",times = "h:m:s"))
  u[[x]] <- united
  
}
if(z == 8){
  u1 <- u
  } else {
    u1 <- u[-which(sapply(u, is.null))]
    }
l <- list()
final_list <- list()
for (b in 1:length(u1)) {
  data <- as.data.frame(u1[[b]])
  colnames(data) <- "timestamp"
  fin <- data.frame(data$timestamp[1])
  colnames(fin) <- "timestamp"
  data <- data[-1,]
  for (d in 1:length(data)) {
    diff <- data[d] - fin$timestamp[1]
    fin <- rbind(fin,diff)
  }
  fin <- fin[-1,]
  l[[b]] <- fin
  #extracting just the timestamps
  n <- l[[b]]
  new <- as.data.frame(format(n, "%H:%M:%S"))
  colnames(new) <- "timeDiff"
  final_list[[b]] <- new
}

remove(final)
final <- data.frame()
for (f in 1:length(final_list)) {
  temp <- as.data.frame(final_list[[f]])
  final <- rbind(temp,final)
}
final$timeDiff <- chron(times = final$timeDiff, format = (times = "h:m:s"))

#Extracting data within one hour
mins <- data.frame()
mins <- subset(final, final$timeDiff < "01:00:00")
mins$status <- 0

#*****************************************RUMOR**************************************************************
#creating a list to get the source tweets from files
source_tweetr <- list()
ur <- list()

for (i in 1:length(rumor_file)) {
  ar <- paste(rumor_dir[i],"/source-tweets/",rumor_file[i],".json", sep = "")
  source_tweetr[i] <- ar
}


setwd("C:/Users/nikita parab/Desktop/SEMESTER 3/ICT Solution/PreProcessed_Data")
kr <- 1
jr <- list()
for (x in 1:length(source_tweetr)) {
  #Extracting timestamp of the source tweet
  sr <- source_tweetr[[x]]
  sourcer <- rjson::fromJSON(file = sr)
  s_listr  <- sourcer["created_at"]
  source_dfr <- as.data.frame(s_listr)
  
  #Extracting timestamp of the reaction tweets
  rr <- paste(rumor_dir[[x]],"reactions", sep = "/")
  file_namesr <- list.files(path = rr ,pattern = "*.json", full.names = TRUE)
  reactionr <- lapply(file_namesr, function(n) rjson::fromJSON(file=n))
  if(length(reactionr)== 0){
    jr[[kr]] <- rr
    kr <- kr+1
    next
  }
  created_atr <- sapply(1:length(reactionr), function(y) reactionr[[y]][["created_at"]])
  reaction_dfr <- as.data.frame(created_atr)
  colnames(reaction_dfr) <- "created_at"
  #Combining timestamps of the source and the reaction tweets
  combiner <- rbind(source_dfr, reaction_dfr)
  
  #Separating timestamp
  separatedr <- separate(combiner, created_at, c("Day","Month","Date","Hour","Minute","Second","Plus","Year"), extra = 'drop')
  separatedr <- separatedr[,c(-1,-7)]
  separatedr <- unite(separatedr, "Date", c("Month", "Date","Year"), sep = "/")
  separatedr <- unite(separatedr, "Time", c("Hour","Minute","Second"), sep = ":")
  unitedr <- chron( dates= separatedr$Date, times = separatedr$Time, format = c(dates = "Month/Day/Year",times = "h:m:s"))
  ur[[x]] <- unitedr
  
}

u1r <- ur[-which(sapply(ur, is.null))]
lr <- list()
final_listr <- list()
for (b in 1:length(u1r)) {
  datar <- as.data.frame(u1r[[b]])
  colnames(datar) <- "timestamp"
  finr <- data.frame(datar$timestamp[1])
  colnames(finr) <- "timestamp"
  datar <- datar[-1,]
  for (d in 1:length(datar)) {
    diffr <- datar[d] - finr$timestamp[1]
    finr <- rbind(finr,diffr)
  }
  finr <- finr[-1,]
  lr[[b]] <- finr
  #extracting just the timestamps
  nr <- lr[[b]]
  newr <- as.data.frame(format(nr, "%H:%M:%S"))
  colnames(newr) <- "timeDiff"
  final_listr[[b]] <- newr
}

remove(finalr)
finalr <- data.frame()
for (f in 1:length(final_listr)) {
  tempr <- as.data.frame(final_listr[[f]])
  finalr <- rbind(tempr,finalr)
}
finalr$timeDiff <- chron(times = finalr$timeDiff, format = (times = "h:m:s"))


minsr <- data.frame()
minsr <- subset(finalr, finalr$timeDiff < "01:00:00")
minsr$status <- 1

time <- data.frame(table(rbind(mins, minsr)))
#converting time to seconds
timenew <- data.frame()
timenew <- time
timenew$timeDiff <- as.POSIXct(timenew$timeDiff, format = '%H:%M:%S')
timenew$timeDiff <- as.numeric(as.POSIXct(timenew$timeDiff)- as.POSIXct(paste(Sys.Date() ,"00:00:00"), units="secs"))
write.csv(timenew, paste(events[z],".csv",sep = ""), row.names = FALSE)
}
