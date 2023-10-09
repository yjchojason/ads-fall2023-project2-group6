#data cleaning process

library(dplyr)
getwd()
data = read.csv('DisasterDeclarationsSummaries.csv')
dim(data)
head(data)
data1 = unique(data)

#delete unwanted columns
data1 = subset(data, select = -hash)
data1 = subset(data1, select = -lastIAFilingDate)

#clean data of incident dates
data1$declarationDate = substr(data1$declarationDate, start = 1, stop = 10)               
data1$incidentBeginDate = substr(data1$incidentBeginDate, start = 1, stop = 10)
data1$incidentEndDate = substr(data1$incidentEndDate, start = 1, stop = 10)
data1$disasterCloseoutDate = substr(data1$disasterCloseoutDate, start = 1, stop = 10)
data1$lastRefresh = substr(data1$lastRefresh, start = 1, stop = 10)

#convert case
data1$declarationTitle = tolower(data1$declarationTitle)

#fill in missing values
data1[data1=='']='NA'

#format values, fixed bits
table(data$incidentType)
summary(data$incidentType)
data1$fipsStateCode = sprintf("%02d", data1$fipsStateCode)
data1$fipsCountyCode = sprintf("%03d", data1$fipsCountyCode)

#set subsets
Major_Disaster1 = data1[data1$disasterNumber<2000, ]
Major_Disaster2 = data1[3999<data1$disasterNumber&data1$disasterNumber<5000, ]
Major_Disaster = rbind(Major_Disaster1,Major_Disaster2)

fire1 = data1[1999<data1$disasterNumber&data1$disasterNumber<3000, ]
fire2 = data1[4999<data1$disasterNumber&data1$disasterNumber<6000, ]
fire = rbind(fire1,fire2)

#set new month columns 
data1 = mutate(data1, begin_mouth 
               = substr(data1$declarationDate, start = 6, stop = 7) )
data1$begin_mouth = ifelse(data1$begin_mouth == "01", "January", data1$begin_mouth)
data1$begin_mouth = ifelse(data1$begin_mouth == "02", "February", data1$begin_mouth)
data1$begin_mouth = ifelse(data1$begin_mouth == "03", "March", data1$begin_mouth)
data1$begin_mouth = ifelse(data1$begin_mouth == "04", "April", data1$begin_mouth)
data1$begin_mouth = ifelse(data1$begin_mouth == "05", "May", data1$begin_mouth)
data1$begin_mouth = ifelse(data1$begin_mouth == "06", "June", data1$begin_mouth)
data1$begin_mouth = ifelse(data1$begin_mouth == "07", "July", data1$begin_mouth)
data1$begin_mouth = ifelse(data1$begin_mouth == "08", "August", data1$begin_mouth)
data1$begin_mouth = ifelse(data1$begin_mouth == "09", "September", data1$begin_mouth)
data1$begin_mouth = ifelse(data1$begin_mouth == "10", "October", data1$begin_mouth)
data1$begin_mouth = ifelse(data1$begin_mouth == "11", "November", data1$begin_mouth)
data1$begin_mouth = ifelse(data1$begin_mouth == "12", "December", data1$begin_mouth)

#write csv
write.csv(data1, "/Users/wshmac/Downloads/Disaster_data.csv",row.names = FALSE)
setwd("/Users/wshmac/Documents/Github")
write.csv(data1, "Disaster_data.csv",row.names = FALSE)
write.csv(Major_Disaster,"MajorDisaster_data.csv",row.names = FALSE )
write.csv(fire, "FireDisaster_data.csv",row.names = FALSE)



