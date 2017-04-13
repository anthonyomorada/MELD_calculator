

library(dplyr)

fileName <- 'C:/Users/MoradaAO/Documents/MELD Calculator Files/Liver Transplant.zip'
setwd('C:/Users/MoradaAO/Documents/MELD Calculator Files/')
labs<- read.csv(unzip(fileName, files = "Report 1.csv"),header = T, skip = 2)
txp <- read.csv(unzip(fileName, files = "Report 2.csv"), header = T, skip = 2)
hd <- read.csv(unzip(fileName, files = "Report 3.csv"), header = T, skip = 2)

## Combining Hemodialysis and Lab Tables
#Cleaning Hemodialysis Table
hd <- select(hd, -ICD.Procedure.End.Date)
hd$Test.Result <- 1
colnames(hd) <- c("Medical.Record.Number..MRN.","Result.Date","Result.Component.Common.Name","Test.Result")
hd$Result.Date <- as.Date(hd$Result.Date, "%m/%d/%Y")

#Cleaning Lab Table
labs$Result.Date <- as.Date(labs$Result.Date, "%m/%d/%y")
labs$Test.Result <- as.numeric(levels(labs$Test.Result))[labs$Test.Result]
labs <- aggregate(labs$Test.Result, by=list(labs$Medical.Record.Number..MRN.,labs$Result.Date,labs$Result.Component.Common.Name), FUN = mean, na.rm =TRUE)
colnames(labs) <- c("Medical.Record.Number..MRN.","Result.Date","Result.Component.Common.Name","Test.Result")


#Row binding hemodialysis and lab tables
vars <- rbind(hd,labs)
rm(labs,hd)
vars$Test.Result <- as.numeric(vars$Test.Result)

##Creating Table
txp <- select(txp, -ICD.Procedure.End.Date)
txp$ICD.Procedure.Start.Date <- as.Date(txp$ICD.Procedure.Start.Date, "%m/%d/%Y")

daycap <- 30
df <- as.data.frame(matrix(NA, nrow(txp), daycap+1))
colnames(df) <- -daycap:0

txp <- cbind(txp,df)

##MELD Calculator
pt <- NA
dt <- NA
fill <- NA
bili <- NA
creat <- NA
inr <- NA
sodium <- NA
dial <- NA




for(r in 1:nrow(txp)){
     for(c in 4:ncol(txp)){
         qday <- as.numeric(colnames(txp[c]))
         pt <- txp$Medical.Record.Number..MRN.[r]
         dt <- txp$ICD.Procedure.Start.Date[r]
         fill <- filter(vars, vars$Medical.Record.Number..MRN.==pt)
         fill$day <-as.numeric(fill$Result.Date-dt)
         #Bili
         if (nrow(filter(fill, fill$day == qday & fill$Result.Component.Common.Name == "BILIRUBIN TOTAL")) == 1){
              bili <- filter(fill, fill$day == qday & fill$Result.Component.Common.Name == "BILIRUBIN TOTAL")$Test.Result
         } else {
              bili <- NA
              }
         #Creat
         if (nrow(filter(fill, fill$day == qday & fill$Result.Component.Common.Name == "CREATININE")) == 1){
              creat <- filter(fill, fill$day == qday & fill$Result.Component.Common.Name == "CREATININE")$Test.Result
         } else {
              creat <- NA
              }
         #INR
         if (nrow(filter(fill, fill$day == qday & fill$Result.Component.Common.Name == "INR")) == 1){
              inr <- filter(fill, fill$day == qday & fill$Result.Component.Common.Name == "INR")$Test.Result
         } else {
              inr <- NA
              }   
         #Sodium
         if (nrow(filter(fill, fill$day == qday & fill$Result.Component.Common.Name == "SODIUM")) == 1){
              sodium <- filter(fill, fill$day == qday & fill$Result.Component.Common.Name == "SODIUM")$Test.Result
         } else {
              sodium <- NA
              }    
         #Hemodialysis status
         if (sum(fill$Result.Component.Common.Name == "HEMODIALYSIS" & fill$day < qday & fill$day > (qday-7)) >= 2){
              dial <- TRUE 
              print(paste(c,sum(fill$Result.Component.Common.Name == "HEMODIALYSIS" & fill$day < qday & fill$day > (qday-7))))
         } else  {
              dial <- FALSE
         }
         ## Value check and MELD calculator execution
         if (is.na(bili)|is.na(creat)|is.na(inr)|is.na(sodium)){
              txp[r,c] <- NA
         } else {
              if (bili < 1){bili <- 1}
              if (inr < 1){inr <- 1 }
              if (sodium < 125 ){
                   sodium <- 125
              } else if(sodium > 137){
                   sodium <- 137
              }
              
              if (dial == TRUE){
                   creat <- 4
              } else if (creat < 1){
                   creat <- 1
              } else if (creat > 4){
                   creat <- 4
              }
              
              # MELD and NaMELD Calculator
              MELD <- round(11.2*log(inr) + 9.57*log(creat)+3.78*log(bili)+6.43)
              MELDNa <- round(MELD + 1.32*(137-sodium)- (0.033*MELD*(137-sodium)))
              txp[r,c] <- MELDNa
              }
         
     }
}

write.csv(txp, file = "Liver MELD Scores-30 Days- 04132017.csv", row.names = F, na = "")



