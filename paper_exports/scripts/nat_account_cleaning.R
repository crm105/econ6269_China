#Revised 10.27.2018
#Author - Christopher Montgomery

# This script processes the national income account files for Singapore, Japan, Korea, 
# China, and the United States. For each country, it reduces data to the simple expenditure
# GDP equation Y = C + I +NX, and merges all data into a single output file

#Input files: 
#Output files: 

library (readxl)
library(reshape2)
library(dplyr)



#Let's start with Singapore. First a simple load using the readxl package
#We have some metadata after row 9. While helpful, we can drop it for now using n_max

sing <- read_excel("../data/Singapore_GDP.xlsx", sheet = "T1", skip = 5, n_max = 9)

#Remove commas then convert to numeric observations in columns 2:59

sing[2:59] <- lapply(sing[2:59], gsub, pattern=',', replacement='')
sing[2:59] <- sapply(sing[2:59], as.numeric)

#for simplicity, I will melt from wide to long. 

sing$code <- "sgpr"
sing <- melt(sing, id.vars = c("code","Variables"), variable.name ="year", value.name = "value")
sing <- dcast(sing, code + year   ~ Variables)

#For some reason converting year straight to numeric was causing issues

sing$year <- as.character(sing$year)
sing$year <- as.numeric(sing$year)

#drop columns that are not part of the  Y = C + I + NX identity 

keep <- c("code","year","Expenditure On GDP", "Net Exports Of Goods And Services",
          "Government Consumption Expenditure", "Gross Fixed Capital Formation",
          "Private Consumption Expenditure" ,"Changes In Inventories", 
          "Statistical Discrepancy")
rename <- c("code", "year", "GDP", "NX", "G", "I", "C", "inventory", "discrepancy")

sing = sing[keep]
colnames(sing) <- rename

#For our purposes, I argue folding inventories into net investment is acceptable

sing$I <- sing$I + sing$inventory
sing$inventory <- NULL


# Test to make sure our identity holds. It does, although some discrepencies are somewhat
#large

sing$test <- sing$GDP - sing$NX - sing$G - sing$I - sing$C - sing$discrepancy


#Korea

kor <- read.csv("../data/korea_GDP.csv", nrows = 21)

#Column values to numeric

kor[2:66] <- sapply(kor[2:66], as.character)
kor[2:66] <- sapply(kor[2:66], as.numeric)

kor$code <- "kor"
kor <- melt(kor, id.vars = c("code","Selection.of.Items"), variable.name ="year",
            value.name = "value")

kor$year <- as.character(kor$year)
kor$year <- lapply(kor$year, gsub, pattern='X', replacement='')
kor$year<- as.numeric(kor$year)


#Take a close look at this
#for some reason an error arises without the fun.aggregate = mean, but there are no dups

kor <- dcast(kor, code + year   ~ Selection.of.Items, fun.aggregate=mean)

kor$`Gross  capital formation` <- kor$`Gross capital formation` +
  kor$`Changes in inventories and acquisitions less disposals of valuables` +
  kor$Construction 

kor$`Exports of goods and services` <- kor$`Exports of goods and services` - kor$`(less) Imports of goods and services`

keep <- c("code", "year", "Expenditure on GDP", "Exports of goods and services",
          "Government", "Gross capital formation", "Private",
          
          "Statistical discrepancy")

rename <- c("code", "year", "GDP", "NX", "G", "I", "C", "discrepancy")

kor = kor[keep]
colnames(kor) <- rename

#test to confirm GDP identity for Korea

kor$test <- kor$GDP - kor$NX - kor$G -kor$I - kor$C  - kor$discrepancy

#Japan

jap <- read_excel("../data/japan_GDP.xls", sheet = 1, skip = 11, n_max = 43 )

jap[1] <- NULL

colnames(jap)[1] <- "year"
jap$year <- as.numeric(jap$year)
jap <- jap[-c(1:3),]
jap[,2:31] <- sapply(jap[,2:31], as.numeric)
jap$code <- "jpn"

keep <- c("year","code", "Gross domestic expenditure", "Net exports of goods and services",
          "Gross domestic capital formation", "Government final consumption expenditure",
          "Private final consumption expenditure")
rename <- c ("year", "code", "GDP", "NX", "I", "G","C")

jap = jap[keep]
colnames(jap) <- rename

jap <- na.omit(jap)
jap <- unique(jap)
jap$test <- jap$GDP - jap$NX - jap$G -jap$I - jap$C  
jap$discrepancy <- 0 

#China 

chn <- read.csv("../data/China_GDP.csv", skip = 2, nrows =10 )

chn$code <- "chn"
chn <- melt(chn, id.vars = c("code","Indicators"), variable.name ="year", value.name = "value")
chn <- dcast(chn, code + year   ~ Indicators)

chn$year <- sapply(chn$year, gsub, pattern = "X", replacement = "")
chn$year <- as.numeric(chn$year)

#Before we know what to keep we need to understand which variables are subcomponents of our GDP identity
#The below equation shows that I = Gross Capital formation = Gross Fixed Capital + Inventories

test_I <- chn$`Gross Capital Formation(100 million yuan)` - 
  chn$`Gross Fixed Capital Formation(100 million yuan)` - 
  chn$`Changes in Inventories(100 million yuan)`

test_I

#Lets see about consumption: Here we see total consumption = G + C

test_C <- chn$`Final Consumption Expenditure(100 million yuan)` -
  chn$`Government Consumption Expenditure(100 million yuan)` - 
  chn$`Household Consumption Expenditure(100 million yuan)`

test_C

#Rename and keep necessary columns, add a test and descrepancy column for merge. 

keep = c("code", "year", "Gross Domestic Product by Expenditure Approach(100 million yuan)",
        "Government Consumption Expenditure(100 million yuan)", "Gross Capital Formation(100 million yuan)",
         "Household Consumption Expenditure(100 million yuan)",
         "Net Exports of Goods and Services(100 million yuan)")

rename <- c("code", "year", "GDP", "G", "I","C", "NX")

chn = chn[keep]
colnames(chn) <- rename

chn$test <- chn$GDP - chn$G - chn$C - chn$I - chn$NX
chn$discrepancy <- 0

chn <- na.omit(chn)

# merge everything together; drop test variable;  output to national_accounts.csv

merge <- rbind(jap,kor)
merge <- rbind(merge, sing)
merge <- rbind(merge, chn)
merge$test <- NULL

write.csv(chn, "../data/national_accounts.csv")





