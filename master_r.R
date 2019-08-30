# !diagnostics off
######################################
#Masterfile Script! Changing the SAS masterfile code into R
######################################

library(foreign)
library(dplyr)
library(stringr)

#Some files read in from SAS outputs to test against the R script
#SASoldbandsB <- read.csv("X:\\brant_data\\testingsas.csv") #To make sure that our oldbandsB is the same as SAS's. Use anti_join to test for differences between dataframes.
#SASOldstuff <- read.csv("X:\\brant_data\\SASOldstuff.csv")
#SASPrea <- read.csv("X:\\brant_data\\SASPrea.csv")
#SASBA86 <- read.csv("X:\\brant_data\\SASBA86.csv")
#SASBC86 <- read.csv("X:\\brant_data\\SASbc86.csv")
#SASBG86 <- read.csv("X:\\brant_data\\SASBG86.csv")
#SASBS86 <- read.csv("X:\\brant_data\\SASBS86.csv")
#SASHL86 <- read.csv("X:\\brant_data\\SASHL86.csv")

###
#Import All the Things
###

#***Maybe include colClasses = "character" when reading in at some point to avoid issues later

#***This is COMMENTSed out in the SAS file for some reason, and instead we use BBL DATA for oldbands. Including it here b/c why not
#oldbands <- read.dbf("X:\\brant_data\\brant data\\bandsch\\1960 to present banding schedules.DBF")

#Import in Bird Banding Lab data as our old bands and do some stuff to it that i'm not sure yet
oldbands <- read.dbf("X:\\brant_data\\brant data\\bandsch\\BBL DATA.DBF")
oldbands <- oldbands %>% mutate_all(as.character)
oldbandsA <- oldbands #***I don't think we actually need oldbandsA; but I'm keeping it in for now in case we need the original oldbands later on in the code
oldbandsA$COUNT <- 1  #  without the count column. But if we don't then just change oldbandsA to oldbands$Count <- 1
#***CHANGE THIS TO GROUP_BY
oldbandsB <- aggregate(COUNT~METAL+BYEAR+BDAY+BMONTH+NAGE+NSEX+PERMIT, oldbandsA, sum) #This goes through and counts for times where a metal band occurs more than once and combines it into one row instead of multiple. It also gets rid of BLAT and BLONG.

#Testing to make sure oldbandsB is the same between the SAS file and this r script
# test <- oldbandsB %>% mutate_all(as.character)
# SASoldbandsB <- SASoldbandsB %>% mutate_all(as.character)
# differences <- anti_join(test, SASoldbandsB)
# rm("test", "SASoldbandsB", "differences")

#Import Recoveries and BSCPRE (Pre 86 plastics?)
recovs <- read.csv("X:\\brant_data\\brant data\\harvest_csv\\brant_recoveries_9_28_15.csv", colClasses = "character")
BSCPRE <- read.csv("X:\\brant_data\\brant data\\bandsch_csv\\PRE 86 PLASTICS ADDED.csv", colClasses = "character")
BSCPRE <- BSCPRE[order(BSCPRE$METAL),] #Sorts the BSCPRE data by metal in ascending order

#Import BS for all years
BSTemp <- "X:\\brant_data\\brant data\\bandsch_csv" #Directory where all the BS files live
BSfilenames <- list.files(BSTemp, pattern= "bs.csv", ignore.case = TRUE, full.names = TRUE) #Finds all the files with bs.csv or BS.csv
BS <- lapply(BSfilenames, read.csv, colClasses = "character") #Reads in all of these files
names(BS) <- paste0("bsc", substr(c("1986":"2017"), 3,4)) #Renames the files in our lists to BSC86-BSC17 

#Import Recaps
RecapTemp <- "X:\\brant_data\\brant data\\RECAPS_csv\\"
Recapfilenames <- list.files(RecapTemp, pattern = "^r", ignore.case = TRUE, full.names = TRUE)
RECAP <- lapply(Recapfilenames, read.csv, colClasses = "character")
rpre00 <- paste0("recap", c("86":"99"))
rpost00 <- paste0("recap", sprintf("%02d", c("00":"17")))
names(RECAP) <- c(rpost00, rpre00) #This one's backwards since the files are read in backwards

#Import summary files 
NestSumTemp <- "X:\\brant_data\\brant data\\NESTSUM\\"
NestSumfilenames <- list.files(NestSumTemp, full.names = TRUE)
NEST <- lapply(NestSumfilenames, read.dbf)
names(NEST) <- paste0("NEST", substr(c("1986":"2017"), 3,4))

#Import egg data
EggTemp <- "X:\\brant_data\\brant data\\webtags_csv" 
Eggfilenames <- list.files(EggTemp,full.names = TRUE)
EGG <- lapply(Eggfilenames, read.csv, colClasses = "character")
names(EGG) <- paste0("EGG", substr(c("1986":"2017"), 3,4))


#Import Manipulation nest files
#*********Look at converting to a csv again. It only did the first sheet.

#Import spring/winter resights, tower data, and band reads not associated with nest/broods
SPRINGMST15 <- read.csv("X:\\brant_data\\brant data\\Spring resights_csv\\SPRINGMST2015.csv", colClasses = "character")
WINTERMST16 <- read.csv("X:\\brant_data\\brant data\\winter resights_csv\\WINTER2016.csv", colClasses = "character")
TOWER <- read.dbf("X:\\brant_data\\brant data\\BAND\\TOWBRD.DBF")
NBBAND <- read.dbf("X:\\brant_data\\brant data\\BAND\\NBBAND.DBF")

###
#BBL DATA
###

oldstuff <- oldbandsB[,c("METAL", "PERMIT")]
oldstuff$BBLAGE <- NA
oldstuff$BBLSEX <- NA

#This for loop takes a while, change it?
#Potentially use which to find the specific cases, then change it? Like we've done ahead, go find that. 
#This follows codes from the BBL found here -> https://www.pwrc.usgs.gov/bbl/manual/age.cfm
for(i in 1:nrow(oldbandsB)){
  if(oldbandsB$NAGE[i] == "0" & !is.na(oldbandsB$NAGE[i])){oldstuff$BBLAGE[i] <- "U"}
  if(oldbandsB$NAGE[i] == "1" & !is.na(oldbandsB$NAGE[i])){oldstuff$BBLAGE[i] <- "AHY"}
  if(oldbandsB$NAGE[i] == "2" & !is.na(oldbandsB$NAGE[i])){oldstuff$BBLAGE[i] <- "HY"}
  #There are no 3's because we never catch Brants as Juveniles. 
  if(oldbandsB$NAGE[i] == "4" & !is.na(oldbandsB$NAGE[i])){oldstuff$BBLAGE[i] <- "L"}
  if(oldbandsB$NAGE[i] == "5" & !is.na(oldbandsB$NAGE[i])){oldstuff$BBLAGE[i] <- "SY"}
  if(oldbandsB$NAGE[i] == "6" & !is.na(oldbandsB$NAGE[i])){oldstuff$BBLAGE[i] <- "ASY"}
  if(oldbandsB$NAGE[i] == "7" & !is.na(oldbandsB$NAGE[i])){oldstuff$BBLAGE[i] <- "TY"}
  if(oldbandsB$NAGE[i] == "8" & !is.na(oldbandsB$NAGE[i])){oldstuff$BBLAGE[i] <- "ATY"}
  
  if(oldbandsB$NSEX[i] == "0" & !is.na(oldbandsB$NSEX[i])){oldstuff$BBLSEX[i] <- "U"} 
  if(oldbandsB$NSEX[i] == "4" & !is.na(oldbandsB$NSEX[i])){oldstuff$BBLSEX[i] <- "M"} 
  if(oldbandsB$NSEX[i] == "5" & !is.na(oldbandsB$NSEX[i])){oldstuff$BBLSEX[i] <- "F"}
}
oldstuff$BBLYEAR <- oldbandsB$BYEAR
oldstuff$BBLSEX[which(is.na(oldstuff$BBLSEX))] <- ""

#Testing to make sure it looks ok
# SASOldstuff <- SASOldstuff %>% mutate_all(as.character)
# differences <- anti_join(oldstuff, SASOldstuff)
# rm("SASOldstuff", "differences")

#PREA are all the banded birds that existed before our study. So we're just taking them into account 
PREA <- merge(BSCPRE, oldstuff, by = "METAL", all = T)

newcols <- c("AN84", "SN84", "AB85", "SB85") #New columns we're adding to PREA
PREA[newcols] <- '' #Creates new columns and just leaves them empty
PREA$YEARB <- PREA$BBLYEAR #Change Year Banded to be the same as BBLYear. In SAS they kept it as '.' except for the ones from the BSCPRE files in 1984 and 1985

PREA$AGE <- as.character(PREA$AGE)
PREA$SEX <- as.character(PREA$SEX)


for(i in 1:nrow(PREA)){
  if(PREA$YEAR[i] == 1984 & !is.na(PREA$YEAR[i])){
    #PREA$N84[i] <- "1" #Why do we do this if we don't keep it. We're not including it for now.
    PREA$YEARB[i] <- 1984
    PREA$AN84[i] <- PREA$AGE[i]
    PREA$SN84[i] <- PREA$SEX[i]
  }
  
  if(PREA$YEAR[i] == 1985 & !is.na(PREA$YEAR[i])){
      #PREA$BD85[i] <- PREA$DRIVE[i] #Why do we do this if we don't keep it
      PREA$YEARB[i] <- 1985
      PREA$AB85[i] <- PREA$AGE[i]
      PREA$SB85[i] <- PREA$SEX[i]
  }
}

keep <- c('METAL', 'PERMIT', "BBLAGE", "BBLSEX", "BBLYEAR", 'BAND','YEARB', 'AN84', 'SN84', 'AB85', 'SB85')
PREA <- PREA[keep]

#Test to make sure they're the same. This one has two differences, when SAS gets read into R we think that sometimes it reads the
#   female code to be false, but otherwise they're the same. Also there's a few weird formatting things you have to do to make
#   anti_join works including up above changing PREA$YEARB <- "." instead of as the year banded since we added that.
# test <- PREA
# test$BAND[which(is.na(test$BAND))] <- ""
# test$SN84[which(test$SN84 == "")] <- NA
# SASPrea <- SASPrea %>% mutate_all(as.character)
# differences <- anti_join(test, SASPrea)
# rm("test", "SASPrea", "differences")
 


#************Some COMMENTs that I don't know what they mean yet
#/*When updating the program; 
# 1)AA## needs to be changed to prior year (except for 86, when it gets PREA) 
# 2)for 86, the first errorlist has to have errorlist removed from the set statement
# 3)for 86, delete AB86 and AC86 as they do not exist
# 4)for 86, delete the AC86 form the LM86 step
# 5)for 86, delete the NG86 and NH86 steps
# 6)for 86, delete the NH86 and NH86 set statements in the NI86 datastep*/ 


###
#1986 Start
###

lists <- c('BS','EGG','NEST','RECAP') #Our lists of dataframes for all of the years. 

#A function that looks through our lists and pulls out the dataframes from the year we specify.
addToEnv <- function(list, regex){
  for(i in list){
    dumb <- grep(pattern = regex, names(get(i)))
    list2env(get(i)[dumb], envir = .GlobalEnv)
  }
}

addToEnv(list = lists, regex = "*86") #Calls the function


BA86 <- bsc86
BA86$FILE <- "BS" #Tells us what file this information came from. 
#This for loop deletes rows where there is missing information. We have to loop backwards here since if we're looping forward 
#   while deleting rows our i will become mismatched to what row we are actually on in the dataframe.
for(i in nrow(BA86):1){
  if(BA86$METAL[i] =="." & BA86$BAND[i] == "" & !is.na(BA86$BAND[i]) & !is.na(BA86$METAL[i])){
    BA86 <- BA86[-i,]
  }  
  if(BA86$BSTAT[i] == "L" & !is.na(BA86$BSTAT[i])){ 
    BA86 <- BA86[-i,]
  }
}

BA86 <- BA86[ , !names(BA86) %in% c("NINTHPRIM","BODYLEN", 'SAMPLENO', 'COMMENTS', 'NEWPLASTIC', 'NEWMETAL')] #Deletes these rows
BA86 <- BA86[order(BA86$METAL),]


#Weird way to test if they're the same lol. This is formatting it like SAS does with some being NA some being "." and some being "" 
#   if there's no data. But I like anti_join to check so here's how to weirdly format our result so we can compare it to SAS.
# test <- BA86
# test$WEBTAG[which(test$WEBTAG == "")] <- NA
# test$CUL[which(test$CUL == "")] <- "."
# test$TAR[which(test$TAR == "")] <- "."
# test$MASS[which(test$MASS == "")] <- "."
# SASBA86 <- SASBA86 %>% mutate_all(as.character)
# differences <- anti_join(test, SASBA86)
# rm("test", "SASBA86", "differences")

BB86 <- recap86
BB86$FILE <- "RE"
for(i in nrow(BB86):1){
  if(BB86$METAL[i] == "." & BB86$BAND[i] == "" & !is.na(BB86$BAND[i]) & !is.na(BB86$METAL[i])){
    BB86 <- BB86[-i,]
  } 
}

BB86 <- BB86[ , !names(BB86) %in% c("NINTHPRIM","BODYLEN", 'SAMPLENO', 'COMMENTS', 'RE_STAT')]
BB86 <- BB86[order(BB86$METAL),]

BC86 <- bind_rows(BA86, BB86)
BC86$NEWMETAL[which((is.na(BC86$NEWMETAL)| BC86$NEWMETAL == "" )) ] <- "."
BC86$NEWPLASTIC[which(BC86$NEWPLASTIC == "")] <- NA

#Test test.
# test <- BC86
# test$WEBTAG[which(test$WEBTAG == "")] <- NA
# test$CUL[which(test$CUL == "")] <- "."
# test$TAR[which(test$TAR == "")] <- "."
# test$MASS[which(test$MASS == "")] <- "."
# test$DATE[which(test$DATE == "")] <- "."
# test$BSTAT[which(is.na(test$BSTAT))] <- ""
# SASBC86 <- SASBC86 %>% mutate_all(as.character)
# differences <- anti_join(test, SASBC86)
# rm("test", "SASBC86", "differences")



###
#Checking for replacement metal and plastic bands
###

BCols <- c("LBAND", "PR86", "LMETAL", "mr86")
BS86 <- BC86[1,]  #Initializing BS86 to have the same columns as BC86
BS86[BCols] <- NA #Adding the new columns and setting them to NA to start
BS86[1,] <- NA #First fake row we'll get rid of later. I have it for the second for loop?? because I thinkt trying to loop through the 0 column would cause issues?

for(i in 1:nrow(BC86)){
  #If METAL > 0
  if(BC86$METAL[i] > 0 & !is.na(BC86$METAL[i])){
    #If BAND != ""
    if(BC86$BAND[i] != "" & !is.na(BC86$BAND[i])){
      #IF NEWPLASTIC != ""
      if( !is.na(BC86$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0 
        if(BC86$NEWMETAL[i] > 0 & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BD86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- which(BC86$METAL[i] == BS86$METAL)
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- as.character(BC86$BAND[i]) 
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- BC86$METAL[i]
            next
          #IF NEWMETAL == "." 
        }else if(BC86$NEWMETAL[i] == "." & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BE86
            BS86 <- bind_rows(BS86, BC86[i,])
            z <- which(BC86$METAL[i] == BS86$METAL)
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- as.character(BC86$BAND[i]) 
            BS86$LMETAL[z] <- BC86$METAL[i]
            BS86$mr86[z] <- "."
            next
        }#IF NEWPLASTIC == ""
      }else if( is.na(BC86$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0 
        if(BC86$NEWMETAL[i] > 0 & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BF86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- which(BC86$METAL[i] == BS86$METAL)
            BS86$LBAND[z] <- BC86$BAND[i]
            BS86$PR86[z] <- "    " 
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- BC86$METAL[i]
            next
          #IF NEWMETAL == "."
        }else if(BC86$NEWMETAL[i] == "." & !is.na(BC86$NEWMETAL[i])){
           #equivalent to BG86
           BS86 <- bind_rows(BS86, BC86[i,])
           z <- which(BC86$METAL[i] == BS86$METAL)
           BS86$LBAND[z] <- BC86$BAND[i]
           BS86$PR86[z] <- "    " 
           BS86$LMETAL[z] <- BC86$METAL[i]
           BS86$mr86[z] <- "."
           next
        }
      }#If BAND == ""
    }else if(BC86$BAND[i] == "" & !is.na(BC86$BAND[i])){
      #IF NEWPLASTIC != ""
      if( !is.na(BC86$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC86$NEWMETAL[i] > 0 & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BH86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- which(BC86$METAL[i] == BS86$METAL)
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- "PBA "
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- BC86$METAL[i]
            next
          #IF NEWMETAL == "."
        }else if(BC86$NEWMETAL[i] == "." & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BI86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- which(BC86$METAL[i] == BS86$METAL)
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- "PBA "
            BS86$LMETAL[z] <- BC86$METAL[i]
            BS86$mr86[z] <- "."
            next
        }#IF NEWPLASTIC == ""
      }else if( is.na(BC86$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC86$NEWMETAL[i] > 0 & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BJ86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- which(BC86$METAL[i] == BS86$METAL)
            BS86$LBAND[z] <- ""
            BS86$PR86[z] <- "    "
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- BC86$METAL[i]
            next
          #IF NEWMETAL == "."
        }else if(BC86$NEWMETAL[i] == "." & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BK86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- which(BC86$METAL[i] == BS86$METAL)
            BS86$LBAND[z] <- ""
            BS86$PR86[z] <- "    "
            BS86$LMETAL[z] <- BC86$METAL[i]
            BS86$mr86[z] <- "."
            next
        }
      }
    }
   #If METAL == "." 
  }else if(BC86$METAL[i] == "." & !is.na(BC86$METAL[i]) ){
    #If BAND != ""
    if(BC86$BAND[i] != "" & !is.na(BC86$BAND[i])){
      #IF NEWPLASTIC != ""
      if( !is.na(BC86$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC86$NEWMETAL[i] > 0 & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BL86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- which(BC86$METAL[i] == BS86$METAL)
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- as.character(BC86$BAND[i]) 
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- "12345"
            next
          #IF NEWMETAL == "."
        }else if(BC86$NEWMETAL[i] == "." & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BM86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- which(BC86$METAL[i] == BS86$METAL)
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- as.character(BC86$BAND[i]) 
            BS86$LMETAL[z] <- "."
            BS86$mr86[z] <- "."
            next
        }#IF NEWPLASTIC == ""
      }else if( is.na(BC86$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC86$NEWMETAL[i] > 0 & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BN86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- which(BC86$METAL[i] == BS86$METAL)
            BS86$LBAND[z] <- BC86$BAND[i]
            BS86$PR86[z] <- "    "
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- "12345"
            next
          #IF NEWMETAL == "."
        }else if(BC86$NEWMETAL[i] == "." & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BO86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- which(BC86$METAL[i] == BS86$METAL)
            BS86$LBAND[z] <- BC86$BAND[i]
            BS86$PR86[z] <- "    "
            BS86$LMETAL[z] <- "."
            BS86$mr86[z] <- "."
            next
        }
      }#If BAND == ""
    }else if(BC86$BAND[i] == "" & !is.na(BC86$BAND[i])){
      #IF NEWPLASTIC != ""
      if( !is.na(BC86$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC86$NEWMETAL[i] > 0 & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BP86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- which(BC86$METAL[i] == BS86$METAL)
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- "PBA "
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- "12345"
            next
          #IF NEWMETAL == "."
        }else if(BC86$NEWMETAL[i] == "." & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BQ86
            BS86 <- bind_rows(BS86, BC86[i,])
            z <- which(BC86$METAL[i] == BS86$METAL)
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- "PBA "
            BS86$LMETAL[z] <- "."
            BS86$mr86[z] <- "."
            next
        }#IF NEWPLASTIC == ""
      }else if( is.na(BC86$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC86$NEWMETAL[i] > 0 & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BR86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- which(BC86$METAL[i] == BS86$METAL)
            BS86$LBAND[z] <- ""
            BS86$PR86[z] <- "    "
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- "12345"
            next
        }
      }
    } 
  }
}
BS86  <- BS86 [-1,] #Delete our first fake column
Lmetal <- BS86[,c("METAL", "LMETAL")] #I want to save lmetal because I feel like deleting it without using it is dumb
BS86   <- BS86  [, !names(BS86  ) %in% c("BAND", "NEWPLASTIC")] #Deletes the two columns that is specified in SAS
BS86$BAND86 <- as.character(BS86$LBAND)
BS86 <- BS86[,!names(BS86) %in% c("LBAND", "LMETAL")] #We don't do anything with LMETAL. I think I want to save it in something else in case I want to use it later?


###
#Checking if a metal/plastic band occurs more than once and creating a df to flag it. 
###

BT86 <- BS86
BT86$COUNT <- 1
BT86 <- BT86[which(BT86$FILE == "BS"), c("METAL", "BAND86", "COUNT")]
BU86 <- aggregate(COUNT~METAL, BT86, sum)
BV86 <- aggregate(COUNT~BAND86, BT86, sum)


#check if a metal band occurs more than once and create a dataframe to flag it
if(length(which(BU86$COUNT > 1)) != 0){
  BW86 <-  BU86[which(BU86$COUNT > 1),]
  BW86$COMMENTS <-  "This metal was put on 2x"
  BW86$YEAR <- "86"
  BW86 <- BX86[,!names(BX86) %in% "COUNT"]
}else{BW86 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("COMMENTS", "METAL", "YEAR"))}

#Check if a plastic band appears more than once and create a dataframe to flag it
if(length(which(BV86$COUNT > 1)) != 0){
   BX86 <-  BV86[which(BV86$COUNT > 1),]
   BX86$COMMENTS <-  "This plastic was put on 2x"
   BX86$YEAR <- "86"
   colnames(BX86)[colnames(BX86) == "BAND86"] <- "BAND"
   BX86 <- BX86[,!names(BX86) %in% "COUNT"]
}else{BX86 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("COMMENTS", "BAND", "YEAR"))}


DA86 <- BS86[which(BS86$DRIVE == "NEST" & BS86$FILE == "BS"), ]
#***For some reason they include one FILE labeled RE and I'm not sure why so I have one less observation here
DA86 <- rename(DA86, AGEB = AGE, SEXB = SEX, YEARB = YEAR)
#Is there a better way to do the below?? I can put it on less lines with ; but not necessarily(sp) more efficient
DA86$DATEB <- DA86$DATE; DA86$an86 <- DA86$AGEB; DA86$sN86 <- DA86$SEXB; DA86$Nc86 <- DA86$CUL
DA86$Nt86 <- DA86$TAR; DA86$Nm86 <- DA86$MASS; DA86$COUNT <- 1; DA86$n86 <- DA86$COLONY
DA86 <- DA86[,!names(DA86) %in% c("BSTAT", "FILE", "DRIVE", "COLONY", "BP", "YEAR", "CUL", "TAR", "MASS")]

if(length(which(DA86$METAL == ".")) != 0){
  DB86 <- DA86[which(DA86$METAL == "."),]
  DB86$COMMENTS <- "Bird was captured with plastic and realeased without metal"
  DB86$YEAR <- "86"
  colnames(DB86)[colnames(DB86) == "BAND86"] <- "BAND"
  DB86 <- DB86[,c("BAND", "COMMENTS", "YEAR")]
}else{DB86 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "COMMENTS", "YEAR"))}

#Create a dataframe with all the observations where metal is not "."
DC86 <- DA86[which(DA86$METAL != "."),]


#This is temporary, but may need to do something at the beginning so it's just nice the whole way through.
DC86$DATE <- as.integer(DC86$DATE)
DC86$Nc86 <- as.integer(DC86$Nc86)
DC86$Nt86 <- as.integer(DC86$Nt86)

#This is for when there are instances of one metal occuring more than once, when that happens we take the mean
#of Nc(cul) and Nt(tar) of the different instances, the min of the dates that occur, and we sum up how many times
#it happened more than once with count.
DD86 <- group_by(DC86, METAL) %>% summarise(meanNc86 = mean(Nc86), meanNt86 = mean(Nt86), 
                                            DATE = min(DATE), COUNTsum = sum(COUNT))

DE86 <- merge(DA86, DD86, by = c("METAL", "DATE"))
#if countsum != "." then we drop two columns. I can't see how you could do this row by row, so is it just a failsafe?
#I'm not sure when count sum could ever be "." ?????
#***This gives an warning but i'm going to keep it because it'll help me remember to come back to this if something's silly later on.
if(DE86$COUNTsum != "."){DE86 <- DE86[,!names(DE86) %in% c("Nc86", "Nt86")]}
 
DF86 <- group_by(DE86, METAL) %>% slice(sum(COUNT))
DF86 <- DF86[,!names(DE86) %in% c("n86", "COUNTsum")]

DG86 <- DF86 %>% rename(NC86 = meanNc86, NT86 = meanNt86)

DH86 <- DG86[which(DG86$NEWMETAL != "."),]
if(nrow(DH86) == 0){DH86 <- DH86 %>% rename(DEL = METAL); DH86 <- DH86 %>% select(-DEL,DEL)}else{
  DH86$DEL <- "Y"
  DH86 <- DH86[,!names(DH86) %in% "METAL"]
}

DI86 <- DH86[,c("mr86", "DEL")]
DI86 <- DI86 %>% rename(METAL = mr86)

DJ86 <- DH86[,c("NEWMETAL", "DEL")]
DJ86 <- DJ86 %>% rename(METAL = NEWMETAL)

DK86 <- bind_rows(DI86, DJ86)

#Delete required metals from the PREA/old bands data
if(nrow(DK86) == 0){DL86 <- PREA; DL86$DEL <- ""}else{ #Only runs if our DK df w/ the metals to deletes has stuff in it.
DL86 <- merge(PREA, DK86, by = "METAL")                
DL86 <- DL86[-which(DL86$DEL=="Y"),]                   #Deletes the columns we have set to delete
}
DL86$duma <- "1" #what's this dooo

#Delete required metals from the subset of our BS/recap files. DG is the nest drives and BS files from that data.
if(nrow(DK86) == 0){DM86 <- DG86; DM86$DEL <- ""}else{
  DM86 <- merge(DG86, DK86, by = "METAL")
  DM86 <- DM86[-which(DM86$DEL == "Y")]
}
DM86$dumb <- "1"
DM86 <- DM86 %>% rename(dumpr86 = PR86, webtag86 = WEBTAG, ntd86 = DATE, ageb86 = AGEB, sexb86 = SEXB, 
                        dateb86 = DATEB, yearb86 = YEARB)
#Merge our cleaned up dataframes together

#DN86 <- merge(DM86, DL86, by = c("METAL", "DEL"), all = T) #full_join in Dplyr does the same thing slightly faster. Consider changing??
DN86 <- full_join(DM86, DL86, by = c("METAL", "DEL")) #It also keeps the column order nicer which I like but that's just a small thing lol

DO86 <- DN86[which(DN86$duma == "" & DN86$dumb == "1"),] #***When will duma ever not be 1???

DP86 <- DH86 
DP86$METAL <- DP86$mr86
DQ86 <- inner_join(PREA, DP86)
#Technically it says if DEL = "Y" then drop the column but they drop it anyways when their's is empty??
#And again idk how it'd work, since you couldn't drop an individual cell from the column you'd have to do the whole thing
DQ86 <- DQ86[,!names(DQ86) %in% "METAL"]

DR86 <- DQ86 %>% rename(dumpr86 = PR86, dbd86 = DATE, webtag86 = WEBTAG, yearb86 = YEARB, dateb86 = DATEB,
                        ageb86 = AGEB, sexb86 = SEXB)
DR86$METAL <- DR86$NEWMETAL
DR86 <- DR86[,!names(DR86) %in% "DEL"]

DS86 <- NULL
DS86 <- bind_rows(DN86, DR86)
DS86$RP86 <- NA #DS86$dumpr86
DS86$AGE <- NA
DS86$SEX <- NA
DS86$DATE <- NA #DS86$dateb86
DS86$YEAR <- NA
DS86$COMMENTS <- NA
DS86$BANDB <- NA
DS86$WEBTAGB <- NA #DS86$webtag86

#K sas just doens't do anything with the info he tells it so this was pointless lol
#maybe it does do something b/c I fucked myself over later on from here whoops 
for(i in 1:nrow(DS86)){
  #Pr stuff
  #if(!is.na(DS86$dumpr86) &  ) #Don't have anything to compare this too in PREA; nothing is similar

  #Age stuff
  if(!is.na(DS86$ageb86[i]) & !is.na(DS86$BBLAGE[i]) & DS86$ageb86[i] == DS86$BBLAGE[i]){
    DS86$AGE[i] <- DS86$BBLAGE[i]}
  if(!is.na(DS86$ageb86[i]) & !is.na(DS86$BBLAGE[i]) & DS86$ageb86[i] != DS86$BBLAGE[i]){
    DS86$AGE[i] <- DS86$BBLAGE[i]
    DS86$COMMENTS[i] <- "Age at banding does not agree with BBL and BSC"
  }
  if(!is.na(DS86$ageb86[i]) & is.na(DS86$BBLAGE[i]) ){DS86$AGE[i] <- DS86$ageb86[i]}
  if(is.na(DS86$ageb86[i]) & !is.na(DS86$BBLAGE[i]) ){DS86$AGE[i] <- DS86$BBLAGE[i]}

  #Sex stuff
  if(!is.na(DS86$sexb86[i]) & !is.na(DS86$BBLSEX[i]) & DS86$sexb86[i] == DS86$BBLSEX[i]){
    DS86$SEX[i] <- DS86$BBLSEX[i]
  }
  if(!is.na(DS86$sexb86[i]) & !is.na(DS86$BBLSEX[i]) & DS86$sexb86[i] != DS86$BBLSEX[i]){
    DS86$SEX[i] <- DS86$BBLSEX[i]
    DS86$COMMENTS[i] <- "Sex at banding does not agree with BBL and BSC"
  }
  if(!is.na(DS86$sexb86[i]) & is.na(DS86$BBLSEX[i]) ){
    DS86$SEX[i] <- DS86$sexb86[i]
  }
  if(is.na(DS86$sexb86[i]) & !is.na(DS86$BBLSEX[i]) ){
    DS86$SEX[i] <- DS86$BBLSEX[i]
  }

  #Date Stuff
  #again there's not really an equivalent so not sure

  #Year Stuff
  if(!is.na(DS86$yearb86[i]) & !is.na(DS86$YEARB[i]) & DS86$yearb86[i] == DS86$YEARB[i]){
    DS86$YEAR[i] <- DS86$YEARB[i]
  }
  if(!is.na(DS86$yearb86[i]) & !is.na(DS86$YEARB[i]) & DS86$yearb86[i] != DS86$YEARB[i]){
    DS86$YEAR[i] <- DS86$YEARB[i]
    DS86$COMMENTS[i] <- "Year at banding does not agree with BBL and BSC"
  }
  if(!is.na(DS86$yearb86[i]) & is.na(DS86$YEARB[i]) ){
    DS86$YEAR[i] <- DS86$yearb86[i]
  }
  if(is.na(DS86$yearb86[i]) & !is.na(DS86$YEARB[i]) ){
    DS86$YEAR[i] <- DS86$YEARB[i]
  }

  #Band Stuff
  if(!is.na(DS86$BAND86[i]) & !is.na(DS86$BAND[i]) & DS86$BAND86[i] == DS86$BAND[i]){
    DS86$BANDB[i] <- DS86$BAND[i]
  }
  if(!is.na(DS86$BAND86[i]) & !is.na(DS86$BAND[i]) & DS86$BAND86[i] != DS86$BAND[i]
     & !is.na(DS86$RP86[i])){
    DS86$BANDB[i] <- DS86$BAND86[i]
    #my own COMMENTS b/c he didn't include one??
    DS86$COMMENTS[i] <- "Band does not agree with BBL and BSC"
  }
  if(!is.na(DS86$BAND86[i]) & !is.na(DS86$BAND[i]) & DS86$BAND86[i] != DS86$BAND[i]
     & is.na(DS86$RP86[i])){
    DS86$BANDB[i] <- DS86$BAND[i]
    DS86$COMMENTS[i] <- "Plastic bands do not agree for this recapture with original BSC"
  }
  if(!is.na(DS86$BAND86[i]) & is.na(DS86$BAND[i]) ){
    DS86$BANDB[i] <- DS86$BAND86[i]
  }
  if(is.na(DS86$BAND86[i]) & !is.na(DS86$BAND[i]) ){
    DS86$BANDB[i] <- DS86$BAND[i]
  }

  #Webtag stuff
  #Again not really a comparrison here
}
DS86 <- DS86[,!names(DS86) %in% c("NEWMETAL", "duma", "dumb", "DEL", "COUNT", "newmetal", "dumpr86",
                                  "PR86", "ageb86", "AGEB", "sexb86", "dateb86", "DATEB", "yearb86", "YEARB")]

DT86 <- DS86 %>% rename(AGEB = AGE, SEXB = SEX, DATEB= DATE, YEARB = YEAR, WEBTAG = WEBTAGB,
                        PR86 = RP86)
DT86$BAND <- DT86$BANDB
DT86 <- DT86[,!names(DT86) %in% "BANDB"]

DU86 <- DT86[, !names(DT86) %in% "COMMENTS"]
DV86 <- DT86[which(!is.na(DT86$COMMENTS)), c("COMMENTS", "METAL", "BAND", "BAND86", "WEBTAG", "webtag86")]


############################################################################
#Now we deal with nest data
############################################################################
NEST86 <- NEST86 %>% mutate_all(as.character)

FA86 <- NEST86[which(!is.na(NEST86$BAND)),]
if(nrow(FA86) != 0){
  FA86$feband <- NA
  FA86$mateband86 <- NA
  FA86$n86 <- NA
  LOC <- c("AUC", "BIG", "COL", "EC1", "EC2", "KIG", "MCN", "MCS", "BSL", "HSC", "IC1", "IC2", "IC3", 
           "IC4", "HSL", "SSL")
  for(i in 1:nrow(FA86)){
    #***Should I pad it to three spaces? It makes sure the color is right but if there isn't a band then it's
    #   four empty spaces which is a little weird.
    band <- str_pad(FA86$BAND[i], 3, side = "right") #Makes sure the bands are three spaces
    color <- str_replace_na(FA86$C1[i], " ")
    FA86$feband[i] <- paste0(band, color)
    
    mate <- str_pad((str_replace_na(FA86$MATE[i], " ")), 3, side = 'right')
    mcolor <- str_replace_na(FA86$C2[i], " ")
    FA86$mateband86[i] <- paste0(mate, mcolor)
    
    if(FA86$LOC[i] %in% LOC & !is.na(FA86$LOC[i])){FA86$n86[i] <- FA86$LOC[i]}else{FA86$n86[i] <- "TUT"}
  }
  FA86 <- FA86[, !names(FA86) %in% c("BAND", 'C1', 'MATE', 'C2')]
}else{
  FA86 <- FA86[, !names(FA86) %in% c("BAND", 'C1', 'MATE', 'C2')]
  FA86 <- setNames(data.frame(matrix(ncol = (length(colnames(FA86)) + 3), nrow = 0)), c(colnames(FA86), "feband", "mateband86", "n86"))
  FA86 <- FA86 %>% mutate_all(as.character)
}


FB86 <- NEST86[which(!is.na(NEST86$MATE)),]
if(nrow(FB86) != 0){
  FB86$mateband86 <- NA
  FB86$maband <- NA
  for(i in 1:nrow(FB86)){
    #***I think what this for loop does is create an entry for the mate of the bird as well?? Otherwise I don't know why we do this 
    band <- str_pad(FB86$BAND[i], 3, side = "right") #Makes sure the bands are three spaces
    color <- str_replace_na(FB86$C1[i], " ")
    FB86$mateband86[i] <- paste0(band, color)
    
    mate <- str_pad((str_replace_na(FB86$MATE[i], " ")), 3, side = 'right')
    mcolor <- str_replace_na(FB86$C2[i], " ")
    FB86$maband[i] <- paste0(mate, mcolor)
    
    if(FB86$LOC[i] %in% LOC & !is.na(FB86$LOC[i])){FB86$n86[i] <- FB86$LOC[i]}else{FB86$n86[i] <- "TUT"}
  }
  FB86 <- FB86[, !names(FB86) %in% c("BAND", 'C1', 'MATE', 'C2')]
}else{
  FB86 <- FB86[, !names(FB86) %in% c("BAND", 'C1', 'MATE', 'C2')]
  FB86 <- setNames(data.frame(matrix(ncol = (length(colnames(FB86)) + 3), nrow = 0)), c(colnames(FB86), "mateband86", "maband", "n86"))
  FB86 <- FB86 %>% mutate_all(as.character)
}

FC86 <- full_join(FA86, FB86)
for(i in 1:nrow(FC86)){
  if(!is.na(FC86$feband[i])){FC86$BAND[i] <- FC86$feband[i]}
  if(!is.na(FC86$maband[i])){FC86$BAND[i] <- FC86$maband[i]}
}
FC86 <- FC86[, !names(FC86) %in% c("feband", "maband")]
FD86 <- FC86
FD86$COUNT <- 1
FE86 <- group_by(FD86, BAND, .drop = FALSE) %>% summarise(COUNT = sum(COUNT))

FF86 <- FE86[which(FE86$COUNT == 1),]
FF86$DEL <- "N"

FG86 <- FE86[which(FE86$COUNT > 1),]
if(nrow(FG86) != 0){FG86$DEL <- "Y"}else{
  FG86 <- setNames(data.frame(matrix(ncol = (length(colnames(FG86)) + 1), nrow = 0)), c(colnames(FG86), "DEL"))
  FG86 <- FG86 %>% mutate_all(as.character)
  }

FI86 <- left_join(FG86, FC86)
if(nrow(FI86) != 0){
  FI86$dud <- NA
  for(i in 1:nrow(FI86)){
    if(FI86$DEL[i] == "N" & !is.na(FI86$DEL)){FI86$dud[i] <- 1}else{FI86$dud[i] <- NA}
  }
}else{
  FI86 <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("BAND", "dud"))
}

FJ86 <- group_by(FI86, BAND) %>% summarise(dud = sum(dud))

FK86 <- FJ86[,"BAND"]
if(nrow(FK86) != 0){
FK86$DEL <- "Y"
FK86$n86 <- "TUT"
}else{
  FK86 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "DEL", "n86"))
  FK86 <- FK86 %>% mutate_all(as.character)
  }

FL86 <- full_join(FC86, FK86)
FL86 <- FL86[which(FL86$DEL != "Y" | is.na(FL86$DEL)),] #Deletes columns with a Y in $DEL

FM86 <- full_join(FL86, FK86)
FM86$CS86 <- FM86$CS
FM86$CSC86 <- FM86$CSC
FM86$E86 <- FM86$E
FM86$F86 <- FM86$F
FM86$LO86 <- FM86$LO
FM86$ID86 <- FM86$ID
FM86$HD86 <- FM86$HD
FM86$HO86 <- FM86$HO
FM86$GLN86 <- FM86$GLN
FM86$ABN86 <- FM86$M
FM86$ABD86 <- FM86$ABDT
FM86$NL86 <- FM86$LOC
FM86$O86 <- FM86$O
keep <- c("NEST", 'BAND', 'n86', 'mateband86', 'CS86', 'CSC86', 'E86', 'F86', 'ID86', 
          'LO86', 'HD86', 'HO86', 'GLN86', 'ABN86', 'ABD86', 'NL86', 'O86')
FM86 <- FM86[, keep]

FO86 <- FM86 %>% rename(REALBAND = BAND)
FO86$BAND <- FO86$mateband86
FO86 <- FO86[-which(FM86$BAND == "UM" | FM86$BAND == "    "),]
if(nrow(FO86) != 0){FO86$DUM <- "Y"}else{
  FO86 <- setNames(data.frame(matrix(ncol = (length(colnames(FO86)) + 1), nrow = 0)), c(colnames(FO86), "DUM"))
  FO86 <- FO86 %>% mutate_all(as.character)
}

DU86 <- DU86 %>% mutate_all(as.character)
FP86 <- inner_join(DU86, FO86)

#***This instead of the for loop? Test when there's something to actually test on?
#FP86$MATEM86[which(FP86$DUM == "Y")] <- FP86$METAL[which(FP86$DUM == "Y")]
#FP86$MATEP86[which(FP86$DUM == "Y")] <- FP86$mateband86[which(FP86$DUM == "Y")]
if(nrow(FP86) != 0){
  FP86$MATEM86 <- NA
  FP86$MATEP86 <- NA
  for(i in 1:nrow(FP86)){
    if(FP86$DUM[i] == "Y" & !is.na(FP86$DUM[i])){
      FP86$MATEM86[i] <- FP86$METAL[i]
      FP86$MATEP86[i] <- FP86$mateband86[i]
    }
  }
  FP86[,c('MATEM86', 'NEST', 'MATEP86', 'N86', 'CS86', 'ID86', 'HD86', 'REALBAND'),]
}else{
  FP86 <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c('MATEM86', 'NEST', 'MATEP86', 'N86', 
                                                             'CS86', 'ID86', 'HD86', 'REALBAND'))
  FP86 <- FP86 %>% mutate_all(as.character)
}

FQ86 <- FP86[,c("REALBAND", "MATEM86", "MATEP86")]
FQ86 <- FQ86 %>% rename(BAND = REALBAND)

FR86 <- full_join(FQ86, FM86)
FR86$MATEP86[which(FR86$mateband86 == "UM")] <- FR86$mateband86[which(FR86$mateband86 == "UM")]
FR86 <- FR86[, !names(FR86) %in% "mateband86"]
  
FS86 <- full_join(DU86, FR86)
FS86 <- FS86[which(!is.na(FS86$METAL)),]
FS86 <- FS86[, !names(FS86) %in% "NEST"]

#hmmm when i do is.na($METAL) there's one instance, but SAS has 0 instances
#so maybe I will do == "."
FT86 <- FS86[which(FS86$METAL == "."), "BAND"] #******* I'm not sure if I should use NA here instead of "." but I'm going to until that proves dumb
if(nrow(FT86) != 0){
  FT86$YEAR <- "86"
  FT86$COMMENTS <- "Bird seen at nest, no banding record"
}else{
  FT86 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "YEAR", "COMMENTS"))
}

###
#It's H time!!! 
###

HA86 <- BS86[which(BS86$DRIVE != "NEST" | is.na(BS86$DRIVE)),]
HA86 <- HA86 %>% rename(YEARB=YEAR, Bc86= CUL, Bt86= TAR, Bm86=MASS, BP86=BP)
HA86$AGEB <- HA86$AGE
HA86$aB86 <- HA86$AGE
HA86$SEXB <- HA86$SEX
HA86$sB86 <- HA86$SEX
HA86$DATEB <- HA86$DATE
HA86$COUNT <- 1
HA86$BD86[which(HA86$COLONY == "TUT")] <- HA86$DRIVE[which(HA86$COLONY == "TUT")]
HA86$BD86[which(HA86$COLONY != "TUT" | is.na(HA86$COLONY))] <- HA86$COLONY[which(HA86$COLONY != "TUT"| is.na(HA86$COLONY))]
HA86 <- HA86[, !names(HA86) %in% c("AGE", "SEX", "BSTAT", "FILE", "DRIVE", "COLONY", "YEAR")]

HB86 <- HA86[which(HA86$METAL == "." | is.na(HA86$METAL)), c("BAND86"), drop = FALSE]
HB86 <- HB86 %>% rename(BAND = BAND86)
if(nrow(HB86) != 0){
  HB86$COMMENTS <- "Bird was captured with plastic and released without metal"
  HB86$YEAR <- "86"
}else{
  HB86 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "COMMENTS", "YEAR"))
  HB86 <- HB86 %>% mutate_all(as.character())
}

HC86 <- HA86[which(HA86$METAL != "." & !is.na(HA86$METAL)),]
HC86$Bc86 <- as.numeric(HC86$Bc86)
HC86$Bt86 <- as.numeric(HC86$Bt86)
HD86 <- group_by(HC86, METAL) %>% summarise(meanbc86 = mean(Bc86), meanbt86 = mean(Bt86),
                                            DATE = min(DATE), COUNTsum = sum(COUNT))

HE86 <- full_join(HA86, HD86)
HE86 <- HE86[which(!is.na(HE86$COUNTsum)), !names(HE86) %in% c("Bc86", "Bt86")]
HF86 <- group_by(HE86[,!names(HE86) %in% "COUNTsum"], METAL) %>% slice(COUNT = sum(COUNT))

HG86 <- HF86 %>% rename(BC86 = meanbc86, BT86 = meanbt86)
HH86 <- HG86[which(HG86$NEWMETAL != "."), !names(HG86) %in% "METAL"]
HH86$DEL <- "Y"

HI86 <- HH86[,c("mr86","DEL")] %>% rename(METAL = mr86)
HJ86 <- HH86[,c("NEWMETAL", "DEL")] %>% rename(METAL = NEWMETAL)

HK86 <- bind_rows(HI86, HJ86)
HL86 <- full_join(FS86, HK86) 
HL86 <- HL86[-which(HL86$DEL == "Y"),]
HL86$duma <- "1"

HM86 <- full_join(HG86, HK86)
HM86 <- HM86[-which(HM86$DEL == "Y"),]
HM86$dumb <- "1"
HM86 <- HM86 %>% rename(dumpr86 = PR86, webtag86 = WEBTAG, dbd86 = DATE, ageb86 = AGEB, sexb86 = SEXB, 
                        dateb86 = DATEB, yearb86 = YEARB)

HN86 <- left_join(HL86, HM86)
HO86 <- HN86[which(is.na(HN86$duma) & HN86$dumb == "1"),]
HP86 <- HH86 
HP86$METAL <- HP86$mr86
HQ86 <- full_join(FS86, HP86)
HQ86 <- HQ86[which(HQ86$DEL == "Y"), !names(HQ86) %in% "METAL"]
HR86 <- HQ86 %>% rename(dumpr86 = PR86, yearb86 = YEARB, dateb86 = DATEB,
                        ageb86 = AGEB, sexb86 = SEXB)
HR86$webtag86 <- HR86$WEBTAG
HR86$dbd86 <- HR86$DATE
HR86$METAL <- HR86$NEWMETAL
HR86 <- HR86[, !names(HR86) %in% c("WEBTAG", "DEL", "DATE")]

HS86 <- bind_rows(HN86, HR86)
HS86$RP86 <- NA
HS86$AGE <- NA
HS86$SEX <- NA
HS86$DATE <- NA
HS86$YEAR <- NA
HS86$BANDB <- NA
HS86$WEBTAGB <- NA
HS86$COMMENTS <- NA
#Takes a Hot Second: Do something different?
for(i in 1:nrow(HS86)){
  #RP
  if((!is.na(HS86$dumpr86[i]) | HS86$dumpr86[i] != "") & !is.na(HS86$PR86[i])){HS86$RP86[i] <- HS86$PR86[i]}else{
    HS86$RP86[i] <- HS86$dumpr86[i]
  }
  
  #Age
  if(!is.na(HS86$ageb86[i]) & !is.na(HS86$AGEB[i]) & HS86$ageb86[i] == HS86$AGEB[i]){HS86$AGE[i] <- HS86$AGEB[i]}
  if(!is.na(HS86$ageb86[i]) & !is.na(HS86$AGEB[i]) & HS86$ageb86[i] != HS86$AGEB[i]){
    HS86$AGE[i] <- HS86$AGEB[i]
    HS86$COMMENTS[i] <- "Age at banding does not agree with BBL and BSC"
  }
  if(!is.na(HS86$ageb86[i]) & is.na(HS86$AGEB[i]) ){HS86$AGE[i] <- HS86$ageb86[i]}
  if(is.na(HS86$ageb86[i]) & !is.na(HS86$AGEB[i]) ){HS86$AGE[i] <- HS86$AGEB[i]}
  
  #Sex
  if(!is.na(HS86$sexb86[i]) & !is.na(HS86$SEXB[i]) & HS86$sexb86[i] == HS86$SEXB[i]){HS86$SEX[i] <- HS86$SEXB[i]}
  if(!is.na(HS86$sexb86[i]) & !is.na(HS86$SEXB[i]) & HS86$sexb86[i] != HS86$SEXB[i]){
    HS86$SEX[i] <- HS86$SEXB[i]
    HS86$COMMENTS[i] <- "Sex at banding does not agree with BBL and BSC"
  }
  if(!is.na(HS86$sexb86[i]) & is.na(HS86$SEXB[i]) ){HS86$SEX[i] <- HS86$sexb86[i]}
  if(is.na(HS86$ageb86[i]) & !is.na(HS86$AGEB[i]) ){HS86$SEX[i] <- HS86$SEXB[i]}
  
  #DATE
  if(!is.na(HS86$dateb86[i]) & !is.na(HS86$DATEB[i]) & HS86$dateb86[i] == HS86$DATEB[i]){HS86$DATE[i] <- HS86$DATEB[i]}
  if(!is.na(HS86$dateb86[i]) & !is.na(HS86$DATEB[i]) & HS86$dateb86[i] != HS86$dateb86[i]){
    HS86$DATE[i] <- HS86$DATEB[i]
    HS86$COMMENTS[i] <- "Date at banding does not agree with BBL and BSC"
  }
  if(!is.na(HS86$dateb86[i]) & is.na(HS86$DATEB[i]) ){HS86$DATE[i] <- HS86$dateb86[i]}
  if(is.na(HS86$dateb86[i]) & !is.na(HS86$DATEB[i]) ){HS86$DATE[i] <- HS86$DATEB[i]}
  
  #Year
  if(!is.na(HS86$yearb86[i]) & !is.na(HS86$YEARB[i]) & HS86$yearb86[i] == HS86$YEARB[i]){HS86$YEAR[i] <- HS86$YEARB[i]}
  if(!is.na(HS86$yearb86[i]) & !is.na(HS86$YEARB[i]) & HS86$yearb86[i] != HS86$YEARB[i]){
    HS86$YEAR[i] <- HS86$YEARB[i]
    HS86$COMMENTS[i] <- "Year at banding does not agree with BBL and BSC"
  }
  if(!is.na(HS86$yearb86[i]) & is.na(HS86$YEARB[i]) ){HS86$YEAR[i] <- HS86$yearb86[i]}
  if(is.na(HS86$yearb86[i]) & !is.na(HS86$YEARB[i]) ){HS86$YEAR[i] <- HS86$YEARB[i]}
  
  #Band
  if(!is.na(HS86$BAND86[i]) & !is.na(HS86$BAND[i]) & HS86$BAND86[i] == HS86$BAND[i]){HS86$BANDB[i] <- HS86$BAND[i]}
  if(!is.na(HS86$BAND86[i]) & !is.na(HS86$BAND[i]) & HS86$BAND86[i] != HS86$BAND[i] & !is.na(HS86$RP86[i])){
    HS86$BANDB[i] <- HS86$BAND86[i]
  }
  if(!is.na(HS86$BAND86[i]) & !is.na(HS86$BAND[i]) & HS86$BAND86[i] != HS86$BAND[i] & is.na(HS86$RP86[i])){
    HS86$BANDB[i] <- HS86$BAND[i]
    HS86$COMMENTS[i] <- "Plastic bands do not agree for this recapture with original BSC"
  }
  if(!is.na(HS86$BAND86[i]) & is.na(HS86$BAND[i]) ){HS86$BANDB[i] <- HS86$BAND86[i]}
  if(is.na(HS86$BAND86[i]) & !is.na(HS86$BAND[i]) ){HS86$BANDB[i] <- HS86$BAND[i]}
  
  #Webtag
  if(!is.na(HS86$webtag86[i]) & !is.na(HS86$WEBTAG[i]) & HS86$webtag86[i] == HS86$WEBTAG[i]){HS86$WEBTAGB[i] <- HS86$WEBTAG[i]}
  if(!is.na(HS86$webtag86[i]) & !is.na(HS86$WEBTAG[i]) & HS86$webtag86[i] != HS86$WEBTAG[i]){
    HS86$WEBTAGB[i] <- HS86$WEBTAG[i]
    HS86$COMMENTS[i] <- "Webtag code has changed"
  }
  if(!is.na(HS86$webtag86[i]) & is.na(HS86$WEBTAG[i]) ){HS86$WEBTAGB[i] <- HS86$webtag86[i]}
  if(is.na(HS86$webtag86[i]) & !is.na(HS86$WEBTAG[i]) ){HS86$WEBTAGB[i] <- HS86$WEBTAG[i]}
  
  if(!is.na(HS86$PR86[i]) & is.na(HS86$RP86[i])){HS86$RP86[i] <- HS86$PR86[i]}
}

HS86 <- HS86[,!names(HS86) %in% c("NEWMETAL", "duma", "dumb", "DEL", "COUNT", "newmetal", "dumpr86",
                                  "PR86", "ageb86", "AGEB", "sexb86", "SEXB", "dateb86", "DATEB", "yearb86",
                                  "YEARB", "BAND86", "BAND", "webtag86", "WEBTAG" )]

HT86 <- HS86 %>% rename(AGEB = AGE, SEXB = SEX, DATEB = DATE, YEARB = YEAR, BAND = BANDB, WEBTAG = WEBTAGB,
                        PR86 = RP86)
HU86 <- HT86[,!names(HT86) %in% "COMMENTS"]
HV86 <- HT86[which(!is.na(HT86$COMMENTS)), c('COMMENTS', "METAL", 'BAND', 'WEBTAG')]

###
#The J's!!!! Egg Data!!!
###

JA86 <- EGG86[which(EGG86$TAG != ""),] #This may need to be NA not "", check a different year
keep <- c("COUNT", "NEST", "EGG", 'TAGD', "STATE", "DATE", "TAG")
if(nrow(JA86) != 0){
  JA86$EGG <- JA86$EGGB 
  JA86$TAGD <- JA86$DATE
  JA86$COUNT <- "1"
  JA86 <- JA86[,keep]
}else{
  JA86 <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), keep)
  JA86 <- JA86 %>% mutate_all(as.character)
}

JB86 <- EGG86[which(EGG86$LENGTH != "" | EGG86$WIDTH != ""),]
keep <- c("NEST", "EGG", "LENGTH", "WIDTH")
if(nrow(JA86) != 0){
  JB86$EGG <- JB86$EGGA
  JB86 <- JB86[,keep]
}else{
  JB86 <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), keep)
  JB86 <- JB86 %>% mutate_all(as.character)
}

JC86 <- full_join(JA86, JB86)
JC86$WEBTAG <- JC86$TAG
JC86 <- JC86[which(JC86$COUNT == 1),]
JC86$D <- JC86$TAGD
JC86$L <- JC86$LENGTH
JC86$W <- JC86$WIDTH
JC86 <- JC86[,!names(JC86) %in% c("TAGD", "LENGTH", "WIDTH", "TAG")]

JC86$COUNT <- as.numeric(JC86$COUNT)
JD86 <- JC86 %>% group_by(WEBTAG, NEST, EGG) %>% summarise(MD = mean(D), ML = mean(L), MW = mean(W), C = sum(COUNT))

JE86 <- JD86
JE86 <- JE86 %>% rename(WTD = MD, EGG1 = ML, EGGW = MW)
if(nrow(JE86) != 0){
  JE86$COUNT <- 1
}else{
  JE86 <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c(colnames(JE86), "COUNT"))
  JE86 <- JE86 %>% mutate_all(as.character)
}
JE86 <- JE86[, !names(JE86) %in% "C"]

JE86$COUNT <- as.numeric(JE86$COUNT)
JF86 <- group_by(JE86, WEBTAG) %>% summarise(COUNT = sum(COUNT))

JG86 <- JF86[which(JF86$COUNT > 1), "WEBTAG"]
if(nrow(JG86) != 0){
JG86$DEL <- "Y"
}else{
  JG86 <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("WEBTAG", "DEL"))
  JG86 <- JG86 %>% mutate_all(as.character)
}

JH86 <- JA86 %>% rename(WEBTAG = TAG)
JI86 <- full_join(JG86, JH86)
JI86 <- JI86[-which(JI86$DEL == "Y"), !names(JI86) %in% "DEL"] #Not sure if this will work but I've decided it does lol

JJ86 <- full_join(JH86, JG86)
JJ86 <- JJ86[which(JJ86$DEL == "Y"),]
keep <- c("WEBTAG", "NEST", "YEAR", "COMMENTS")
if(nrow(JJ86) != 0){
  JJ86$YEAR <- "86"
  JJ86$COMMENTS <- "This webtag put on 2 different eggs"
}else{
  JJ86 <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), keep)
  JJ86 <- JJ86 %>% mutate_all(as.character)
}

JK86 <- JI86
if(nrow(JK86) != 0){
  JK86$DUMA <- "Y"
}else{
  JK86 <- setNames(data.frame(matrix(ncol = (length(colnames(JK86)) + 1), nrow = 0)), c(colnames(JK86), "DUMA"))
  JK86 <- JK86 %>% mutate_all(as.character)
}

JK86$COUNT <- as.numeric(JK86$COUNT)
JL86 <- full_join(JK86, JE86)
JL86 <- JL86[which(JL86$DUMA == "Y"), !names(JL86) %in% c("DUMA", "COUNT", "WTD")] #Again assuming I can do this

###
#SAS COMMENTS: Starts dealing with parent info for this years webtags;
#               but identifies webtags from prior years caught this year for the first time.
###

LA86 <- FM86
LA86$dum <- 1

LB86 <- group_by(LA86, NEST) %>% summarise(dum = sum(dum))
LC86 <- NEST86
LD86 <- full_join(LC86, LB86)
LD86 <- LD86[which(LD86$dum > 0 & LD86$NEST != ""),]
for(i in 1:nrow(LD86)){
  band <- str_pad(LD86$BAND[i], 3, side = "right") #Makes sure the bands are three spaces
  color <- str_replace_na(LD86$C1[i], " ")
  LD86$PARENT1P[i] <- paste0(band, color)
  
  band2 <- str_pad((str_replace_na(LD86$MATE[i], " ")), 3, side = 'right')
  color2 <- str_replace_na(LD86$C2[i], " ")
  LD86$PARENT2P[i] <- paste0(band2, color2)
}
LD86 <- LD86[,c("NEST", "PARENT1P", "PARENT2P")]

LE86 <- LD86[which(LD86$PARENT1P != "    "),]
LE86$BAND <- LE86$PARENT1P
LE86 <- LE86[,c("NEST", "BAND")]

LF86 <- LD86[which(LD86$PARENT2P != "    "),]
LF86$BAND <- LF86$PARENT2P
LF86 <- LF86[,c("NEST", "BAND")]
LG86 <- DU86[which(!is.na(DU86$BAND) | DU86$BAND != ""), c("BAND", "METAL")] %>% rename(PARENTM = METAL)

LH86 <- full_join(LG86, LE86)
LH86 <- LH86[which(!is.na(LH86$NEST)),]  
LH86$PARENTM[which(LH86$BAND == "UM")] <- "UM"
LH86$PARENTM[which(LH86$PARENTM == "    ")] <- "banded"
LH86 <- LH86 %>% rename(PARENT1M = PARENTM, PARENT1P = BAND)

LI86 <- full_join(LG86, LF86)
LI86 <- LI86[which(!is.na(LI86$NEST)),] 
LI86$PARENTM[which(LI86$BAND == "UM")] <- "UM"
LI86$PARENTM[which(LI86$PARENTM == "    ")] <- "banded"
LI86 <- LI86 %>% rename(PARENT2M = PARENTM, PARENT2P = BAND)

LJ86 <- full_join(LH86, LI86)
LK86 <- full_join(JL86, LJ86)
LK86 <- LK86[-which(is.na(LK86$WEBTAG)),]
if(nrow(LK86) !=0){LK86$KEEP <- "Y"}else{
  LK86 <- setNames(data.frame(matrix(ncol = (length(colnames(LK86)) +1), nrow = 0)), c(colnames(LK86), "KEEP"))
  LK86 <- LK86 %>% mutate_all(as.character)
}
keep <- c("EGG1", "EGGW", "WEBTAG", "NEST", "STATE", "EGG", "TAGD", "PARENT1M", "PARENT1P", 'PARENT2M', 'PARENT2P', 'KEEP')
LK86 <- LK86[,keep] 

LL86 <- LK86
if(nrow(LL86) !=0){LL86$YEAR <- "86"}else{
  LL86 <- setNames(data.frame(matrix(ncol = (length(colnames(LL86)) +1), nrow = 0)), c(colnames(LL86), "YEAR"))
  LL86 <- LL86 %>% mutate_all(as.character)
}

LM86 <- LL86

NA86 <- HG86[,c("METAL", "WEBTAG", "AGEB", "YEARB")]

NB86 <- full_join(NA86, LK86)
NB86 <- NB86[which(NB86$WEBTAG != "" & NB86$METAL != ""),]
if(nrow(NB86) != 0){
  NB86$YEAR <- (NB86$YEARB - 1900)
  NB86 <- NB86[which(NB86$YEAR == "86"), !names(NB86) %in% "YEAR"]
}else{
  NB86 <- setNames(data.frame(matrix(nrow = 0, ncol = length(colnames(NB86)) )), colnames(NB86))
  NB86 <- NB86 %>% mutate_all(as.character) 
}

NC86 <- NB86[which(NB86$KEEP == "Y"),]
ND86 <- NB86[which(NB86$AGEB == "L" & NB86$KEEP == "Y"), c("METAL", "WEBTAG", "AGEB", "YEARB")]

NE86 <- NB86[which(NB86$AGEB == "SY"), c("METAL", "WEBTAG", "AGEB", "YEARB")]
if(nrow(NE86) != 0){
  NE86$DUMA <- "Y"
}else{
  NE86 <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("METAL", "WEBTAG", "AGEB", "YEARB", "DUMA"))
  NE86 <- NE86 %>% mutate_all(as.character)
}

NF86 <- NB86[which(NB86$AGEB == "ASY"), c("METAL", "WEBTAG", "AGEB", "YEARB")]
if(nrow(NF86) != 0){
  NF86$DUMA <- "Y"
}else{
  NF86 <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("METAL", "WEBTAG", "AGEB", "YEARB", "DUMB"))
  NF86 <- NF86 %>% mutate_all(as.character)
}

###
#Sas COMMENTS: each row must have the NH column filled in
###

NI86 <- bind_rows(NC86, ND86)
if(nrow(NI86) != 0){
  NI86$NEST[which(NI86$NEST == "")] <- "noinfo" 
}
NI86 <- NI86 %>% rename(NHID86 = NEST, EGGL86 = EGG1, EGGW86 = EGGW, STATE86 = STATE, PILS86 = EGG, TAGD86 = TAGD,
                        PARENT1M86 = PARENT1M, PARENT1P86 = PARENT1P, PARENT2M86 = PARENT2M, PARENT2P86 = PARENT2P)
NI86 <- NI86[,!names(NI86) %in% c("AGEB", "YEARB", "KEEP", "YEAR")]

NJ86 <- full_join(HU86, NI86)
NJ86$COMMENTS <- NA
#See COMMENTS # 8 where does NHID come from why I'm gonna ignore it
for(i in 1:nrow(NJ86)){
  if(!is.na(NJ86$NHID86[i])){NJ86$COMMENTS <- "Double use of applied webtag"}
}
NJ86$EGGL <- NJ86$EGGL86
NJ86$EGGW <- NJ86$EGGW86
NJ86$NHID <- NJ86$NHID86
NJ86$STATE <- NJ86$STATE86
NJ86$PILS <- NJ86$PILS86
NJ86$TAGD <- NJ86$TAGD86
NJ86$PARENT1M <- NJ86$PARENT1M86
NJ86$PARENT2M <- NJ86$PARENT2M86
NJ86$TAGD <- "   "

NK86 <- NJ86[which(!is.na(NJ86$COMMENTS)),]
if(nrow(NK86) != 0){
  NK86$YEAR <- "86"
  NK86 <- NK86[,c("METAL", "BAND", "COMMENTS", "YEAR", "WEBTAG")]
}else{
  NK86 <- setNames(data.frame(matrix(nrow = 0, ncol = 5)), c("METAL", "BAND", "COMMENTS", "YEAR", "WEBTAG"))
  NK86 <- NK86 %>% mutate_all(as.character)
}

NL86 <- NJ86[,!names(NJ86) %in% c("COMMENTS", "eggl86", "eggw86", "NHID86", "STATE86", "PILS86", "TAGD86", 
                                  "PARENT1M86", "PARENT2M86", "PARENT1P86", "PARENT2P86")]
NL86 <- NL86[-which(!is.na(NL86$BBLYEAR) & NL86$BBLYEAR < 1967),]

ERRORS86 <- bind_rows(BW86, BX86, DB86, DV86, FT86, HB86, HV86, JJ86, NK86)

#rm(list = grep(pattern = "*86", names(.GlobalEnv), value = TRUE))

  
