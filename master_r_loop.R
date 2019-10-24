# !diagnostics off
######################################
#Masterfile Script! Changing the SAS masterfile code into R
######################################

library(foreign)
library(dplyr)
library(stringr)

#SASLG88 <- read.csv("X:\\brant_data\\SASLG88.csv")

#Creates and empty dataframe where we'll store instances where a band or metal is repeated so we can look into
# the data and check for mistakes later
CheckReplicates <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("ID", "FROM", "METAL"))

#Function to pull mistake bands from whatever dataframe we're trying to summarise
#         Arg Explanations - df: dataframe you're checking
#                            groupby: The id you're grouping by, the thing you're looking for duplicates of
#                            yeardf: this is a silly way for me to find the year. I just pick a basic df with a 
#                                    year column to pull the year out from. This may change later.
#                            CheckReplicate: Just puts in the same df we're storing it all in. 
#         Col Explanations - FROM: What dataframe the duplicates were pulled from
#                            ID: What the duplicates are of 
#               Other columns are to easily look and see if there are differences in the duplicates.
#               For example if you have two of the same bands repeated you can look to the metal column and 
#               see that their metals aren't the same, and that you need to go into the data and see why. 

Mistakes <- function(x, groupby, yeardf,CheckReplicates){
  if(nrow(x) != 0){
    name <- as.character(substitute(x)) #pulls the name of the df the duplicates are from
    year <-  names(table(yeardf$YEAR))[as.vector(table(yeardf$YEAR)) == max(table(yeardf$YEAR))] #This might be ok once in giant for loop
    subset <- x[x$COUNT > 1 & !is.na(x[[noquote(groupby)]]),] %>% select(!!groupby, starts_with("METAL"), starts_with("BAND"), 
                                                                starts_with("NEST"), matches("TMATE*")) %>%
                                                         mutate(FROM = name, ID = !!groupby, YEAR = year)
    CheckReplicates <- bind_rows(CheckReplicates, subset)
  } 
  return(CheckReplicates)
}

#This is a function that takes the mean across a group (like METAL), but it handles
#   NA's better. If I didn't have this in instances where there is NA for the whole group I'd get NaN
#   and would have to change that to NA in another step but this makes it easier.
mean_ <- function(df, col){
  dumb <- mean(col, na.rm = T, data = df) 
  if(is.nan(dumb)){
    dumb <- NA
  }
  return(dumb)
}


###
#Import All the Things
###

#***Maybe include colClasses = "character" when reading in at some point to avoid issues later

#***This is COMMENTSed out in the SAS file for some reason, and instead we use BBL DATA for oldbands. Including it here b/c why not
#oldbands <- read.dbf("X:\\brant_data\\brant data\\bandsch\\1960 to present banding schedules.DBF")

#Import in Bird Banding Lab data as our old bands and do some stuff to it that i'm not sure yet
oldbands <- read.dbf("X:\\brant_data\\brant data\\bandsch\\BBL DATA.DBF")
oldbands <- oldbands %>% mutate_all(as.character)
oldbands <- unique(oldbands)

#Import Recoveries and BSCPRE (Pre 86 plastics?)
recovs <- read.csv("X:\\brant_data\\brant data\\harvest_csv\\brant_recoveries_9_28_15.csv", colClasses = "character")
BSCPRE <- read.csv("X:\\brant_data\\brant data\\bandsch_csv\\PRE 86 PLASTICS ADDED.csv", colClasses = "character")
BSCPRE <- BSCPRE[order(BSCPRE$METAL),] #Sorts the BSCPRE data by metal in ascending order

#Import BS for all years
BSTemp <- "X:\\brant_data\\brant data\\bandsch_csv" #Directory where all the BS files live
BSfilenames <- list.files(BSTemp, pattern= "bs.csv", ignore.case = TRUE, full.names = TRUE) #Finds all the files with bs.csv or BS.csv
BSC <- lapply(BSfilenames, read.csv, colClasses = "character") #Reads in all of these files #***Changed to BSC from BS
names(BSC) <- paste0("bsc", substr(c("1986":"2017"), 3,4)) #Renames the files in our lists to BSC86-BSC17 

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
ManipTemp <- "X:\\brant_data\\brant data\\manipnest\\"
Manipfilenames <- grep(list.files(path= ManipTemp, pattern = ".csv", full.names = TRUE), 
                       pattern='Egg', inv=T, value=T)
MANIP <- lapply(Manipfilenames, read.csv, colClasses = "character")
names(MANIP) <- paste0("MNEST", substr(c("2004", "2008":"2014"), 3,4))


#Import spring/winter resights, tower data, and band reads not associated with nest/broods
SPRINGMST15 <- read.csv("X:\\brant_data\\brant data\\Spring resights_csv\\SPRINGMST2015.csv", colClasses = "character")
WINTERMST16 <- read.csv("X:\\brant_data\\brant data\\winter resights_csv\\WINTER2016.csv", colClasses = "character")
TOWER <- read.dbf("X:\\brant_data\\brant data\\BAND\\TOWBRD.DBF")
TOWER <- TOWER %>% mutate_all(as.character)
NBBAND <- read.dbf("X:\\brant_data\\brant data\\BAND\\NBBAND.DBF")
NBBAND <- NBBAND %>% mutate_all(as.character)

###
#BBL DATA
###

oldstuff <- oldbands[,c("METAL", "PERMIT")]
oldstuff$BBLAGE <- NA
oldstuff$BBLSEX <- NA

#This for loop takes a while, change it?
#Potentially use which to find the specific cases, then change it? Like we've done ahead, go find that. 
#This follows codes from the BBL found here -> https://www.pwrc.usgs.gov/bbl/manual/age.cfm
for(i in 1:nrow(oldbands)){
  if(oldbands$NAGE[i] == "0" & !is.na(oldbands$NAGE[i])){oldstuff$BBLAGE[i] <- "U"}
  if(oldbands$NAGE[i] == "1" & !is.na(oldbands$NAGE[i])){oldstuff$BBLAGE[i] <- "AHY"}
  if(oldbands$NAGE[i] == "2" & !is.na(oldbands$NAGE[i])){oldstuff$BBLAGE[i] <- "HY"}
  #There are no 3's because we never catch Brants as Juveniles. 
  if(oldbands$NAGE[i] == "4" & !is.na(oldbands$NAGE[i])){oldstuff$BBLAGE[i] <- "L"}
  if(oldbands$NAGE[i] == "5" & !is.na(oldbands$NAGE[i])){oldstuff$BBLAGE[i] <- "SY"}
  if(oldbands$NAGE[i] == "6" & !is.na(oldbands$NAGE[i])){oldstuff$BBLAGE[i] <- "ASY"}
  if(oldbands$NAGE[i] == "7" & !is.na(oldbands$NAGE[i])){oldstuff$BBLAGE[i] <- "TY"}
  if(oldbands$NAGE[i] == "8" & !is.na(oldbands$NAGE[i])){oldstuff$BBLAGE[i] <- "ATY"}
  
  if(oldbands$NSEX[i] == "0" & !is.na(oldbands$NSEX[i])){oldstuff$BBLSEX[i] <- "U"} 
  if(oldbands$NSEX[i] == "4" & !is.na(oldbands$NSEX[i])){oldstuff$BBLSEX[i] <- "M"} 
  if(oldbands$NSEX[i] == "5" & !is.na(oldbands$NSEX[i])){oldstuff$BBLSEX[i] <- "F"}
}
oldstuff$BBLYEAR <- oldbands$BYEAR
oldstuff$BBLSEX[which(is.na(oldstuff$BBLSEX))] <- ""

#PREA are all the banded birds that existed before our study. So we're just taking them into account 
PREA <- merge(BSCPRE, oldstuff, by = "METAL", all = T)

newcols <- c("YEARB","AN84", "SN84", "AB85", "SB85") #New columns we're adding to PREA
PREA[newcols] <- '' #Creates new columns and just leaves them empty

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


#************Some COMMENTs that I don't know what they mean yet
#/*When updating the program; 
# 1)AA## needs to be changed to prior year (except for 86, when it gets PREA) 
# 2)for 86, the first errorlist has to have errorlist removed from the set statement
# 3)for 86, delete AB86 and AC86 as they do not exist
# 4)for 86, delete the AC86 form the LM86 step
# 5)for 86, delete the NG86 and NH86 steps
# 6)for 86, delete the NH86 and NH86 set statements in the NI86 datastep*/ 

#A function that will pull all the files we'll need for a specific year
addToEnv <- function(list, regex){
  for(i in list){
    matches <- grep(pattern = regex, names(get(i)))
    list2env(get(i)[matches], envir = .GlobalEnv)
  }
}



lists <- c('BSC','EGG','NEST','RECAP', 'MANIP') #List of the type of files will be pulling from
#years <- c(86:99, sprintf("%02d", c(00:15))) #Vector of the different years we'll be looping through
years <- c("86", "87", "88" )

#This a tad confusing, so we work with winter years the year before. So for example when coding for year 90 
#we have code for WINTER91; so winter_yrs are the years that will correlate w/ f for where we should run 
#winter code, and not the years that the winter code is... i'm so sorry 
winter_yrs <- c("90":"99", sprintf("%02d", c("00":"05")), "10":"12")

manip_yrs <- c("04", sprintf("%02d", c("08":"15")))

###
#Start of Big Loop^TM
###

for(f in years){
  if(f == "86"){
    AA <- PREA
    }else if(f == "87"){
      AA <- NL
      AB <- LL
      AC <- LM
      prev_NL <- NL
    }else{
      AA <- NN
      AB <- LL
      AC <- LM
      prev_NL <- NL
    }
  
  addToEnv(list = lists, regex = paste0("*", f)) #Calls the function
  #***rm(list = grep(pattern = "*86", names(.GlobalEnv), value = TRUE)) I need to think about this one for a sec
  
  BA <- get(paste0("bsc", f))
  BA$FILE <- "BS"
  
  for(i in nrow(BA):1){
    if( (BA$METAL[i] == "" | is.na(BA$METAL[i])) & (BA$BAND[i] == "" | is.na(BA$BAND[i])) ){ #changing metal == "." to ""
      BA <- BA[-i,]
    }  
    if(BA$BSTAT[i] == "L" & !is.na(BA$BSTAT[i])){ 
      BA <- BA[-i,]
    }
  }
  BA <- BA[ , !names(BA) %in% c("NINTHPRIM","BODYLEN", 'SAMPLENO', 'COMMENTS', 'NEWPLASTIC', 'NEWMETAL')] #Deletes these rows
  
  BB <- get(paste0("recap", f))
  BB$FILE <- "RE"
  for(i in nrow(BB):1){
    if( (BB$METAL[i] == "" | is.na(BB$METAL[i])) & (BB$BAND[i] == "" | is.na(BB$BAND[i])) ){ #changing metal == "." to ""
      BB <- BB[-i,]
    } 
  }
  BB <- BB[ , !names(BB) %in% c("NINTHPRIM","BODYLEN", 'SAMPLENO', 'COMMENTS', 'RE_STAT')]
  
  BC <- bind_rows(BA, BB)
  BC$NEWMETAL[which((is.na(BC$NEWMETAL)| BC$NEWMETAL == "" )) ] <- NA # changing "." to NA
  BC$NEWPLASTIC[which(BC$NEWPLASTIC == "")] <- NA
  
  BCols <- c("LBAND", paste0("PR", f), "LMETAL", paste0("mr", f))
  BS <- BC[1,]  #Initializing BS to have the same columns as BC
  BS[BCols] <- NA #Adding the new columns and setting them to NA to start
  BS[1,] <- NA #First fake row we'll get rid of later. I have it for the second for loop?? because I thinkt trying to loop through the 0 column would cause issues?
  
  for(i in 1:nrow(BC)){
    #If METAL > 0
    if(BC$METAL[i] > 0 & !is.na(BC$METAL[i])){
      #If BAND != ""
      if(BC$BAND[i] != "" & !is.na(BC$BAND[i])){
        #IF NEWPLASTIC != ""
        if( !is.na(BC$NEWPLASTIC[i]) ){
          #IF NEWMETAL > 0 
          if(BC$NEWMETAL[i] > 0 & !is.na(BC$NEWMETAL[i])){
            #equivalent to BD
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$NEWPLASTIC[i]
            BS[[paste0("PR",f)]][z] <- as.character(BC$BAND[i]) 
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS[[paste0("mr",f)]][z] <- BC$METAL[i]
            next
            #IF NEWMETAL == "." 
          }else if( is.na(BC$NEWMETAL[i]) ){ 
            #equivalent to BE
            BS <- bind_rows(BS, BC[i,])
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$NEWPLASTIC[i]
            BS[[paste0("PR",f)]][z] <- as.character(BC$BAND[i]) 
            BS$LMETAL[z] <- BC$METAL[i]
            BS[[paste0("mr",f)]][z] <- NA 
            next
          }#IF NEWPLASTIC == ""
        }else if( is.na(BC$NEWPLASTIC[i]) ){
          #IF NEWMETAL > 0 
          if(BC$NEWMETAL[i] > 0 & !is.na(BC$NEWMETAL[i])){
            #equivalent to BF
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$BAND[i]
            BS[[paste0("PR",f)]][z] <- NA
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS[[paste0("mr",f)]][z] <- BC$METAL[i]
            next
            #IF NEWMETAL == "."
          }else if( is.na(BC$NEWMETAL[i]) ){ 
            #equivalent to BG
            BS <- bind_rows(BS, BC[i,])
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$BAND[i]
            BS[[paste0("PR",f)]][z] <- NA
            BS$LMETAL[z] <- BC$METAL[i]
            BS[[paste0("mr",f)]][z] <- NA 
            next
          }
        }#If BAND == ""
      }else if(BC$BAND[i] == "" & !is.na(BC$BAND[i])){
        #IF NEWPLASTIC != ""
        if( !is.na(BC$NEWPLASTIC[i]) ){
          #IF NEWMETAL > 0
          if(BC$NEWMETAL[i] > 0 & !is.na(BC$NEWMETAL[i])){
            #equivalent to BH
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$NEWPLASTIC[i]
            BS[[paste0("PR",f)]][z] <- "PBA "
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS[[paste0("mr",f)]][z] <- BC$METAL[i]
            next
            #IF NEWMETAL == "."
          }else if( is.na(BC$NEWMETAL[i]) ){ 
            #equivalent to BI
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$NEWPLASTIC[i]
            BS[[paste0("PR",f)]][z] <- "PBA "
            BS$LMETAL[z] <- BC$METAL[i]
            BS[[paste0("mr",f)]][z] <- NA 
            next
          }#IF NEWPLASTIC == ""
        }else if( is.na(BC$NEWPLASTIC[i]) ){
          #IF NEWMETAL > 0
          if(BC$NEWMETAL[i] > 0 & !is.na(BC$NEWMETAL[i])){
            #equivalent to BJ
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- NA#""
            BS[[paste0("PR",f)]][z] <- NA
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS[[paste0("mr",f)]][z] <- BC$METAL[i]
            next
            #IF NEWMETAL == "."
          }else if( is.na(BC$NEWMETAL[i]) ){ 
            #equivalent to BK
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- NA#""
            BS[[paste0("PR",f)]][z] <- NA
            BS$LMETAL[z] <- BC$METAL[i]
            BS[[paste0("mr",f)]][z] <- NA 
            next
          }
        }
      }
      #If METAL == "." 
    }else if(BC$METAL[i] == "" | is.na(BC$METAL[i]) ){ 
      if(BC$BAND[i] != "" & !is.na(BC$BAND[i])){
        #IF NEWPLASTIC != ""
        if( !is.na(BC$NEWPLASTIC[i]) ){
          #IF NEWMETAL > 0
          if(BC$NEWMETAL[i] > 0 & !is.na(BC$NEWMETAL[i])){
            #equivalent to BL
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$NEWPLASTIC[i]
            BS[[paste0("PR",f)]][z] <- as.character(BC$BAND[i]) 
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS[[paste0("mr",f)]][z] <- "12345"
            next
            #IF NEWMETAL == "."
          }else if( is.na(BC$NEWMETAL[i]) ){ 
            #equivalent to BM
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$NEWPLASTIC[i]
            BS[[paste0("PR",f)]][z] <- as.character(BC$BAND[i]) 
            BS$LMETAL[z] <- NA#"."
            BS[[paste0("mr",f)]][z] <- NA 
            next
          }#IF NEWPLASTIC == ""
        }else if( is.na(BC$NEWPLASTIC[i]) ){
          #IF NEWMETAL > 0
          if(BC$NEWMETAL[i] > 0 & !is.na(BC$NEWMETAL[i])){
            #equivalent to BN
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$BAND[i]
            BS[[paste0("PR",f)]][z] <- NA 
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS[[paste0("mr",f)]][z] <- "12345"
            next
            #IF NEWMETAL == "."
          }else if( is.na(BC$NEWMETAL[i]) ){
            #equivalent to BO
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$BAND[i]
            BS[[paste0("PR",f)]][z] <- NA 
            BS$LMETAL[z] <- NA #"."
            BS[[paste0("mr",f)]][z] <- NA 
            next
          }
        }#If BAND == ""
      }else if(BC$BAND[i] == "" & !is.na(BC$BAND[i])){
        #IF NEWPLASTIC != ""
        if( !is.na(BC$NEWPLASTIC[i]) ){
          #IF NEWMETAL > 0
          if(BC$NEWMETAL[i] > 0 & !is.na(BC$NEWMETAL[i])){
            #equivalent to BP
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$NEWPLASTIC[i]
            BS[[paste0("PR",f)]][z] <- "PBA "
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS[[paste0("mr",f)]][z] <- "12345"
            next
            #IF NEWMETAL == "."
          }else if(is.na(BC$NEWMETAL[i]) ){ 
            #equivalent to BQ
            BS <- bind_rows(BS, BC[i,])
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$NEWPLASTIC[i]
            BS[[paste0("PR",f)]][z] <- "PBA "
            BS$LMETAL[z] <- NA 
            BS[[paste0("mr",f)]][z] <- NA 
            next
          }#IF NEWPLASTIC == ""
        }else if( is.na(BC$NEWPLASTIC[i]) ){
          #IF NEWMETAL > 0
          if(BC$NEWMETAL[i] > 0 & !is.na(BC$NEWMETAL[i])){
            #equivalent to BR
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- NA
            BS[[paste0("PR",f)]][z] <- NA 
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS[[paste0("mr",f)]][z] <- "12345"
            next
          }
        }
      } 
    }
  }
  BS <- BS[-1,] #Delete our first fake column

  BS[[paste0("BAND", f)]] <- as.character(BS$LBAND) #Suppose to have an 86 yr extension
  BS <- BS[, !names(BS) %in% c("BAND", "NEWPLASTIC", "LBAND", "LMETAL")] #Deletes the two columns that is specified in SAS
  BS <- BS %>% mutate_if(is.logical,as.character)
  
  ###
  #Checking if a metal/plastic band occurs more than once and creating a df to flag it. 
  ###
  
  BT <- BS
  BT$COUNT <- 1
  BT <- BT[BT$FILE == "BS", c("METAL", paste0("BAND", f), "COUNT")]
  BU <- BT %>% group_by(METAL) %>% mutate(COUNT = sum(COUNT))
  CheckReplicates <- Mistakes(x = BU, groupby = "METAL", yeardf = BA, CheckReplicates)
  BV <- BT %>% group_by_at(noquote(paste0("BAND", f))) %>% mutate(COUNT = sum(COUNT))
  CheckReplicates <- Mistakes(x = BV, groupby = paste0("BAND", f), yeardf = BA, CheckReplicates)
  
  if(length(which(BU$COUNT > 1)) != 0){
    BW <-  BU[BU$COUNT > 1,]
    BW$COMMENTS <-  "This metal was put on 2x"
    BW$YEAR <- f
    BW <- BW[,!names(BW) %in% "COUNT"]
  }else{BW <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("COMMENTS", "METAL", "YEAR"))}
  
  #Check if a plastic band appears more than once and create a dataframe to flag it
  if(length(which(BV$COUNT > 1)) != 0){
    BX <-  BV[BV$COUNT > 1,]
    BX$COMMENTS <-  "This plastic was put on 2x"
    BX$YEAR <- f
    colnames(BX)[colnames(BX) == paste0("BAND", f)] <- "BAND"
    BX <- BX[,!names(BX) %in% "COUNT"]
  }else{BX <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("COMMENTS", "BAND", "YEAR"))}
  
  DA <- BS[BS$DRIVE == "NEST", ]
  DA$AGEB <- NA
  DA$SEXB <- NA
  DA$DATEB <- NA
  DA$YEARB <- NA
  for(i in 1:nrow(DA)){
    if(DA$FILE[i] == "BS"){
      DA$AGEB[i] <- DA$AGE[i]
      DA$SEXB[i] <- DA$SEX[i]
      DA$DATEB[i] <- DA$DATE[i]
      DA$YEARB[i] <- DA$YEAR[i]
    }
  }
  
  DA <- DA %>% rename(!!paste0("an", f) := AGE, !!paste0("sN", f) := SEX, !!paste0("Nc", f) := CUL, !!paste0("Nt", f) := TAR,
                      !!paste0("Nm", f) := MASS, !!paste0("n", f) := COLONY)
  DA$COUNT <- 1 
  DA <- DA[,!names(DA) %in% c("BSTAT", "FILE", "DRIVE", "BP", "YEAR")]
  
  if(length(which(DA$METAL == "" | is.na(DA$METAL))) != 0){ #Changed from "."
    DB <- DA[(DA$METAL == "" | is.na(DA$METAL)),]
    DB$COMMENTS <- "Bird was captured with plastic and realeased without metal"
    DB$YEAR <- f
    colnames(DB)[colnames(DB) == paste0("BAND", f)] <- "BAND"
    DB <- DB[,c("BAND", "COMMENTS", "YEAR")]
  }else{DB <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "COMMENTS", "YEAR"))}
  
  #Create a dataframe with all the observations where metal is not "."
  DC <- DA[DA$METAL != "" & !is.na(DA$METAL),]
  
  #***This is temporary, but may need to do something at the beginning so it's just nice the whole way through.
  DC$DATE <- as.integer(DC$DATE)
  DC[[paste0("Nc", f)]] <- as.integer(DC[[paste0("Nc", f)]])
  DC[[paste0("Nt", f)]] <- as.integer(DC[[paste0("Nt", f)]])
  
  #This is for when there are instances of one metal occuring more than once, when that happens we take the mean
  #of Nc(cul) and Nt(tar) of the different instances, the min of the dates that occur, and we sum up how many times
  #it happened more than once with count.
  DD <- DC %>% group_by(METAL) %>% mutate(COUNT = sum(COUNT))
  CheckReplicates <- Mistakes(DD, groupby = "METAL", yeardf = BA, CheckReplicates)
  DD <- group_by(DC, METAL) %>% summarise(!!paste0("meanNc",f) := mean_(DC, !!as.name(paste0("Nc", f))), 
                                          !!paste0("meanNt",f) := mean_(DC, !!as.name(paste0("Nt", f))), 
                                          DATE = min(DATE), COUNTsum = sum(COUNT)) 
  #This one I think is ok to have replicates and to use summarise to combine. Because it includes Recap and BS data
  #   which we combined in BS, so we might have duplicate metals because of that and taking the mean between 
  #   them makes sense I think ???????????
  
  DE <- merge(DA, DD, by = c("METAL", "DATE"))
  DE <- DE[(!is.na(DE$COUNTsum)), !names(DE) %in% c(paste0("Nc", f), paste0("Nt", f))]
  
  DF <- DE %>% group_by(METAL) %>% mutate(COUNT = sum(COUNTsum)) 
  CheckReplicates <- Mistakes(DF, groupby="METAL", yeardf = BA ,CheckReplicates)
  DF <- DF[!(DF$COUNT > 1),!names(DF) %in% c(paste0("n", f), "COUNTsum")]
  
  DG <- DF %>% rename(!!paste0("NC", f) := !!paste0("meanNc",f), !!paste0("NT", f) := !!paste0("meanNt",f))
  
  DH <- DG[(!is.na(DG$NEWMETAL)),] #Changed from != "." to !is.na
  if(nrow(DH) == 0){DH <- DH %>% rename(DEL = METAL); DH <- DH %>% select(-DEL,DEL)}else{
    DH$DEL <- "Y"
    DH <- DH[,!names(DH) %in% "METAL"]
  }

  DI <- DH[,c(paste0("mr",f), "DEL")]
  DI <- DI %>% rename(METAL := !!paste0("mr",f))
  
  DJ <- DH[,c("NEWMETAL", "DEL")]
  DJ <- DJ %>% rename(METAL = NEWMETAL)
  
  DK <- bind_rows(DI, DJ)

  #Delete required metals from the PREA/old bands data
  if(nrow(DK) == 0){DL <- AA; DL$DEL <- ""}else{ #Only runs if our DK df w/ the metals to deletes has stuff in it.
    DL <- merge(AA, DK, by = "METAL")                
    DL <- DL[!(DL$DEL=="Y"),]                   #Deletes the columns we have set to delete
  }
  DL$duma <- "1" #what's this dooo
  
  #Delete required metals from the subset of our BS/recap files. DG is the nest drives and BS files from that data.
  if(nrow(DK) == 0){DM <- DG; DM$DEL <- ""}else{
    DM <- merge(DG, DK, by = "METAL")
    DM <- DM[!(DM$DEL == "Y")]
  }
  DM$dumb <- "1"
  DM <- DM %>% rename(!!paste0("dumpr",f) := !!paste0("PR",f), !!paste0("webtag",f) := WEBTAG, !!paste0("ntd",f) := DATE, 
                      !!paste0("ageb",f) := AGEB, !!paste0("sexb",f) := SEXB, !!paste0("dateb",f) := DATEB, 
                      !!paste0("yearb",f) := YEARB)
  
  #Merge our cleaned up dataframes together
  DN <- full_join(DM, DL) 
  
  DO <- DN[( is.na(DN$duma) & DN$dumb == "1"),] #Changed duma = "" to be is.na, since from my dumb test I think that's what it'd be
  
  DP <- DH 
  DP$METAL <- as.character(DP[[paste0("mr",f)]])

  DQ <- full_join(AA, DP)
  DQ <- DQ[which(DQ$DEL == "Y"),!names(DQ) %in% "METAL"]
  
  DR <- DQ %>% rename(!!paste0("dumpr",f) := !!paste0("PR",f), !!paste0("dbd",f) := DATE, !!paste0("webtag",f) := WEBTAG, 
                      !!paste0("yearb",f) := YEARB, !!paste0("dateb",f) := DATEB, !!paste0("ageb",f) := AGEB, 
                      !!paste0("sexb",f) := SEXB)
  
  DR$METAL <- DR$NEWMETAL
  DR <- DR[,!names(DR) %in% "DEL"] 
  
  DS <- bind_rows(DN, DR)
  #Old columns that aren't there so I'm adding them I guesss
  DS[[paste0("PR",f)]] <- NA
  if(f == "86"){
    DS$AGEB <- NA
    DS$SEXB <- NA
    DS$DATEB <- NA
    DS$WEBTAG <- NA
  }
  
  #New columns we're creating
  DCols <- c(paste0("RP",f), "AGE", "SEX", "DATE", "YEAR", "COMMENTS", "BANDB", "WEBTAGB")
  DS[DCols] <- NA

  #***Temporary b/c i'm lazy?
  #Could add this in earlier when populating YEARB and yearb86 ?? Initialize it as NA?
  #idk but put it somewhere smarter later
  DS$YEARB[which(DS$YEARB == "")] <- NA
  DS[[paste0("yearb", f)]][which(DS[[paste0("yearb", f)]] == "")] <- NA
  
  for(i in 1:nrow(DS)){
    #Pr stuff
    if( !is.na(DS[[paste0("dumpr", f)]][i]) & !is.na(DS[[paste0("PR", f)]][i]) ){ DS[[paste0("RP", f)]][i] <- DS[[paste0("PR", f)]][i] }else{
      DS[[paste0("RP", f)]][i] <- DS[[paste0("dumpr", f)]][i]
      } 
    
    #Age stuff
    if(!is.na(DS[[paste0("ageb", f)]][i]) & !is.na(DS$AGEB[i]) & DS[[paste0("ageb", f)]][i] == DS$AGEB[i]){
      DS$AGE[i] <- DS$AGEB[i]}
    if(!is.na(DS[[paste0("ageb", f)]][i]) & !is.na(DS$AGEB[i]) & DS[[paste0("ageb", f)]][i] != DS$AGEB[i]){
      DS$AGE[i] <- DS$AGEB[i]
      DS$COMMENTS[i] <- "Age at banding does not agree with BBL and BSC"
    }
    if(!is.na(DS[[paste0("ageb", f)]][i]) & is.na(DS$AGEB[i]) ){DS$AGE[i] <- DS[[paste0("ageb", f)]][i]}
    if(is.na(DS[[paste0("ageb", f)]][i]) & !is.na(DS$AGEB[i]) ){DS$AGE[i] <- DS$AGEB[i]}
    
    #Sex stuff
    if(!is.na(DS[[paste0("sexb", f)]][i]) & !is.na(DS$SEXB[i]) & DS[[paste0("sexb", f)]][i] == DS$SEXB[i]){
      DS$SEX[i] <- DS$SEXB[i]
    }
    if(!is.na(DS[[paste0("sexb", f)]][i]) & !is.na(DS$SEXB[i]) & DS[[paste0("sexb", f)]][i] != DS$SEXB[i]){
      DS$SEX[i] <- DS$SEXB[i]
      DS$COMMENTS[i] <- "Sex at banding does not agree with BBL and BSC"
    }
    if(!is.na(DS[[paste0("sexb", f)]][i]) & is.na(DS$SEXB[i]) ){
      DS$SEX[i] <- DS[[paste0("sexb", f)]][i]
    }
    if(is.na(DS[[paste0("sexb", f)]][i]) & !is.na(DS$SEXB[i]) ){
      DS$SEX[i] <- DS$SEXB[i]
    }
    
    #Date Stuff
    if(!is.na(DS[[paste0("dateb", f)]][i]) & !is.na(DS$DATEB[i]) & DS[[paste0("dateb", f)]][i] == DS$DATEB[i]){
      DS$DATE[i] <- DS$DATEB[i]}
    if(!is.na(DS[[paste0("dateb", f)]][i]) & !is.na(DS$DATEB[i]) & DS[[paste0("dateb", f)]][i] != DS$DATEB[i]){
      DS$DATE[i] <- DS$DATEB[i]
      DS$COMMENTS[i] <- "Date at banding does not agree with BBL and BSC"
    }
    if(!is.na(DS[[paste0("dateb", f)]][i]) & is.na(DS$DATEB[i]) ){DS$DATE[i] <- DS[[paste0("dateb", f)]][i]}
    if(is.na(DS[[paste0("dateb", f)]][i]) & !is.na(DS$DATEB[i]) ){DS$DATE[i] <- DS$DATEB[i]}
    
    #Year Stuff
    if(!is.na(DS[[paste0("yearb", f)]][i]) & !is.na(DS$YEARB[i]) & DS[[paste0("yearb", f)]][i] == DS$YEARB[i]){
      DS$YEAR[i] <- DS$YEARB[i]
    }
    if(!is.na(DS[[paste0("yearb", f)]][i]) & !is.na(DS$YEARB[i]) & DS[[paste0("yearb", f)]][i] != DS$YEARB[i]){
      DS$YEAR[i] <- DS$YEARB[i]
      DS$COMMENTS[i] <- "Year at banding does not agree with BBL and BSC"
    }
    if(!is.na(DS[[paste0("yearb", f)]][i]) & is.na(DS$YEARB[i]) ){
      DS$YEAR[i] <- DS[[paste0("yearb", f)]][i]
    }
    if(is.na(DS[[paste0("yearb", f)]][i]) & !is.na(DS$YEARB[i]) ){
      DS$YEAR[i] <- DS$YEARB[i]
    }
    
    #Band Stuff
    if(!is.na(DS[[paste0("BAND", f)]][i]) & !is.na(DS$BAND[i]) & DS[[paste0("BAND", f)]][i] == DS$BAND[i]){
      DS$BANDB[i] <- DS$BAND[i]
    }
    if(!is.na(DS[[paste0("BAND", f)]][i]) & !is.na(DS$BAND[i]) & DS[[paste0("BAND", f)]][i] != DS$BAND[i]
       & !is.na(DS[[paste0("RP", f)]][i])){
      DS$BANDB[i] <- DS[[paste0("BAND", f)]][i]
      #my own COMMENTS b/c he didn't include one??
      DS$COMMENTS[i] <- "Band does not agree with BBL and BSC"
    }
    if(!is.na(DS[[paste0("BAND", f)]][i]) & !is.na(DS$BAND[i]) & DS[[paste0("BAND", f)]][i] != DS$BAND[i]
       & is.na(DS[[paste0("RP", f)]][i])){
      DS$BANDB[i] <- DS$BAND[i]
      DS$COMMENTS[i] <- "Plastic bands do not agree for this recapture with original BSC"
    }
    if(!is.na(DS[[paste0("BAND", f)]][i]) & is.na(DS$BAND[i]) ){
      DS$BANDB[i] <- DS[[paste0("BAND", f)]][i]
    }
    if(is.na(DS[[paste0("BAND", f)]][i]) & !is.na(DS$BAND[i]) ){
      DS$BANDB[i] <- DS$BAND[i]
    }
    
    #Webtag stuff
    if(!is.na(DS[[paste0("webtag", f)]][i]) & !is.na(DS$WEBTAG[i]) & DS[[paste0("webtag", f)]][i] == DS$WEBTAG[i]){
      DS$WEBTAGB[i] <- DS$WEBTAG[i]}
    if(!is.na(DS[[paste0("webtag", f)]][i]) & !is.na(DS$WEBTAG[i]) & DS[[paste0("webtag", f)]][i] != DS$WEBTAG[i]){
      DS$WEBTAGB[i] <- DS$WEBTAG[i]
      DS$COMMENTS[i] <- "Age at banding does not agree with BBL and BSC"
    }
    if(!is.na(DS[[paste0("webtag", f)]][i]) & is.na(DS$WEBTAG[i]) ){DS$WEBTAGB[i] <- DS[[paste0("webtag", f)]][i]}
    if(is.na(DS[[paste0("webtag", f)]][i]) & !is.na(DS$WEBTAG[i]) ){DS$WEBTAGB[i] <- DS$WEBTAG[i]}
  }
  DS <- DS[,!names(DS) %in% c("NEWMETAL", "duma", "dumb", "DEL", "COUNT", "newmetal", paste0("dumpr", f),
                                    paste0("PR", f), paste0("ageb", f), "AGEB", paste0("sexb", f), "SEXB",paste0("dateb", f), 
                                    "DATEB", paste0("yearb", f), "YEARB")]
  if(f != "86"){
    DS <- DS[,!names(DS) %in% c(paste0("BAND", f), "BAND", paste0("webtag", f), "WEBTAG")]
  }
  
  DT <- DS %>% rename(AGEB = AGE, SEXB = SEX, DATEB= DATE, YEARB = YEAR, !!paste0("PR", f) := !!paste0("RP", f))
  DT$WEBTAG <- DT$WEBTAGB
  DT$BAND <-  DT$BANDB
  DT <- DT[,!names(DT) %in% c("BANDB", "WEBTAGB")]
  
  DU <- DT[, !names(DT) %in% "COMMENTS"]
  DV <- DS[(!is.na(DT$COMMENTS)), c("COMMENTS", "METAL")] # "BAND", paste0("BAND", f), "WEBTAG", paste0("webtag", f))
  
  ############################################################################
  #Now we deal with nest data
  ############################################################################
  #Had to change to assign to do a dynamic dataframe, different from other file
  assign(paste0("NEST", f), get(paste0("NEST", f)) %>% mutate_all(as.character)) #NEST86 <- NEST86 %>% mutate_all(as.character)
  
  FA <- get(paste0("NEST", f))[(!is.na(get(paste0("NEST", f))[["BAND"]])),]
  FA <- FA[FA$BAND != "UM",]
  if(nrow(FA) != 0){
    FA$feband <- NA
    FA[[paste0("mateband",f)]] <- NA
    FA[[paste0("n", f)]] <- NA
    LOC <- c("AUC", "BIG", "COL", "EC1", "EC2", "KIG", "MCN", "MCS", "BSL", "HSC", "IC1", "IC2", "IC3", 
             "IC4", "HSL", "SSL")
    for(i in 1:nrow(FA)){
      FA$feband[i] <- paste0(trimws(str_replace_na(FA$BAND[i], "")), trimws(str_replace_na(FA$C1[i], "")))
      FA[[paste0("mateband",f)]][i] <- paste0(trimws(str_replace_na(FA$MATE[i], "")), trimws(str_replace_na(FA$C2[i], "")))
      
      if(FA$LOC[i] %in% LOC & !is.na(FA$LOC[i])){FA[[paste0("n", f)]][i] <- FA$LOC[i]}else{FA[[paste0("n", f)]][i] <- "TUT"}
    }
    FA <- FA[, !names(FA) %in% c("BAND", 'C1', 'MATE', 'C2')]
  }else{
    FA <- FA[, !names(FA) %in% c("BAND", 'C1', 'MATE', 'C2')]
    FA <- setNames(data.frame(matrix(ncol = (length(colnames(FA)) + 3), nrow = 0)), c(colnames(FA), "feband", 
                                                                                      paste0("mateband",f), paste0("n", f)))
    FA <- FA %>% mutate_all(as.character)
  }

  FB <- get(paste0("NEST", f))[(!is.na(get(paste0("NEST", f))[["MATE"]])),]
  FB <- FB[FB$MATE != "UM",]
  if(nrow(FB) != 0){
    FB[[paste0("mateband",f)]] <- NA
    FB$maband <- NA
    for(i in 1:nrow(FB)){
      #***I think what this for loop does is create an entry for the mate of the bird as well?? Otherwise I don't know why we do this 
      FB[[paste0("mateband",f)]][i] <- paste0(trimws(str_replace_na(FB$BAND[i], "")), trimws(str_replace_na(FB$C1[i], "")))
      FB$maband[i] <- paste0(trimws(str_replace_na(FB$MATE[i], "")), trimws(str_replace_na(FB$C2[i], "")))
      
      if(FB$LOC[i] %in% LOC & !is.na(FB$LOC[i])){FB[[paste0("n", f)]][i] <- FB$LOC[i]}else{FB[[paste0("n", f)]][i] <- "TUT"}
    }
    FB <- FB[, !names(FB) %in% c("BAND", 'C1', 'MATE', 'C2')]
  }else{
    FB <- FB[, !names(FB) %in% c("BAND", 'C1', 'MATE', 'C2')]
    FB <- setNames(data.frame(matrix(ncol = (length(colnames(FB)) + 3), nrow = 0)), c(colnames(FB), paste0("mateband",f), 
                                                                                      "maband", paste0("n", f)))
    FB <- FB %>% mutate_all(as.character)
  }
  
  FC <- bind_rows(FA, FB) 
  if(nrow(FC != 0)){
    for(i in 1:nrow(FC)){
      if(!is.na(FC$feband[i])){FC$BAND[i] <- FC$feband[i]}
      if(!is.na(FC$maband[i])){FC$BAND[i] <- FC$maband[i]}
    }
  }
  FC <- FC[, !names(FC) %in% c("feband", "maband")]
  
  FD <- FC
  FD$COUNT <- 1
  
  FE <- group_by(FD, BAND) %>% mutate(COUNT = sum(COUNT)) 
  CheckReplicates <- Mistakes(x = FE, groupby = "BAND", yeardf = BA,CheckReplicates)
  #This will be off for years where there are duplicates, since I'm not condensing anything we will have more
  
  FF <- FE[FE$COUNT == 1, c("BAND", "COUNT")]
  FF$DEL <- "N"
  
  FG <- FE[FE$COUNT > 1, c("BAND", "COUNT")] 
  #This is going to be off since I didn't condense it down, it shouldn't make a difference in deleting since it's still the same names
  if(nrow(FG) != 0){FG$DEL <- "Y"}else{
    FG <- setNames(data.frame(matrix(ncol = (length(colnames(FG)) + 1), nrow = 0)), c(colnames(FG), "DEL"))
    FG <- FG %>% mutate_all(as.character)
  }
  
  #This will have a duplicates of everything again but when we delete it in FL it won't matter
  FI <- full_join(FG, FC) 
  FI <- FI[which(FI$DEL == "Y"),]
  if(nrow(FI) != 0){
    FI$dud <- 1
  }else{
    FI <- setNames(data.frame(matrix(ncol = (length(colnames(FI)) + 1), nrow = 0)), c(colnames(FI), "dud"))
  }
  
  FJ <- FI %>% group_by(BAND) %>% mutate(COUNT = sum(COUNT))
  CheckReplicates <- Mistakes(x = FJ, groupby = "BAND", yeardf = BA,CheckReplicates) 
  FJ <- group_by(FI, BAND) %>% summarise(dud = sum(COUNT))
  
  FK <- FJ[,"BAND"]
  if(nrow(FK) != 0){
    FK$DEL <- "Y"
    FK[[paste0("n",f)]] <- "TUT"
  }else{
    FK <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "DEL", paste0("n",f)))
    FK <- FK %>% mutate_all(as.character)
  }
  
  FL <- full_join(FC, FK)
  FL <- FL[(FL$DEL != "Y" | is.na(FL$DEL)),] #Deletes columns with a Y in $DEL basically by pulling out everything that doesn't have a "Y" in the DEL col.
  
  FM <- full_join(FL, FK)
  FM[[paste0("CS",f)]] <- FM$CS; FM[[paste0("CSC",f)]] <- FM$CSC; FM[[paste0("E",f)]] <- FM$E; FM[[paste0("F",f)]] <- FM$F; 
  FM[[paste0("LO",f)]] <- FM$LO; FM[[paste0("ID",f)]] <- FM$ID; FM[[paste0("HD",f)]] <- FM$HD; FM[[paste0("HO",f)]] <- FM$HO; 
  FM[[paste0("GLN",f)]] <- FM$GLN; FM[[paste0("ABN",f)]] <- FM$M; FM[[paste0("ABD",f)]] <- FM$ABDT;
  FM[[paste0("NL",f)]] <- FM$LOC; FM[[paste0("O",f)]] <- FM$O
  keep <- c("NEST", 'BAND', paste0('n',f), paste0('mateband',f), paste0("CS",f), paste0("CSC",f), paste0("E",f), paste0("F",f), paste0("ID",f), 
            paste0("LO",f), paste0("HD",f), paste0("HO",f), paste0("GLN",f), paste0("ABN",f), paste0("ABD",f), paste0("NL",f), 
            paste0("O",f))
  FM <- FM[, keep]
  
  #Manipulation years!
  #***Check on year where BAND or MATEBAND_YR is empty, is it NA or space??
  if(f %in% manip_yrs){
    MNA <- FM
    
    MNB <- get(paste0("MNEST",f))
    if(any(is.na(MNB$NEST) | MNB$NEST == "")){
      MNB <- MNB[-which(MNB$NEST == "" | is.na(MNB$NEST)),] #Check operators here
    }
    
    for(i in 1:nrow(MNB)){
      if(MNB[[paste0("CM",f)]][i] != 0 & !is.na(MNB[[paste0("CM",f)]][i])){MNB[[paste0("CMP",f)]][i] <- 1}else{
        MNB[[paste0("CMP",f)]][i] <- 0}
      if(MNB[[paste0("BM",f)]][i] != 0 & !is.na(MNB[[paste0("BM",f)]][i])){MNB[[paste0("BMP",f)]][i] <- 1}else{
        MNB[[paste0("BMP",f)]][i] <- 0}
    }
    
    MNC <- full_join(MNA, MNB)
    MNC <- MNC %>% mutate_at(c("E", paste0("CM",f), paste0("MCS",f), paste0("BM",f), "J", "K"), as.integer)
    for(i in 1:nrow(MNC)){
      if(MNC[[paste0("OCS",f)]][i] != ""){MNC[[paste0("CS",f,"A")]][i] <- MNC[[paste0("OCS",f)]][i]}else{
        MNC[[paste0("CS",f,"A")]][i] <- MNC[[paste0("CS",f)]][i] }
      if(MNC[[paste0("GLN",f)]][i] != ""){MNC[[paste0("GLN",f,"B")]][i] <- MNC[[paste0("GLN",f)]][i]}else{
        MNC[[paste0("GLN",f,"B")]][i] <- MNC[[paste0("GLN",f, "A")]][i] }
    }
    MNC[[paste0("ECM",f)]] <- MNC[[paste0("CM",f)]] - MNC$E
    MNC[[paste0("EMCS",f)]] <- MNC[[paste0("MCS",f)]] - MNC$E
    MNC[[paste0("EBM",f)]] <- MNC[[paste0("BM",f)]] - (MNC$J + MNC$K)
    keep <- c("BAND", paste0("N", f), paste0("MATEBAND", f), paste0("CS", f), paste0("CSC", f), paste0("LO", f),
              paste0("ID", f), paste0("HD", f), paste0("HO", f), paste0("ECM", f), paste0("EMCS", f), 
              paste0("EMB", f),paste0("E", f), paste0("F", f), paste0("DG", f), paste0("GLN", f), paste0("CMP", f), 
              paste0("BMP", f), paste0("ABN", f), paste0("ABD", f), paste0("NL", f), paste0("O", f))
    MNC <- MNC[,c("E", keep)]
    
    MND <- MNC 
    MND[[paste0("CS",f)]] <- MND[[paste0("CS",f, "A")]]
    MND[[paste0("GLN",f)]] <- MND[[paste0("GLN",f, "B")]]
    MND <- MND[which(!is.na(MND$BAND) & !is.na(MND[[paste0("MATEBAND", f)]]))]
    MND <- MND[, keep]
  }

  if(f %in% manip_yrs){FO <- MND %>% rename(REALBAND = BAND)}else{
  FO <- FM %>% rename(REALBAND = BAND)}
  FO$BAND <- FO[[paste0("mateband",f)]]
  FO <- FO[-which(FO$BAND == "UM" | FO$BAND == "" | is.na(FO$BAND)),]
  if(nrow(FO) != 0){FO$DUM <- "Y"}else{
    FO <- setNames(data.frame(matrix(ncol = (length(colnames(FO)) + 1), nrow = 0)), c(colnames(FO), "DUM"))
    FO <- FO %>% mutate_all(as.character)
  }
  
  DU <- DU %>% mutate_all(as.character)
  FP <- full_join(DU, FO) #Not inner join lmao
  FP <- FP[which(FP$DUM == "Y"),] 
  
  if(nrow(FP) != 0){
    FP[[paste0("MATEM",f)]]<- FP$METAL
    FP[[paste0("MATEP",f)]] <- FP[[paste0("mateband",f)]]
    FP[[paste0("NMSEX",f)]] <- FP$SEXB
    
    FP <- FP[,c(paste0("MATEM",f), 'NEST', paste0("MATEP",f), paste0("n",f), paste0("CS",f), paste0("ID",f), 
                paste0("HD",f), 'REALBAND', paste0("NMSEX",f))]
  }else{
    FP <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c(paste0("MATEM",f), 'NEST', paste0("MATEP",f), paste0("n",f), 
                                                             paste0("CS",f), paste0("ID",f), paste0("HD",f), 'REALBAND'))
    FP <- FP %>% mutate_all(as.character)
  }

  FQ <- FP[,c("REALBAND", paste0("MATEM",f), paste0("MATEP",f), if(f != "86"){paste0("NMSEX",f)})]
  FQ <- FQ %>% rename(BAND = REALBAND)
  
  FR <- full_join(FQ, FM)
  FR[[paste0("MATEP",f)]][which(FR[[paste0("mateband",f)]] == "UM")] <- FR[[paste0("mateband",f)]][which(FR[[paste0("mateband",f)]] == "UM")] #lol this looks awful i'm so sorry
  FR <- FR[, !names(FR) %in% paste0("mateband",f)]
  
  FS <- full_join(DU, FR)
  FS <- FS[which(!is.na(FS$METAL)),]
  FS <- FS[, !names(FS) %in% "NEST"]
  
  FT <- full_join(DU, FR) 
  FT <- FT[which(is.na(FT$METAL)), "BAND"] #Changing "." to NA
  if(nrow(FT) != 0){
    FT$YEAR <- f
    FT$COMMENTS <- "Bird seen at nest, no banding record"
  }else{
    FT <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "YEAR", "COMMENTS"))
  }
  
  ###
  #It's H time!!! 
  ###
  
  HA <- BS[(BS$DRIVE != "NEST" | is.na(BS$DRIVE)),]
  HA$AGEB <- NA; HA$SEXB <- NA; HA$DATEB <- NA; HA$YEARB <- NA
  for(i in 1:nrow(HA)){
    if(HA$FILE[i] == "BS"){
      HA$AGEB[i] <- HA$AGE[i]; HA$SEXB[i] <- HA$SEX[i] 
      HA$DATEB[i] <- HA$DATE[i]; HA$YEARB[i] <- HA$YEAR[i]
    }
  }
  HA <- HA %>% rename(!!paste0("aB",f) := AGE, !!paste0("sB",f) := SEX, !!paste0("Bc",f) := CUL, !!paste0("Bt",f) := TAR, 
                      !!paste0("Bm",f) :=MASS, !!paste0("BP",f) :=BP)
  HA$COUNT <- 1
  HA[[paste0("BD",f)]][which(HA$COLONY == "TUT")] <- HA$DRIVE[which(HA$COLONY == "TUT")]
  HA[[paste0("BD",f)]][which(HA$COLONY != "TUT" | is.na(HA$COLONY))] <- HA$COLONY[which(HA$COLONY != "TUT"| is.na(HA$COLONY))]
  HA <- HA[, !names(HA) %in% c("BSTAT", "FILE", "DRIVE", "COLONY", "YEAR")]
  
  HB <- HA[(HA$METAL == "" | is.na(HA$METAL)), paste0("BAND",f), drop = FALSE] #Changed from "." to "" 
  HB <- HB %>% rename(BAND = !!paste0("BAND",f))
  if(nrow(HB) != 0){
    HB$COMMENTS <- "Bird was captured with plastic and released without metal"
    HB$YEAR <- f
  }else{
    HB <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "COMMENTS", "YEAR"))
    HB <- HB %>% mutate_all(as.character())
  }
  
  HC <- HA[(HA$METAL != "" & !is.na(HA$METAL)),] 
  HC[[paste0("Bc",f)]] <- as.numeric(HC[[paste0("Bc",f)]])
  HC[[paste0("Bt",f)]] <- as.numeric(HC[[paste0("Bt",f)]])
  
  #I don't think we should do one here there's a lot of birds that I think aren't mistakes?? 
  #And it'd make CheckReplicates worthless with how many things it flags
  #HD <- HC %>% group_by(METAL) %>% mutate(COUNT = sum(COUNT))
  #CheckReplicates <- Mistakes(HD, groupby = "METAL", yeardf = BA, CheckReplicates)
  HD <- group_by(HC, METAL) %>% summarise(!!paste0("meanbc",f) := mean_(HC, !!as.name(paste0("Bc",f))), 
                                            !!paste0("meanbt",f) := mean_(HC, !!as.name(paste0("Bt",f))),
                                              DATE = min(DATE), COUNTsum = sum(COUNT))
  
  HE <- full_join(HA, HD)
  HE <- HE[(!is.na(HE$COUNTsum)), !names(HE) %in% c(paste0("Bc",f), paste0("Bt",f))]
  
  #Again like w/ HD this includes a lot of duplicates that seem ok? Like they should be summarised instead?
  #i'm gonna do that?
  HF <- HE[,!names(HE) %in% "COUNTsum"]
  HF <- HF %>% group_by(METAL) %>% mutate(COUNT = sum(COUNT)) 
  CheckReplicates <- Mistakes(HF, groupby = "METAL", yeardf = BA, CheckReplicates)
  #HF <- HF[!(HF$COUNT > 1),]
  HF <- HF %>% group_by(METAL) %>% mutate(COUNT = sum(COUNT)) %>%
                                   summarise_all(~last(.[which(!is.na(.) & (. != "") )]))
  
  HG <- HF %>% rename(!!paste0("BC",f) := !!paste0("meanbc",f), !!paste0("BT",f) := !!paste0("meanbt",f))
  
  HH <- HG[(!is.na(HG$NEWMETAL) ), !names(HG) %in% "METAL"] 
  if(nrow(HH) != 0){HH$DEL <- "Y"}else{
    HH <- setNames(data.frame(matrix(ncol = (length(colnames(HH)) + 1), nrow = 0)), c(colnames(HH), "DEL"))}
  
  HI <- HH[,c(paste0("mr",f),"DEL")] %>% rename(METAL = !!paste0("mr",f))
  HJ <- HH[,c("NEWMETAL", "DEL")] %>% rename(METAL = NEWMETAL)
  
  HK <- bind_rows(HI, HJ) %>% mutate_if(is.logical, as.character)
  
  HL <- full_join(FS, HK)
  if("Y" %in% HL$DEL){
    HL <- HL[-which(HL$DEL == "Y"),]
  }
  HL$duma <- "1"
  
  HM <- full_join(HG, HK) #Might be missing things compared to SAS since I took out duplicates in HG - HF?
  if("Y" %in% HM$DEL){
    HM <- HM[-which(HM$DEL == "Y"),]
  }
  HM$dumb <- "1"
  HM <- HM %>% rename(!!paste0("dumpr",f) := !!paste0("PR",f), !!paste0("webtag",f) := WEBTAG, !!paste0("dbd",f) := DATE, 
                          !!paste0("ageb",f) := AGEB, !!paste0("sexb",f) := SEXB, 
                          !!paste0("dateb",f) := DATEB, !!paste0("yearb",f) := YEARB)
  
  HN <- full_join(HL, HM, by = c("METAL")) 
   #I couldn't figure out a better way to seperate this out, tried inside a mutate with if but it seems like case_when 
   #and ifelse are the only real options which isn't what I want :(
  if(f == "86"){
     HN <- HN %>% mutate(!!paste0("webtag",f) := coalesce(!!as.name(paste0("webtag",f, ".x")), !!as.name(paste0("webtag",f, ".y"))), #Different from '86
                         !!paste0("BAND",f) := coalesce(!!as.name(paste0("BAND",f, ".x")), !!as.name(paste0("BAND",f, ".y"))), #Different from '86 
                        ) %>% 
                  select( -!!paste0("webtag",f, ".x"), -!!paste0("webtag",f, ".y"), -!!paste0("BAND",f, ".x"), -!!paste0("BAND",f, ".y"))
     
  } 
   #This could be put on the same line as the full_join but I like it visually down here better 
  HN <- HN %>% mutate(!!paste0("mr",f) := coalesce(!!as.name(paste0("mr",f,".x")), !!as.name(paste0("mr",f,".y"))),
                      !!paste0("dbd",f) := coalesce(!!as.name(paste0("dbd",f, ".x")), !!as.name(paste0("dbd",f, ".y"))),
                      DEL = coalesce(DEL.x, DEL.y),
                     ) %>%
               select(-!!paste0("mr",f,".x"), -!!paste0("mr",f,".y"), -!!paste0("dbd",f, ".x"), -!!paste0("dbd",f, ".y"),
                       -DEL.x, -DEL.y)
  
  
  HO <- HN[(is.na(HN$duma) & HN$dumb == "1"),]
  HP <- HH 
  HP$METAL <- HP[[paste0("mr",f)]]
  HP <- HP %>% mutate_if(is.logical, as.character)
  
  HQ <- full_join(FS, HP, by = "METAL") %>% #***I think a right join here would be quicker to get the same outcome, but idk if that could cause issues later??
    mutate(!!paste0("mr",f) := coalesce(!!as.name(paste0("mr",f,".x")), !!as.name(paste0("mr",f,".y"))),
           WEBTAG = coalesce(WEBTAG.x, WEBTAG.y),
           !!paste0("PR",f) := coalesce(!!as.name(paste0("PR",f, ".x")), !!as.name(paste0("PR",f, ".y"))),
           AGEB = coalesce(AGEB.x, AGEB.y),
           SEXB = coalesce(SEXB.x, SEXB.y),
           DATEB = coalesce(DATEB.x, DATEB.y),
           YEARB = coalesce(YEARB.x, YEARB.y)
    ) %>%
    select(-!!paste0("mr",f,".x"), -!!paste0("mr",f,".y"), -WEBTAG.x, -WEBTAG.y, -!!paste0("PR",f, ".x"), -!!paste0("PR",f, ".y"), -AGEB.x, -AGEB.y,
           -SEXB.x, -SEXB.y, -DATEB.x, -DATEB.y, -YEARB.x, -YEARB.y)
  
  if(f == "86"){
    HQ <- HQ %>% mutate(!!paste0("BAND",f) := coalesce(!!as.name(paste0("BAND",f, ".x")), !!as.name(paste0("BAND",f, ".y")))) %>% 
                 select(-!!paste0("BAND",f, ".x"), -!!paste0("BAND",f, ".y"))
  }
  
  HQ <- HQ[which(HQ$DEL == "Y"), !names(HQ) %in% "METAL"]
  
  HR <- HQ %>% rename(!!paste0("dumpr",f) := !!paste0("PR",f), !!paste0("yearb",f) := YEARB, !!paste0("dateb",f) := DATEB,
                          !!paste0("ageb",f) := AGEB, !!paste0("sexb",f) := SEXB)
  HR[[paste0("webtag",f)]] <- HR$WEBTAG
  HR[[paste0("dbd",f)]] <- HR$DATE
  HR$METAL <- HR$NEWMETAL
  HR <- HR[, !names(HR) %in% c("WEBTAG", "DEL", "DATE")]
  
  HS <- bind_rows(HN, HR)
  Hcols <- c(paste0("RP",f), 'AGE', 'SEX', 'DATE', 'YEAR', 'BANDB', 'WEBTAGB', 'COMMENTS')
  HS[Hcols] <- NA
 
  HS$BAND[which(HS$BAND == "")] <- NA
  #Takes a Hot Second: Do something different?
  for(i in 1:nrow(HS)){
    #RP
    if(!is.na(HS[[paste0("dumpr",f)]][i]) & !is.na(HS[[paste0("PR",f)]][i])){HS[[paste0("RP",f)]][i] <- HS[[paste0("PR",f)]][i]}else{
      HS[[paste0("RP",f)]][i] <- HS[[paste0("dumpr",f)]][i]
    }
    
    #Age
    if(!is.na(HS[[paste0("ageb",f)]][i]) & !is.na(HS$AGEB[i]) & HS[[paste0("ageb",f)]][i] == HS$AGEB[i]){HS$AGE[i] <- HS$AGEB[i]}
    if(!is.na(HS[[paste0("ageb",f)]][i]) & !is.na(HS$AGEB[i]) & HS[[paste0("ageb",f)]][i] != HS$AGEB[i]){
      HS$AGE[i] <- HS$AGEB[i]
      HS$COMMENTS[i] <- "Age at banding does not agree with BBL and BSC"
    }
    if(!is.na(HS[[paste0("ageb",f)]][i]) & is.na(HS$AGEB[i]) ){HS$AGE[i] <- HS[[paste0("ageb",f)]][i]}
    if(is.na(HS[[paste0("ageb",f)]][i]) & !is.na(HS$AGEB[i]) ){HS$AGE[i] <- HS$AGEB[i]}
    
    #Sex
    if(!is.na(HS[[paste0("sexb",f)]][i]) & !is.na(HS$SEXB[i]) & HS[[paste0("sexb",f)]][i] == HS$SEXB[i]){HS$SEX[i] <- HS$SEXB[i]}
    if(!is.na(HS[[paste0("sexb",f)]][i]) & !is.na(HS$SEXB[i]) & HS[[paste0("sexb",f)]][i] != HS$SEXB[i]){
      HS$SEX[i] <- HS$SEXB[i]
      HS$COMMENTS[i] <- "Sex at banding does not agree with BBL and BSC"
    }
    if(!is.na(HS[[paste0("sexb",f)]][i]) & is.na(HS$SEXB[i]) ){HS$SEX[i] <- HS[[paste0("sexb",f)]][i]}
    if(is.na(HS[[paste0("sexb",f)]][i]) & !is.na(HS$SEXB[i]) ){HS$SEX[i] <- HS$SEXB[i]}
    
    #DATE
    if(!is.na(HS[[paste0("dateb",f)]][i]) & !is.na(HS$DATEB[i]) & HS[[paste0("dateb",f)]][i] == HS$DATEB[i]){HS$DATE[i] <- HS$DATEB[i]}
    if(!is.na(HS[[paste0("dateb",f)]][i]) & !is.na(HS$DATEB[i]) & HS[[paste0("dateb",f)]][i] != HS[[paste0("dateb",f)]][i]){
      HS$DATE[i] <- HS$DATEB[i]
      HS$COMMENTS[i] <- "Date at banding does not agree with BBL and BSC"
    }
    if(!is.na(HS[[paste0("dateb",f)]][i]) & is.na(HS$DATEB[i]) ){HS$DATE[i] <- HS[[paste0("dateb",f)]][i]}
    if(is.na(HS[[paste0("dateb",f)]][i]) & !is.na(HS$DATEB[i]) ){HS$DATE[i] <- HS$DATEB[i]}
    
    #Year
    if(!is.na(HS[[paste0("yearb",f)]][i]) & !is.na(HS$YEARB[i]) & HS[[paste0("yearb",f)]][i] == HS$YEARB[i]){HS$YEAR[i] <- HS$YEARB[i]}
    if(!is.na(HS[[paste0("yearb",f)]][i]) & !is.na(HS$YEARB[i]) & HS[[paste0("yearb",f)]][i] != HS$YEARB[i]){
      HS$YEAR[i] <- HS$YEARB[i]
      HS$COMMENTS[i] <- "Year at banding does not agree with BBL and BSC"
    }
    if(!is.na(HS[[paste0("yearb",f)]][i]) & is.na(HS$YEARB[i]) ){HS$YEAR[i] <- HS[[paste0("yearb",f)]][i]}
    if(is.na(HS[[paste0("yearb",f)]][i]) & !is.na(HS$YEARB[i]) ){HS$YEAR[i] <- HS$YEARB[i]}
    
    #Band
    if(!is.na(HS[[paste0("BAND",f)]][i]) & !is.na(HS$BAND[i]) & HS[[paste0("BAND",f)]][i] == HS$BAND[i]){HS$BANDB[i] <- HS$BAND[i]}
    if(!is.na(HS[[paste0("BAND",f)]][i]) & !is.na(HS$BAND[i]) & HS[[paste0("BAND",f)]][i] != HS$BAND[i] & !is.na(HS[[paste0("RP",f)]][i])){
      HS$BANDB[i] <- HS[[paste0("BAND",f)]][i]
    }
    if(!is.na(HS[[paste0("BAND",f)]][i]) & !is.na(HS$BAND[i]) & HS[[paste0("BAND",f)]][i] != HS$BAND[i] & is.na(HS[[paste0("RP",f)]][i])){
      HS$BANDB[i] <- HS$BAND[i]
      HS$COMMENTS[i] <- "Plastic bands do not agree for this recapture with original BSC"
    }
    if(!is.na(HS[[paste0("BAND",f)]][i]) & is.na(HS$BAND[i]) ){HS$BANDB[i] <- HS[[paste0("BAND",f)]][i]}
    if(is.na(HS[[paste0("BAND",f)]][i]) & !is.na(HS$BAND[i]) ){HS$BANDB[i] <- HS$BAND[i]}
    
    #Webtag
    if(!is.na(HS[[paste0("webtag",f)]][i]) & !is.na(HS$WEBTAG[i]) & HS[[paste0("webtag",f)]][i] == HS$WEBTAG[i]){HS$WEBTAGB[i] <- HS$WEBTAG[i]}
    if(!is.na(HS[[paste0("webtag",f)]][i]) & !is.na(HS$WEBTAG[i]) & HS[[paste0("webtag",f)]][i] != HS$WEBTAG[i]){
      HS$WEBTAGB[i] <- HS$WEBTAG[i]
      HS$COMMENTS[i] <- "Webtag code has changed"
    }
    if(!is.na(HS[[paste0("webtag",f)]][i]) & is.na(HS$WEBTAG[i]) ){HS$WEBTAGB[i] <- HS[[paste0("webtag",f)]][i]}
    if(is.na(HS[[paste0("webtag",f)]][i]) & !is.na(HS$WEBTAG[i]) ){HS$WEBTAGB[i] <- HS$WEBTAG[i]}
    
    if(!is.na(HS[[paste0("PR",f)]][i]) & is.na(HS[[paste0("RP",f)]][i])){HS[[paste0("RP",f)]][i] <- HS[[paste0("PR",f)]][i]}
  }
  
  HS <- HS[,!names(HS) %in% c("NEWMETAL", "duma", "dumb", "DEL", "COUNT", "newmetal", paste0("dumpr",f),
                                    paste0("PR",f), paste0("ageb",f), "AGEB", paste0("sexb",f), "SEXB", paste0("dateb",f), 
                                    "DATEB", paste0("yearb",f), "YEARB", paste0("BAND",f), "BAND", paste0("webtag",f), "WEBTAG" )]
  HS <- HS %>% mutate_if(is.logical, as.character)
  
  HT <- HS %>% rename(AGEB = AGE, SEXB = SEX, DATEB = DATE, YEARB = YEAR, BAND = BANDB, WEBTAG = WEBTAGB,
                          !!paste0("PR",f) := !!paste0("RP",f))
  HU <- HT[,!names(HT) %in% "COMMENTS"]
  HV <- HS[(!is.na(HT$COMMENTS)), c('COMMENTS', "METAL")] #, 'BAND', 'WEBTAG'
  
  ###
  #The J's!!!! Egg Data!!!
  ###
  
  JA <- get(paste0("EGG",f))[( get(paste0("EGG",f))[["TAG"]] != "" ),] #I think "" is right here, so far I have seen "" but no NA's so this seems good for now
  keep <- c("COUNT", "NEST", "EGG", 'TAGD', "STATE", "DATE", "TAG")
  if(nrow(JA) != 0){
    JA$EGG <- JA$EGGB 
    JA$TAGD <- JA$DATE
    JA$COUNT <- "1"
    JA <- JA[,keep]
  }else{
    JA <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), keep)
    JA <- JA %>% mutate_all(as.character)
  }
  
  JB <- get(paste0("EGG",f))[(get(paste0("EGG",f))[["LENGTH"]] != "" | get(paste0("EGG",f))[["WIDTH"]] != ""),]
  keep <- c("NEST", "EGG", "LENGTH", "WIDTH")
  if(nrow(JA) != 0){
    JB$EGG <- JB$EGGA
    JB <- JB[,keep]
  }else{
    JB <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), keep)
    JB <- JB %>% mutate_all(as.character)
  }
  
  ###
  #***Two extra here for some reason
  JC <- full_join(JA, JB)
  JC$WEBTAG <- JC$TAG
  JC <- JC[which(JC$COUNT == 1),]
  JC$D <- JC$TAGD
  JC$L <- JC$LENGTH
  JC$W <- JC$WIDTH
  JC <- JC[,!names(JC) %in% c("TAGD", "LENGTH", "WIDTH", "TAG")]
  
  JC <- JC %>% mutate_at(vars(D, L, W, COUNT), as.numeric)
  
  #*** Not sure if my function can handle multiple inputs for grouby??
  #This one is weird so I'm ignoring it for now
  #JD <- JC %>% group_by(WEBTAG, NEST, EGG) %>% mutate(COUNT = sum(COUNT))
  #CheckReplicates <- Mistakes(JD, groupby = "WEBTAG", yeardf = BA, CheckReplicates)
  JD <- JC %>% group_by(WEBTAG, NEST, EGG) %>% summarise(MD = mean_(JC, D), ML = mean_(JC, L), 
                                                             MW = mean_(JC,W), C = sum(COUNT))
  
  JE <- JD
  JE <- JE %>% rename(WTD = MD, EGG1 = ML, EGGW = MW)
  if(nrow(JE) != 0){
    JE$COUNT <- 1
  }else{
    JE <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c(colnames(JE), "COUNT"))
    JE <- JE %>% mutate_if(is.logical, as.character)
    JE$COUNT <- as.numeric(JE$COUNT)
  }
  JE <- JE[, !names(JE) %in% "C"]
  
  JF <- group_by(JE, WEBTAG) %>% mutate(COUNT = sum(COUNT))
  CheckReplicates <- Mistakes(x = JF, groupby = "WEBTAG", yeardf = BA, CheckReplicates)
  JF <- JE %>% group_by(WEBTAG) %>% summarise(COUNT = sum(COUNT))
  
  JG <- JF[(JF$COUNT > 1), "WEBTAG"]
  if(nrow(JG) != 0){
    JG$DEL <- "Y"
  }else{
    JG <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("WEBTAG", "DEL"))
    JG <- JG %>% mutate_all(as.character)
  }
  
  JH <- JA %>% rename(WEBTAG = TAG)
  JI <- full_join(JG, JH)
  JI <- JI[-which(JI$DEL == "Y"), !names(JI) %in% "DEL"] 
  
  JJ <- full_join(JH, JG)
  JJ <- JJ[which(JJ$DEL == "Y"),c("WEBTAG", "NEST")]
  if(nrow(JJ) != 0){
    JJ$YEAR <- f
    JJ$COMMENTS <- "This webtag put on 2 different eggs"
  }else{
    JJ <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), keep)
    JJ <- JJ %>% mutate_all(as.character)
  }
  
  JK <- JI
  if(nrow(JK) != 0){
    JK$DUMA <- "Y"
  }else{
    JK <- setNames(data.frame(matrix(ncol = (length(colnames(JK)) + 1), nrow = 0)), c(colnames(JK), "DUMA"))
    JK <- JK %>% mutate_all(as.character)
  }
  
  JK$COUNT <- as.numeric(JK$COUNT)
  JL <- full_join(JK, JE)
  JL <- JL[which(JL$DUMA == "Y"), !names(JL) %in% c("DUMA", "COUNT", "WTD")] #Again assuming I can do this
  
  ###
  #SAS COMMENTS: Starts dealing with parent info for this years webtags;
  #               but identifies webtags from prior years caught this year for the first time.
  ###
  
  LA <- FM
  LA$dum <- 1
  
  LB <- LA %>% group_by(NEST) %>% mutate(COUNT = sum(dum))
  CheckReplicates <- Mistakes(x = LB, groupby = "NEST", yeardf = BA, CheckReplicates)
  LB <- group_by(LA, NEST) %>% summarise(dum = sum(dum))
  
  LC <- get(paste0("NEST", f))
  
  LD <- full_join(LC, LB)
  LD <- LD[which(LD$dum > 0 & LD$NEST != ""),]
  LD$PARENT1P <- as.character(NA)
  LD$PARENT2P <- as.character(NA)
  for(i in 1:nrow(LD)){
    #Some cleaning up could happen here: what happens if there's a band but no color, should it be AO2_ or just AO2?
    
    #Runs as long as one of either band or color isn't NA. Will only NOT run if both are NA
    if(!is.na(LD$BAND[i]) | !is.na(LD$C1[i])){
      color <- str_replace_na(LD$C1[i], "")
      band <- str_replace_na(LD$BAND[i], "")
      LD$PARENT1P[i] <- paste0(band, color)
    }
    if(!is.na(LD$MATE[i]) | !is.na(LD$C2[i])){
      color2 <- str_replace_na(LD$C2[i], "")
      mate <- str_replace_na(LD$MATE[i], "")
      LD$PARENT2P[i] <- paste0(mate, color2)
    }
  }
  LD <- LD[,c("NEST", "PARENT1P", "PARENT2P")]
  
  LE <- LD[(!is.na(LD$PARENT1P)),] 
  LE$BAND <- LE$PARENT1P
  LE <- LE[,c("NEST", "BAND")]
  
  LF <- LD[(!is.na(LD$PARENT2P)),]
  LF$BAND <- LF$PARENT2P
  LF <- LF[,c("NEST", "BAND")]
  
  #***Missing 4 in 89
  LG <- DU[(!is.na(DU$BAND) & DU$BAND != ""), c("BAND", "METAL")] %>% rename(PARENTM = METAL)
  
  LH <- full_join(LG, LE)
  LH <- LH[(!is.na(LH$NEST)),]  
  LH$PARENTM[which(LH$BAND == "UM")] <- "UM"
  LH$PARENTM[which(LH$PARENTM == "")] <- "banded"
  LH <- LH %>% rename(PARENT1M = PARENTM, PARENT1P = BAND)
  
  LI <- full_join(LG, LF)
  LI <- LI[(!is.na(LI$NEST)),] 
  LI$PARENTM[which(LI$BAND == "UM")] <- "UM"
  LI$PARENTM[which(LI$PARENTM == "")] <- "banded"
  LI <- LI %>% rename(PARENT2M = PARENTM, PARENT2P = BAND)
  
  LJ <- full_join(LH, LI)
  LK <- full_join(JL, LJ)
  LK <- LK[-which(is.na(LK$WEBTAG)),]
  if(nrow(LK) !=0){LK$KEEP <- "Y"}else{
    LK <- setNames(data.frame(matrix(ncol = (length(colnames(LK)) +1), nrow = 0)), c(colnames(LK), "KEEP"))
    LK <- LK %>% mutate_all(as.character)
  }
  keep <- c("EGG1", "EGGW", "WEBTAG", "NEST", "STATE", "EGG", "TAGD", "PARENT1M", "PARENT1P", 'PARENT2M', 'PARENT2P', 'KEEP')
  LK <- LK[,keep]
  
  LL <- LK
  if(nrow(LL) !=0){LL$YEAR <- f}else{
    LL <- setNames(data.frame(matrix(ncol = (length(colnames(LL)) +1), nrow = 0)), c(colnames(LL), "YEAR"))
    LL <- LL %>% mutate_all(as.character)
  }
  
  if(f == "86"){LM <- LL}else{
    LM <- bind_rows(LL, AC)
  }
 
  
  NA_ <- HG[,c("METAL", "WEBTAG", "AGEB", "YEARB")] #need a name convention change here b/c it'd just be NA 
  
  NB <- full_join(NA_, LK)
  NB <- NB[which(NB$WEBTAG != "" & NB$METAL != "" & !is.na(NB$WEBTAG), !is.na(NB$METAL)),] 
  if(nrow(NB) != 0){
    NB$YEAR <- (as.numeric(NB$YEARB) - 1900) #***Need to think about this one here for years past '99
    NB <- NB[which(NB$YEAR == f), !names(NB) %in% "YEAR"]
  }else{
    NB <- setNames(data.frame(matrix(nrow = 0, ncol = length(colnames(NB)) )), colnames(NB))
    NB <- NB %>% mutate_all(as.character) 
  }
  
  NC <- NB[which(NB$KEEP == "Y"),]
  ND <- NB[which(NB$AGEB == "L" & is.na(NB$KEEP)), c("METAL", "WEBTAG", "AGEB", "YEARB")]
  
  NE <- NB[which(NB$AGEB == "SY"), c("METAL", "WEBTAG", "AGEB", "YEARB")]
  if(nrow(NE) != 0){
    NE$DUMA <- "Y"
  }else{
    NE <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("METAL", "WEBTAG", "AGEB", "YEARB", "DUMA"))
    NE <- NE %>% mutate_all(as.character)
  }
  
  NF <- NB[(NB$AGEB == "ASY"), c("METAL", "WEBTAG", "AGEB", "YEARB")]
  if(nrow(NF) != 0){
    NF$DUMA <- "Y"
  }else{
    NF <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("METAL", "WEBTAG", "AGEB", "YEARB", "DUMB"))
    NF <- NF %>% mutate_all(as.character)
  }
  
  #NH wrong in 89, missing 2
  if(f != "86"){
    NG <- full_join(NE, AB) #Different from 86
    NG <- NG[which(NG$DUMA == "Y"), !names(NG) %in% "DUMA"]
    
    #***DUmb doesn't exist in 89 for some reason
    NH <- full_join(NF, AC)#Diff from 86
    NH <- NH[which(NH$DUMB == "Y"), !names(NH) %in% "DUMB"]
  }

  ###
  #Sas COMMENTS: each row must have the NH column filled in
  ###
  
  if(f == "86"){NI <- bind_rows(NC, ND)}else{
    NI <- bind_rows(NC, ND, NG, NH)
  }
  
  if(nrow(NI) != 0){
    NI$NEST[which(NI$NEST == "")] <- "noinfo" 
  }
  NI <- NI %>% rename(!!paste0("NHID",f) := NEST, !!paste0("EGGL",f) := EGG1, !!paste0("EGGW",f) := EGGW, 
                      !!paste0("STATE",f) := STATE, !!paste0("PILS",f) := EGG, !!paste0("TAGD",f) := TAGD,
                      !!paste0("PARENT1M",f) := PARENT1M, !!paste0("PARENT1P",f) := PARENT1P, 
                      !!paste0("PARENT2M",f) := PARENT2M, !!paste0("PARENT2P",f) := PARENT2P)
  NI <- NI[,!names(NI) %in% c("AGEB", "YEARB", "KEEP", "YEAR")]
  
  NJ <- full_join(HU, NI)
  NJ$COMMENTS <- NA
  
  #***Questionable part, look back at later
  NJ$NHID <- NA
  NJ[c("EGGL", "EGGW", "STATE", "PILS", "TAGD", "PARENT1M", "PARENT2M")] <- NA
  for(i in 1:nrow(NJ)){
    if(!is.na(NJ$NHID[i]) & !is.na(NJ[[paste0("NHID",f)]][i])){NJ$COMMENTS[i] <- "Double use of applied webtag"}
    if(is.na(NJ$NHID[i]) &!is.na(NJ[[paste0("NHID",f)]][i])){
      NJ$EGGL[i] <- NJ[[paste0("EGGL",f)]][i]
      NJ$EGGW[i] <- NJ[[paste0("EGGW",f)]][i]
      NJ$NHID[i] <- NJ[[paste0("NHID",f)]][i]
      NJ$STATE[i] <- NJ[[paste0("STATE",f)]][i]
      NJ$PILS[i] <- NJ[[paste0("PILS",f)]][i]
      NJ$TAGD[i] <- NJ[[paste0("TAGD",f)]][i]
      NJ$PARENT1M[i] <- NJ[[paste0("PARENT1M",f)]][i]
      NJ$PARENT2M[i] <- NJ[[paste0("PARENT1P",f)]][i]
    }
  }
  #I don't like the thing below I don't know why we do it in '86 and I don't want to lol
  #NJ$TAGD <- "   " #***Why do we do this #'non 86 year doesn't do this
  
  NK <- NJ[(!is.na(NJ$COMMENTS)),]
  if(nrow(NK) != 0){
    NK$YEAR <- f
    NK <- NK[,c("METAL", "BAND", "COMMENTS", "YEAR", "WEBTAG")]
  }else{
    NK <- setNames(data.frame(matrix(nrow = 0, ncol = 5)), c("METAL", "BAND", "COMMENTS", "YEAR", "WEBTAG"))
    NK <- NK %>% mutate_all(as.character)
  }
  
  NL <- NJ[,!names(NJ) %in% c("COMMENTS", paste0("EGGL",f), paste0("EGGW",f), paste0("NHID",f), paste0("STATE",f), 
                              paste0("PILS",f), paste0("TAGD",f), paste0("PARENT1M",f), paste0("PARENT2M",f),
                              paste0("PARENT1P",f), paste0("PARENT2P",f))]
  
  if(f == "86"){
    NL <- NL[-which(!is.na(NL$BBLYEAR) & NL$BBLYEAR < 1967),] #diff from '86
    ERRORS <- bind_rows(BW, BX, DB, DV, FT, HB, HV, JJ, NK) #diff from '86
  }else{
    
    #Finds the full format of the next year
    if(f < 50){next_yr <- paste0("20", (as.numeric(f)+1) )}else{next_yr <- paste0("19",(as.numeric(f)+1) )}
    
    #if(f > "87"){
      if(any(grepl(next_yr,SPRINGMST15$YEAR))){ #Will only run if there is data in the SPRING file for the year in the future. I think it makes the above if obsolete? 
        SPRINGA <- SPRINGMST15[which(SPRINGMST15$YEAR == next_yr),] %>% 
          rename(REALBAND = BAND, !!paste0("SM", (as.numeric(f)+1)) := MONTH, !!paste0("SD", (as.numeric(f)+1)) := DAY)
        SPRINGA[[paste0("SL", (as.numeric(f)+1))]] <- "SOG"
        SPRINGA  <- SPRINGA[,c("REALBAND", paste0("SL", (as.numeric(f)+1)), paste0("SM", (as.numeric(f)+1)), paste0("SD", (as.numeric(f)+1)), "MATE")]
        
        SPRINGB <- SPRINGA %>% rename(BAND = MATE)
        SPRINGB <- SPRINGB[!(SPRINGB$BAND == "" | SPRINGB$BAND == "UM" | SPRINGB$BAND == "NO"),] #Check operators 
        
        SPRINGC <- left_join(SPRINGB, NL) 
        SPRINGC <- SPRINGC[!(SPRINGC$REALBAND == ""),] %>% rename(!!paste0("SMATEM", (as.numeric(f)+1)) := METAL, 
                                                                  !!paste0("SMATEP", (as.numeric(f)+1)) := BAND)
        SPRINGC <- SPRINGC[,c("REALBAND", paste0("SL", (as.numeric(f)+1)), paste0("SM", (as.numeric(f)+1)), paste0("SD", (as.numeric(f)+1)), 
                              paste0("SMATEM", (as.numeric(f)+1)), paste0("SMATEP", (as.numeric(f)+1)))]
        
        SPRINGD <- SPRINGC %>% rename(BAND = REALBAND)
        
        SPRINGE <- SPRINGMST15[which(SPRINGMST15$YEAR == next_yr),]
        SPRINGE <- SPRINGE[which(SPRINGE$MATE == "" | SPRINGE$MATE == "UM" | SPRINGE$MATE == "NO"),] %>% 
          rename(!!paste0("SM", (as.numeric(f)+1)) := MONTH, !!paste0("SD", (as.numeric(f)+1)) := DAY, 
                 !!paste0("SMATEP", (as.numeric(f)+1)) := MATE)
        SPRINGE[[paste0("SL", (as.numeric(f)+1))]] <- "SOG"
        SPRINGE[[paste0("SMATEM", (as.numeric(f)+1))]] <- ""
        SPRINGE <- SPRINGE[,c("BAND", paste0("SD", (as.numeric(f)+1)), paste0("SM", (as.numeric(f)+1)), paste0("SL", (as.numeric(f)+1)), 
                              paste0("SMATEP", (as.numeric(f)+1)), paste0("SMATEM", (as.numeric(f)+1)))]
        
        SPRINGF <- bind_rows(SPRINGD, SPRINGE)
      }
      
    #}
    
    WINTER <- WINTERMST16 #***This is done so OG doesn't throw an error in years where the below code won't run? I think it's fine
    if(f %in% winter_yrs){
      WINTER <- WINTERMST16[which(WINTERMST16$YEAR == next_yr),] %>% #***If doing the thing above the if statement won't break it this can just be changed to WINTER later
                  rename(!!paste0("WL", (as.numeric(f)+1)) := LOCATION, !!paste0("WM", (as.numeric(f)+1)) := MONTH,
                         !!paste0("WD", (as.numeric(f)+1)) := DAY)
      WINTER[,c(paste0("WL", (as.numeric(f)+1)), paste0("WM", (as.numeric(f)+1)), paste0("WD", (as.numeric(f)+1)), "BAND")]
    }
    
    TOWA <- TOWER[which(grepl(paste0("*", f), TOWER$YEAR)),] %>% rename(REALBAND = BAND)
    
    TOWB <- TOWA %>% rename(BAND = MATE)
    TOWB <- TOWB[-which(is.na(TOWB$BAND) | TOWB$BAND == "UM" | TOWB$BAND == "NO"),]
    
    TOWC <- full_join(TOWB, NL)
    TOWC <- TOWC[-which(is.na(TOWC$BRSIZE)),]
    TOWC <- TOWC %>% rename(!!paste0("DTO",f) := DATE, !!paste0("TMATEM",f) := METAL, !!paste0("TMATEP",f) := BAND, 
                            !!paste0("TMSEX",f) := SEXB, !!paste0("TBS",f) := BRSIZE)
    TOWC <- TOWC[,c("REALBAND", paste0("DTO",f), paste0("TMATEM",f), paste0("TMATEP",f), paste0("TMSEX",f), paste0("TBS",f))]
    
    TOWD <- TOWC[(is.na(TOWC[[paste0("TMATEM",f)]])), !names(TOWC) %in% c(paste0("TMATEM",f), paste0("TMSEX",f))] %>% 
               rename(!!paste0("PR",f) := !!paste0("TMATEP",f))
    
    TOWE <- NL[(!is.na(NL[[paste0("PR",f)]])),]
    
    TOWF <- full_join(TOWE, TOWD)
    TOWF <- TOWF %>% rename(!!paste0("TMATEM",f) := METAL, !!paste0("TMATEP",f) := BAND, !!paste0("TMSEX",f) := SEXB)
    TOWF <- TOWF[-which(is.na(TOWF$REALBAND) | is.na(TOWF[[paste0("TMATEM",f)]])), c('REALBAND', paste0("DTO",f), paste0("TBS",f), paste0("TMATEM",f), 
                                                                        paste0("TMATEP",f), paste0("TMSEX",f))]
    
    TOWG <- bind_rows(TOWC, TOWF) 
    TOWG <- TOWG %>% rename(BAND = REALBAND)
    
    #Figures out the full year
    if(f < 50){full_year <- paste0("20",f)}else{full_year <- paste0("19",f)}
    
    TOWH <- TOWER[which(TOWER$YEAR == full_year & (is.na(TOWER$MATE) | TOWER$MATE == "UM" | TOWER$MATE == "NO")), !names(TOWER) %in% "YEAR"]
    TOWH <- TOWH %>% rename(!!paste0("DTO",f) := DATE, !!paste0("TMATEP",f) := MATE, !!paste0("TBS",f) := BRSIZE)
    TOWH[[paste0("TMATEM",f)]] <- as.character(NA)
    
    
    TOWI <- full_join(TOWH, TOWG) %>% full_join(.,NL)
    TOWI <- TOWI[-which(!is.na(TOWI$METAL)), c("BAND",paste0("DTO",f), paste0("TMATEM",f),paste0("TMATEP",f), paste0("TBS",f))] %>%
              rename(!!paste0("PR",f) := BAND)
    
    TOWJ <- NL[(!is.na(NL[[paste0("PR",f)]])),] #same as tow87e?
    
    TOWK <- full_join(TOWJ, TOWI)
    #***Double check below when there's a year that fits it b/c I'm not sure the which statement is correct.
    TOWK <- TOWK[-which(is.na(TOWK$METAL) & (is.na(TOWK[[paste0("DTO",f)]]) & is.na(TOWK[[paste0("TMATEP",f)]]))), 
                 c("BAND", paste0("DTO",f), paste0("TMATEM",f), paste0("TMATEP",f), paste0("TBS",f))]
    
    TOWL <- bind_rows(TOWG, TOWH, TOWK)
    TOWL$COUNT <- 1
    
    #find duplicate bands from towL
    TOWM <- TOWL %>% group_by(BAND) %>% mutate(COUNT = sum(COUNT))
    CheckReplicates <- Mistakes(x = TOWM, groupby = "BAND", yeardf = BA, CheckReplicates)
    TOWM <- TOWM[!(TOWM$COUNT > 1), !names(TOWM) %in% "COUNT"]
    
    NBRA <- NBBAND[(NBBAND$YEAR == full_year),] %>% rename(REALBAND = BAND, !!paste0("NBC",f) := NBCOL)
    
    NBRB <- NBRA %>% rename(BAND = MATE)
    NBRB <- NBRB[-which(is.na(NBRB$BAND) | NBRB$BAND == "UM" | NBRB$BAND == "NO"),]
    
    NBRC <- full_join(NBRB, NL) 
    NBRC <- NBRC[-which(is.na(NBRC$REALBAND)), c("REALBAND", "DATE", "METAL", "BAND", "SEXB", paste0("NBC",f))] %>% 
              rename(!!paste0("NBD",f) := DATE, !!paste0("NBMATEM",f) := METAL, !!paste0("NBMATEP",f) := BAND, 
                           !!paste0("NBMSEX",f) := SEXB)
    NBRD <- NBRC[which(is.na(NBRC[[paste0("NBMATEM",f)]])), !names(NBRC) %in% c(paste0("NBMATEM",f), paste0("NBMSEX",f))] %>% 
              rename(!!paste0("PR",f) := !!paste0("NBMATEP",f))
    
    NBRE <- NL[(!is.na(NL[[paste0("PR",f)]])),]
    
    NBRF <- full_join(NBRE, NBRD)
    NBRF <- NBRF[-which(is.na(NBRF$REALBAND) | is.na(NBRF$METAL)), 
                     c("REALBAND", "METAL", paste0("NBD",f), "BAND", "SEXB", paste0("NBC",f))] %>% 
              rename(!!paste0("NBMATEM",f) := METAL, !!paste0("NBMATEP",f) := BAND, !!paste0("NBMSEX",f) := SEXB)
    
    NBRG <- bind_rows(NBRC, NBRF) %>% rename(BAND = REALBAND)
    
    NBRH <- NBBAND[which(NBBAND$YEAR == full_year & (is.na(NBBAND$MATE) | NBBAND$MATE == "UM" | 
                                                      NBBAND$MATE == "NO")), !names(NBBAND) %in% "YEAR"]
    NBRH <- NBRH %>% rename(!!paste0("NBD",f) := DATE, !!paste0("NBMATEP",f) := MATE, !!paste0("NBC",f) := NBCOL)
    NBRH[[paste0("NBMATEM",f)]] <- as.character(NA)
    
    NBRI <- full_join(NBRG, NBRH) %>% full_join(., NL)
    NBRI <- NBRI[-which(!is.na(NBRI$METAL)),c("BAND", paste0("NBC",f),paste0("NBD",f), paste0("NBMATEM",f), paste0("NBMATEP",f))] %>% 
              rename(!!paste0("PR",f) := BAND)
    NBRJ <- NL[(!is.na(NL[[paste0("PR",f)]])),] #same as something we did above in like tower or something 
    
    NBRK <- full_join(NBRJ, NBRI) 
    NBRK <- NBRK[!(is.na(NBRK$METAL) | (is.na(NBRK[[paste0("NBD",f)]]) & is.na(NBRK[[paste0("NBMATEP",f)]])) ), 
                     c("BAND", paste0("NBD",f), paste0("NBMATEM",f), paste0("NBMATEP",f), paste0("NBC",f))] #*** Check Operators here again they confuse me a lot lol
    NBRL <- bind_rows(NBRG, NBRH, NBRK)
    NBRL$COUNT <- 1
    
    #find duplicate bands from NBRL
    NBRM <- NBRL %>% group_by(BAND) %>% mutate(COUNT = sum(COUNT))
    CheckReplicates <- Mistakes(x = NBRM, groupby = "BAND", yeardf = BA, CheckReplicates)
    NBRM <- NBRM[!(NBRM$COUNT > 1), !names(NBRM) %in% "COUNT"]
    
    NN <- full_join(NL, TOWM) %>% full_join(., NBRM)
    if(f > "87"){
      NN <- full_join(NN, SPRINGF)
      if(f %in% winter_yrs){
        NN <- full_join(WINTER)
      }
    }
    NN <- NN[!(is.na(NN$METAL)),]
    
    OA <- NN[!(is.na(NN[[paste0("MATEM",f)]]) & is.na(NN[[paste0("NBMATEM",f)]]) & is.na(NN[[paste0("TMATEM",f)]]) & 
                 is.na(NN[[paste0("MATEP",f)]]) & is.na(NN[[paste0("NBMATEP",f)]]) & is.na(NN[[paste0("TMATEP",f)]])),]
    OA$COMMENTS <- as.character(NA)
    
    for(i in 1:nrow(OA)){
      if( (!is.na(OA[[paste0("MATEP",f)]][i]) & !is.na(OA[[paste0("TMATEP",f)]][i]) & (OA[[paste0("MATEP",f)]][i] != OA[[paste0("TMATEP",f)]][i])) |
          (!is.na(OA[[paste0("MATEP",f)]][i]) & !is.na(OA[[paste0("NBMATEP",f)]][i]) & (OA[[paste0("MATEP",f)]][i] != OA[[paste0("NBMATEP",f)]][i])) |
          (!is.na(OA[[paste0("TMATEP",f)]][i]) & !is.na(OA[[paste0("NBMATEP",f)]][i]) & (OA[[paste0("TMATEP",f)]][i] != OA[[paste0("NBMATEP",f)]][i]))
      ){
        OA$COMMENTS[i] <- "Mate changed within year"
      }
    }
    keep <- c("METAL", "BAND", paste0("MATEM",f), paste0("MATEP",f), paste0("NBD",f), paste0("NBMATEM",f), paste0("NBMATEP",f), 
              paste0("DTO",f), paste0("TMATEM",f), paste0("TMATEP",f), paste0("TBS",f), "COMMENTS")
    OA <- OA[!(is.na(OA$COMMENTS)), keep]
    
    OB <- NN[!(is.na(NN[[paste0("NMSEX",f)]]) & is.na(NN[[paste0("TMSEX",f)]]) & is.na(NN[[paste0("NBMSEX",f)]])),]
    OB$COMMENTS <- as.character(NA)
    OB$COMMENTS[which((OB[[paste0("NMSEX",f)]] == OB$SEXB) | (OB[[paste0("TMSEX",f)]] == OB$SEXB)| 
                    (OB[[paste0("NBMSEX",f)]] == OB$SEXB) | (is.na(OB$SEXB) & is.na(OB[[paste0("NMSEX",f)]])) | 
                     (is.na(OB$SEXB) & is.na(OB[[paste0("NBMSEX",f)]])) |
                     (is.na(OB$SEXB) & is.na(OB[[paste0("TMSEX",f)]])))] <- "Same sex pair" #hell yeah gay bird rights
    keep <- c("METAL", "BAND", "SEXB", paste0("MATEM",f), paste0("MATEP",f), paste0("NBD",f), paste0("NBMATEM",f), paste0("NBMATEP",f), 
              paste0("NBMSEX",f), paste0("DTO",f), paste0("TMATEM",f), paste0("TMATEP",f), paste0("TBS",f), paste0("TMSEX",f), "COMMENTS")
    OB <- OB[!is.na(OB$COMMENTS), keep]
    
    #Finds the full format of the previous year
    if(f < 50){prev_yr <- paste0("20", (as.numeric(f)-1) )}else{prev_yr <- paste0("19",(as.numeric(f)-1) )}
    
    OC <- NN
    OC$COMMENTS <- as.character(NA)
    OC$COMMENTS[((OC$BBLAGE == "L" & OC$BBLYEAR == prev_yr & !is.na(OC[[paste0("n",f)]]))|
                     (OC$BBLAGE == "L" & OC$BBLYEAR == prev_yr & OC[[paste0("TBS",f)]] > 0))] <- "Breeding SY bird"
    OC <- OC[!is.na(OC$COMMENTS), keep] #Columns saved are the same as keep above so not changing it
    
    OD <- full_join(NL, NBRM)
    OD$COMMENTS <- as.character(NA)
    OD$COMMENTS[is.na(OD$METAL)] <- "Bird seen NB, no banding record"
    OD <- OD[!is.na(OD$COMMENTS), c("BAND", "SEXB", "COMMENTS", paste0("NBD",f), paste0("NBMATEM",f), paste0("NBMATEP",f))]
    
    OE <- full_join(NL, TOWM)
    OE$COMMENTS <- as.character(NA)
    OE$COMMENTS[is.na(OE$METAL)] <- "Bird seen tower, no banding record"
    OE <- OE[!is.na(OE$COMMENTS),c("BAND", "SEXB", "COMMENTS", paste0("DTO",f), paste0("TMATEM",f), paste0("TMATEP",f), paste0("TBS",f))]
    
    if(f > 88 | f < 85){
      OF <- full_join(prev_NL, SPRINGF) #****Fuck it's suppose to be NL of the year before :(
      OF <- OF[which(is.na(OF$METAL)), c("BAND", paste0("SMATEM",f), paste0("SMATEP",f), paste0("SD",f), paste0("SM",f))]
      OF$COMMENTS <- "Bird seen spring, no banding record"
    }
    
    if(any(grepl(f, colnames(WINTER)))){
      OG <- full_join(prev_NL, WINTER) #****It's suppose to be NL of last year, think about that
      OG <- OG[which(is.na(OG$METAL)),]
      OG$COMMENTS <- "Bird seen winter, no banding records"
    }
    
    
    if(f < 89 & f > 84){
      ERRORS <- bind_rows(ERRORS, BW, BX, DV, FT, HB, HB, JJ, NK, OA, OB, OC, OD, OE)
    } else if(any(grepl(f, colnames(WINTER)))){
      ERRORS <- bind_rows(ERRORS, BW, BX, DV, FT, HB, HB, JJ, NK, OA, OB, OC, OD, OE, OF, OG)
    } else{
      ERRORS <- bind_rows(ERRORS, BW, BX, DV, FT, HB, HB, JJ, NK, OA, OB, OC, OD, OE, OF)
    }
    ERRORS <- unique(ERRORS)
  }
  
  if(f > "05" & f < "86"){
    PA <- LL
    PA$COUNT <- 1
    
    PB <- PA %>% group_by(PARENT1M) %>% summarise(COUNT = sum(COUNT)) #mutate?
    #CheckReplicates?
    
    PC <- PA %>% group_by(PARENT2M) %>% summarise(COUNT = sum(COUNT)) #mutate?
    #CheckReplicates?
    
    PD <- PB %>% rename(METAL = PARENT1M, A = COUNT)
    
    PE <- PC %>% rename(METAL = PARENT2M, B = COUNT)
    
    PF <- full_join(PD, PE)
    
    PG <- PF[!(is.na(PF$METAL)),]
  }
  
  
}


#Manip statements

###
#Averaging Sex
###


###
#Recovery Information
###

###
#Averaging Measures
###

###
#Count Observations per Data Step???
###
