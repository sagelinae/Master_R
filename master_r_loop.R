# !diagnostics off
######################################
#Masterfile Script! Changing the SAS masterfile code into R
######################################

library(foreign)
library(dplyr)
library(stringr)

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
x = BV
Mistakes <- function(x, groupby, yeardf,CheckReplicates){
  if(nrow(x) != 0){
    name <- as.character(substitute(x)) #pulls the name of the df the duplicates are from
    year <-  names(table(yeardf$YEAR))[as.vector(table(yeardf$YEAR)) == max(table(yeardf$YEAR))] #This might be ok once in giant for loop
    subset <- x[x$COUNT > 1 & !is.na(x[[groupby]]),] %>% select(groupby, starts_with("METAL"), 
                                                                starts_with("BAND"), starts_with("NEST"), 
                                                                matches("TMATE*")) %>%
                                                         mutate(FROM = name, ID := groupby, YEAR = year)
    CheckReplicates <- bind_rows(CheckReplicates, subset)
  } 
  return(CheckReplicates)
}

#This is a function that takes the mean across a group (like METAL), but it handles
#   NA's better. If I didn't have this in instances where there is NA for the whole group I'd get NaN
#   and would have to change that to NA in another step but this makes it easier.
mean_ <- function(df, col){
  dumb <- mean(col, na.rm = T, data = df) #*** double check round
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

lists <- c('BS','EGG','NEST','RECAP') #List of the type of files will be pulling from
years <- c(86:99, sprintf("%02d", c(00:15))) #Vector of the different years we'll be looping through

f = "86"
for(f in 1:length(years)){
  if(f == "86"){
    AA <- PREA
    }else if(f == "87"){
      AA <- NL
      AB <- LL
      AC <- LM
    }else{
      AA <- NN
      AB <- LL
      AC <- LM
    }
  
  addToEnv <- function(list, regex){
    for(i in list){
      matches <- grep(pattern = regex, names(get(i)))
      list2env(get(i)[matches], envir = .GlobalEnv)
    }
  }
  
  addToEnv(list = lists, regex = paste0("*", f)) #Calls the function
  
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
  
  BCols <- c("LBAND", "PR", "LMETAL", "mr")
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
            BS$PR[z] <- as.character(BC$BAND[i]) 
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS$mr[z] <- BC$METAL[i]
            next
            #IF NEWMETAL == "." 
          }else if( is.na(BC$NEWMETAL[i]) ){ #Changing "." to is.na
            #equivalent to BE
            BS <- bind_rows(BS, BC[i,])
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$NEWPLASTIC[i]
            BS$PR[z] <- as.character(BC$BAND[i]) 
            BS$LMETAL[z] <- BC$METAL[i]
            BS$mr[z] <- NA #changing "." to NA
            next
          }#IF NEWPLASTIC == ""
        }else if( is.na(BC$NEWPLASTIC[i]) ){
          #IF NEWMETAL > 0 
          if(BC$NEWMETAL[i] > 0 & !is.na(BC$NEWMETAL[i])){
            #equivalent to BF
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$BAND[i]
            BS$PR[z] <- NA#"    " 
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS$mr[z] <- BC$METAL[i]
            next
            #IF NEWMETAL == "."
          }else if( is.na(BC$NEWMETAL[i]) ){ #Changing from "." to is.na
            #equivalent to BG
            BS <- bind_rows(BS, BC[i,])
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$BAND[i]
            BS$PR[z] <- NA#"    " 
            BS$LMETAL[z] <- BC$METAL[i]
            BS$mr[z] <- NA #changing "." to NA
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
            BS$PR[z] <- "PBA "
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS$mr[z] <- BC$METAL[i]
            next
            #IF NEWMETAL == "."
          }else if( is.na(BC$NEWMETAL[i]) ){ #Changing == "." to is.na
            #equivalent to BI
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$NEWPLASTIC[i]
            BS$PR[z] <- "PBA "
            BS$LMETAL[z] <- BC$METAL[i]
            BS$mr[z] <- NA #"."
            next
          }#IF NEWPLASTIC == ""
        }else if( is.na(BC$NEWPLASTIC[i]) ){
          #IF NEWMETAL > 0
          if(BC$NEWMETAL[i] > 0 & !is.na(BC$NEWMETAL[i])){
            #equivalent to BJ
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- NA#""
            BS$PR[z] <- NA#"    "
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS$mr[z] <- BC$METAL[i]
            next
            #IF NEWMETAL == "."
          }else if( is.na(BC$NEWMETAL[i]) ){ #Changing from "."
            #equivalent to BK
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- NA#""
            BS$PR[z] <- NA#"    "
            BS$LMETAL[z] <- BC$METAL[i]
            BS$mr[z] <- NA #"."
            next
          }
        }
      }
      #If METAL == "." 
    }else if(BC$METAL[i] == "" | is.na(BC$METAL[i]) ){ #*********Changing "." to ""; maybe needs to be NA? Can't find what it'd actually be in data so far, but I think it's just empty
      #If BAND != ""
      if(BC$BAND[i] != "" & !is.na(BC$BAND[i])){
        #IF NEWPLASTIC != ""
        if( !is.na(BC$NEWPLASTIC[i]) ){
          #IF NEWMETAL > 0
          if(BC$NEWMETAL[i] > 0 & !is.na(BC$NEWMETAL[i])){
            #equivalent to BL
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$NEWPLASTIC[i]
            BS$PR[z] <- as.character(BC$BAND[i]) 
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS$mr[z] <- "12345"
            next
            #IF NEWMETAL == "."
          }else if( is.na(BC$NEWMETAL[i]) ){ #Changing from "." to is.na
            #equivalent to BM
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$NEWPLASTIC[i]
            BS$PR[z] <- as.character(BC$BAND[i]) 
            BS$LMETAL[z] <- NA#"."
            BS$mr[z] <- NA #"."
            next
          }#IF NEWPLASTIC == ""
        }else if( is.na(BC$NEWPLASTIC[i]) ){
          #IF NEWMETAL > 0
          if(BC$NEWMETAL[i] > 0 & !is.na(BC$NEWMETAL[i])){
            #equivalent to BN
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$BAND[i]
            BS$PR[z] <- NA #"    "
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS$mr[z] <- "12345"
            next
            #IF NEWMETAL == "."
          }else if( is.na(BC$NEWMETAL[i]) ){
            #equivalent to BO
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$BAND[i]
            BS$PR[z] <- NA #"    "
            BS$LMETAL[z] <- NA #"."
            BS$mr[z] <- NA #"."
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
            BS$PR[z] <- "PBA "
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS$mr[z] <- "12345"
            next
            #IF NEWMETAL == "."
          }else if(is.na(BC$NEWMETAL[i]) ){ #Changing from "." to is.na
            #equivalent to BQ
            BS <- bind_rows(BS, BC[i,])
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- BC$NEWPLASTIC[i]
            BS$PR[z] <- "PBA "
            BS$LMETAL[z] <- NA #"."
            BS$mr[z] <- NA #"."
            next
          }#IF NEWPLASTIC == ""
        }else if( is.na(BC$NEWPLASTIC[i]) ){
          #IF NEWMETAL > 0
          if(BC$NEWMETAL[i] > 0 & !is.na(BC$NEWMETAL[i])){
            #equivalent to BR
            BS <- bind_rows(BS, BC[i,]) 
            z <- last(which(BC$METAL[i] == BS$METAL))
            BS$LBAND[z] <- NA#""
            BS$PR[z] <- NA #"    " idk if this will fuck anything else up we'll see I guessssss
            BS$LMETAL[z] <- BC$NEWMETAL[i]
            BS$mr[z] <- "12345"
            next
          }
        }
      } 
    }
  }
  BS <- BS[-1,] #Delete our first fake column
  #Lmetal <- BS[,c("METAL", "LMETAL")] #I want to save lmetal because I feel like deleting it without using it is dumb
  BS[[paste0("BAND", f)]] <- as.character(BS$LBAND) #Suppose to have an 86 yr extension
  BS <- BS[, !names(BS) %in% c("BAND", "NEWPLASTIC", "LBAND", "LMETAL")] #Deletes the two columns that is specified in SAS
  
  ###
  #Checking if a metal/plastic band occurs more than once and creating a df to flag it. 
  ###
  
  BT <- BS
  BT$COUNT <- 1
  BT <- BT[BT$FILE == "BS", c("METAL", paste0("BAND", f), "COUNT")]
  BU <- BT %>% group_by(METAL) %>% mutate(COUNT = sum(COUNT))
  CheckReplicates <- Mistakes(x = BU, groupby = "METAL", yeardf = BA, CheckReplicates)
  BV <- BT %>% group_by_at(noquote(paste0("BAND", f))) %>% mutate(COUNT = sum(COUNT))
  CheckReplicates <- Mistakes(x = BV, groupby = noquote(paste0("BAND", f)), yeardf = BA, CheckReplicates)
  
}

#Tuesday: mutate causes an error :( w/ groupby and noquote (vectorizing??? key word???) Figure it out :)






