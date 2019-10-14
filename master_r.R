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
Mistakes <- function(x, groupby, yeardf,CheckReplicates){
  if(nrow(x) != 0){
    name <- as.character(substitute(x)) #pulls the name of the df the duplicates are from
    year <-  names(table(yeardf$YEAR))[as.vector(table(yeardf$YEAR)) == max(table(yeardf$YEAR))] #This might be ok once in giant for loop
    subset <- x[x$COUNT > 1 & !is.na(x[[groupby]]),] %>% select(groupby, starts_with("METAL"), 
                                                                starts_with("BAND"), starts_with("NEST"), 
                                                                matches("TMATE*")) %>%
                                                         mutate(FROM = name, ID = groupby, YEAR = year)
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


###
#1986 Start
###

lists <- c('BS','EGG','NEST','RECAP') #Our lists of dataframes for all of the years. 

#A function that looks through our lists and pulls out the dataframes from the year we specify.
addToEnv <- function(list, regex){
  for(i in list){
    matches <- grep(pattern = regex, names(get(i)))
    list2env(get(i)[matches], envir = .GlobalEnv)
  }
}

addToEnv(list = lists, regex = "*86") #Calls the function

AA86 <- PREA

BA86 <- bsc86
BA86$FILE <- "BS" #Tells us what file this information came from. 
#This for loop deletes rows where there is missing information. We have to loop backwards here since if we're looping forward 
#   while deleting rows our i will become mismatched to what row we are actually on in the dataframe.
for(i in nrow(BA86):1){
  if( (BA86$METAL[i] == "" | is.na(BA86$METAL[i])) & (BA86$BAND[i] == "" | is.na(BA86$BAND[i])) ){ #changing metal == "." to ""
    BA86 <- BA86[-i,]
  }  
  if(BA86$BSTAT[i] == "L" & !is.na(BA86$BSTAT[i])){ 
    BA86 <- BA86[-i,]
  }
}
BA86 <- BA86[ , !names(BA86) %in% c("NINTHPRIM","BODYLEN", 'SAMPLENO', 'COMMENTS', 'NEWPLASTIC', 'NEWMETAL')] #Deletes these rows

BB86 <- recap86
BB86$FILE <- "RE"
for(i in nrow(BB86):1){
  if( (BB86$METAL[i] == "" | is.na(BB86$METAL[i])) & (BB86$BAND[i] == "" | is.na(BB86$BAND[i])) ){ #changing metal == "." to ""
    BB86 <- BB86[-i,]
  } 
}
BB86 <- BB86[ , !names(BB86) %in% c("NINTHPRIM","BODYLEN", 'SAMPLENO', 'COMMENTS', 'RE_STAT')]

BC86 <- bind_rows(BA86, BB86)
BC86$NEWMETAL[which((is.na(BC86$NEWMETAL)| BC86$NEWMETAL == "" )) ] <- NA # changing "." to NA
BC86$NEWPLASTIC[which(BC86$NEWPLASTIC == "")] <- NA

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
            z <- last(which(BC86$METAL[i] == BS86$METAL))
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- as.character(BC86$BAND[i]) 
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- BC86$METAL[i]
            next
          #IF NEWMETAL == "." 
        }else if( is.na(BC86$NEWMETAL[i]) ){ #Changing "." to is.na
          #equivalent to BE86
            BS86 <- bind_rows(BS86, BC86[i,])
            z <- last(which(BC86$METAL[i] == BS86$METAL))
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- as.character(BC86$BAND[i]) 
            BS86$LMETAL[z] <- BC86$METAL[i]
            BS86$mr86[z] <- NA #changing "." to NA
            next
        }#IF NEWPLASTIC == ""
      }else if( is.na(BC86$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0 
        if(BC86$NEWMETAL[i] > 0 & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BF86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- last(which(BC86$METAL[i] == BS86$METAL))
            BS86$LBAND[z] <- BC86$BAND[i]
            BS86$PR86[z] <- NA#"    " 
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- BC86$METAL[i]
            next
          #IF NEWMETAL == "."
        }else if( is.na(BC86$NEWMETAL[i]) ){ #Changing from "." to is.na
           #equivalent to BG86
           BS86 <- bind_rows(BS86, BC86[i,])
           z <- last(which(BC86$METAL[i] == BS86$METAL))
           BS86$LBAND[z] <- BC86$BAND[i]
           BS86$PR86[z] <- NA#"    " 
           BS86$LMETAL[z] <- BC86$METAL[i]
           BS86$mr86[z] <- NA #changing "." to NA
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
            z <- last(which(BC86$METAL[i] == BS86$METAL))
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- "PBA "
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- BC86$METAL[i]
            next
          #IF NEWMETAL == "."
        }else if( is.na(BC86$NEWMETAL[i]) ){ #Changing == "." to is.na
          #equivalent to BI86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- last(which(BC86$METAL[i] == BS86$METAL))
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- "PBA "
            BS86$LMETAL[z] <- BC86$METAL[i]
            BS86$mr86[z] <- NA #"."
            next
        }#IF NEWPLASTIC == ""
      }else if( is.na(BC86$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC86$NEWMETAL[i] > 0 & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BJ86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- last(which(BC86$METAL[i] == BS86$METAL))
            BS86$LBAND[z] <- NA#""
            BS86$PR86[z] <- NA#"    "
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- BC86$METAL[i]
            next
          #IF NEWMETAL == "."
        }else if( is.na(BC86$NEWMETAL[i]) ){ #Changing from "."
          #equivalent to BK86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- last(which(BC86$METAL[i] == BS86$METAL))
            BS86$LBAND[z] <- NA#""
            BS86$PR86[z] <- NA#"    "
            BS86$LMETAL[z] <- BC86$METAL[i]
            BS86$mr86[z] <- NA #"."
            next
        }
      }
    }
   #If METAL == "." 
  }else if(BC86$METAL[i] == "" | is.na(BC86$METAL[i]) ){ #*********Changing "." to ""; maybe needs to be NA? Can't find what it'd actually be in data so far, but I think it's just empty
    #If BAND != ""
    if(BC86$BAND[i] != "" & !is.na(BC86$BAND[i])){
      #IF NEWPLASTIC != ""
      if( !is.na(BC86$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC86$NEWMETAL[i] > 0 & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BL86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- last(which(BC86$METAL[i] == BS86$METAL))
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- as.character(BC86$BAND[i]) 
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- "12345"
            next
          #IF NEWMETAL == "."
        }else if( is.na(BC86$NEWMETAL[i]) ){ #Changing from "." to is.na
          #equivalent to BM86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- last(which(BC86$METAL[i] == BS86$METAL))
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- as.character(BC86$BAND[i]) 
            BS86$LMETAL[z] <- NA#"."
            BS86$mr86[z] <- NA #"."
            next
        }#IF NEWPLASTIC == ""
      }else if( is.na(BC86$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC86$NEWMETAL[i] > 0 & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BN86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- last(which(BC86$METAL[i] == BS86$METAL))
            BS86$LBAND[z] <- BC86$BAND[i]
            BS86$PR86[z] <- NA #"    "
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- "12345"
            next
          #IF NEWMETAL == "."
        }else if( is.na(BC86$NEWMETAL[i]) ){
          #equivalent to BO86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- last(which(BC86$METAL[i] == BS86$METAL))
            BS86$LBAND[z] <- BC86$BAND[i]
            BS86$PR86[z] <- NA #"    "
            BS86$LMETAL[z] <- NA #"."
            BS86$mr86[z] <- NA #"."
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
            z <- last(which(BC86$METAL[i] == BS86$METAL))
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- "PBA "
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- "12345"
            next
          #IF NEWMETAL == "."
        }else if(is.na(BC86$NEWMETAL[i]) ){ #Changing from "." to is.na
          #equivalent to BQ86
            BS86 <- bind_rows(BS86, BC86[i,])
            z <- last(which(BC86$METAL[i] == BS86$METAL))
            BS86$LBAND[z] <- BC86$NEWPLASTIC[i]
            BS86$PR86[z] <- "PBA "
            BS86$LMETAL[z] <- NA #"."
            BS86$mr86[z] <- NA #"."
            next
        }#IF NEWPLASTIC == ""
      }else if( is.na(BC86$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC86$NEWMETAL[i] > 0 & !is.na(BC86$NEWMETAL[i])){
          #equivalent to BR86
            BS86 <- bind_rows(BS86, BC86[i,]) 
            z <- last(which(BC86$METAL[i] == BS86$METAL))
            BS86$LBAND[z] <- NA#""
            BS86$PR86[z] <- NA #"    " idk if this will fuck anything else up we'll see I guessssss
            BS86$LMETAL[z] <- BC86$NEWMETAL[i]
            BS86$mr86[z] <- "12345"
            next
        }
      }
    } 
  }
}
BS86  <- BS86 [-1,] #Delete our first fake column
#Lmetal <- BS86[,c("METAL", "LMETAL")] #I want to save lmetal because I feel like deleting it without using it is dumb
BS86$BAND86 <- as.character(BS86$LBAND)
BS86   <- BS86  [, !names(BS86) %in% c("BAND", "NEWPLASTIC", "LBAND", "LMETAL")] #Deletes the two columns that is specified in SAS
#We don't do anything with LMETAL. I think I want to save it in something else in case I want to use it later?


###
#Checking if a metal/plastic band occurs more than once and creating a df to flag it. 
###

BT86 <- BS86
BT86$COUNT <- 1
BT86 <- BT86[BT86$FILE == "BS", c("METAL", "BAND86", "COUNT")]
BU86 <- BT86 %>% group_by(METAL) %>% mutate(COUNT = sum(COUNT))
CheckReplicates <- Mistakes(x = BU86, groupby = "METAL", yeardf = BA86, CheckReplicates)
BV86 <- BT86 %>% group_by(BAND86) %>% mutate(COUNT = sum(COUNT))
CheckReplicates <- Mistakes(BV86, groupby = "BAND86", yeardf = BA86, CheckReplicates)

#check if a metal band occurs more than once and create a dataframe to flag it
if(length(which(BU86$COUNT > 1)) != 0){
  BW86 <-  BU86[BU86$COUNT > 1,]
  BW86$COMMENTS <-  "This metal was put on 2x"
  BW86$YEAR <- "86"
  BW86 <- BX86[,!names(BX86) %in% "COUNT"]
}else{BW86 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("COMMENTS", "METAL", "YEAR"))}

#Check if a plastic band appears more than once and create a dataframe to flag it
if(length(which(BV86$COUNT > 1)) != 0){
   BX86 <-  BV86[BV86$COUNT > 1,]
   BX86$COMMENTS <-  "This plastic was put on 2x"
   BX86$YEAR <- "86"
   colnames(BX86)[colnames(BX86) == "BAND86"] <- "BAND"
   BX86 <- BX86[,!names(BX86) %in% "COUNT"]
}else{BX86 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("COMMENTS", "BAND", "YEAR"))}


DA86 <- BS86[BS86$DRIVE == "NEST", ]
DA86$AGEB <- NA
DA86$SEXB <- NA
DA86$DATEB <- NA
DA86$YEARB <- NA
for(i in 1:nrow(DA86)){
  if(DA86$FILE[i] == "BS"){
    DA86$AGEB[i] <- DA86$AGE[i]
    DA86$SEXB[i] <- DA86$SEX[i]
    DA86$DATEB[i] <- DA86$DATE[i]
    DA86$YEARB[i] <- DA86$YEAR[i]
  }
}
#Is there a better way to do the below?? I can put it on less lines with ; but not necessarily(sp) more efficient
DA86$an86 <- DA86$AGE; DA86$sN86 <- DA86$SEX; DA86$Nc86 <- DA86$CUL
DA86$Nt86 <- DA86$TAR; DA86$Nm86 <- DA86$MASS; DA86$COUNT <- 1; DA86$n86 <- DA86$COLONY
DA86 <- DA86[,!names(DA86) %in% c("AGE", "SEX","BSTAT", "FILE", "DRIVE", "COLONY", "BP", "YEAR", "CUL", "TAR", "MASS")]

if(length(which(DA86$METAL == "" | is.na(DA86$METAL))) != 0){ #Changed from "."
  DB86 <- DA86[(DA86$METAL == "" | is.na(DA86$METAL)),]
  DB86$COMMENTS <- "Bird was captured with plastic and realeased without metal"
  DB86$YEAR <- "86"
  colnames(DB86)[colnames(DB86) == "BAND86"] <- "BAND"
  DB86 <- DB86[,c("BAND", "COMMENTS", "YEAR")]
}else{DB86 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "COMMENTS", "YEAR"))}

#Create a dataframe with all the observations where metal is not "."
DC86 <- DA86[DA86$METAL != "" & !is.na(DA86$METAL),]


#***This is temporary, but may need to do something at the beginning so it's just nice the whole way through.
DC86$DATE <- as.integer(DC86$DATE)
DC86$Nc86 <- as.integer(DC86$Nc86)
DC86$Nt86 <- as.integer(DC86$Nt86)

#This is for when there are instances of one metal occuring more than once, when that happens we take the mean
#of Nc(cul) and Nt(tar) of the different instances, the min of the dates that occur, and we sum up how many times
#it happened more than once with count.
DD86 <- group_by(DC86, METAL) %>% summarise(meanNc86 = mean_(DC86, Nc86), meanNt86 = mean_(DC86, Nt86), 
                                            DATE = min(DATE), COUNTsum = sum(COUNT)) #Changing from COUNTsum to COUNT
#This one I think is ok to have replicates and to use summarise to combine. Because it includes Recap and BS data
#   which we combined in BS, so we might have duplicate metals because of that and taking the mean between 
#   them makes sense I think. 
#CheckReplicates <- Mistakes(DD86, CheckReplicates)

DE86 <- merge(DA86, DD86, by = c("METAL", "DATE"))
DE86 <- DE86[(!is.na(DE86$COUNTsum)), !names(DE86) %in% c("Nc86", "Nt86")]


#DF86 <- group_by(DE86, METAL) %>% slice(sum(COUNT))
DF86 <- DE86 %>% group_by(METAL) %>% mutate(COUNT = sum(COUNTsum)) # %>% summarise_all(~last(.[which(!is.na(.) & (. != ""))]))
CheckReplicates <- Mistakes(DF86, groupby="METAL", yeardf = BA86 ,CheckReplicates)
DF86 <- DF86[!(DF86$COUNT > 1),!names(DF86) %in% c("n86", "COUNTsum")] 


DG86 <- DF86 %>% rename(NC86 = meanNc86, NT86 = meanNt86)

DH86 <- DG86[(!is.na(DG86$NEWMETAL)),] #Changed from != "." to !is.na
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
if(nrow(DK86) == 0){DL86 <- AA86; DL86$DEL <- ""}else{ #Only runs if our DK df w/ the metals to deletes has stuff in it.
DL86 <- merge(AA86, DK86, by = "METAL")                
DL86 <- DL86[!(DL86$DEL=="Y"),]                   #Deletes the columns we have set to delete
}
DL86$duma <- "1" #what's this dooo

#Delete required metals from the subset of our BS/recap files. DG is the nest drives and BS files from that data.
if(nrow(DK86) == 0){DM86 <- DG86; DM86$DEL <- ""}else{
  DM86 <- merge(DG86, DK86, by = "METAL")
  DM86 <- DM86[!(DM86$DEL == "Y")]
}
DM86$dumb <- "1"
DM86 <- DM86 %>% rename(dumpr86 = PR86, webtag86 = WEBTAG, ntd86 = DATE, ageb86 = AGEB, sexb86 = SEXB, 
                        dateb86 = DATEB, yearb86 = YEARB)

#Merge our cleaned up dataframes together
DN86 <- full_join(DM86, DL86) 

DO86 <- DN86[( is.na(DN86$duma) & DN86$dumb == "1"),] #Changed duma = "" to be is.na, since from my dumb test I think that's what it'd be

DP86 <- DH86 
DP86$METAL <- DP86$mr86

DQ86 <- full_join(AA86, DP86)
DQ86 <- DQ86[which(DQ86$DEL == "Y"),!names(DQ86) %in% "METAL"]

DR86 <- DQ86 %>% rename(dumpr86 = PR86, dbd86 = DATE, webtag86 = WEBTAG, yearb86 = YEARB, dateb86 = DATEB,
                        ageb86 = AGEB, sexb86 = SEXB)
DR86$METAL <- DR86$NEWMETAL
DR86 <- DR86[,!names(DR86) %in% "DEL"]

DS86 <- bind_rows(DN86, DR86)
#Old columns that aren't there so I'm adding them I guesss
DS86$PR86 <- NA
DS86$AGEB <- NA
DS86$SEXB <- NA
DS86$DATEB <- NA
DS86$WEBTAG <- NA

#New columns we're creating
DCols <- c("RP86", "AGE", "SEX", "DATE", "YEAR", "COMMENTS", "BANDB", "WEBTAGB")
DS86[DCols] <- NA

# DS86$RP86 <- NA #DS86$dumpr86 #I don't remember what these were for but I'm scared to delete them now lol
# DS86$AGE <- NA
# DS86$SEX <- NA
# DS86$DATE <- NA #DS86$dateb86
# DS86$YEAR <- NA
# DS86$COMMENTS <- NA
# DS86$BANDB <- NA
# DS86$WEBTAGB <- NA #DS86$webtag86

#***Temporary b/c i'm lazy?
#Could add this in earlier when populating YEARB and yearb86 ?? Initialize it as NA?
#idk but put it somewhere smarter later
DS86$YEARB[which(DS86$YEARB == "")] <- NA
DS86$yearb86[which(DS86$yearb86 == "")] <- NA

for(i in 1:nrow(DS86)){
  #Pr stuff
  if(!is.na(DS86$dumpr86[i]) & !is.na(DS86$PR86[i])){DS86$RP86[i] <- DS86$PR86[i]}else{DS86$RP86[i] <- DS86$dumpr86[i]} 

  #Age stuff
  if(!is.na(DS86$ageb86[i]) & !is.na(DS86$AGEB[i]) & DS86$ageb86[i] == DS86$AGEB[i]){
    DS86$AGE[i] <- DS86$AGEB[i]}
  if(!is.na(DS86$ageb86[i]) & !is.na(DS86$AGEB[i]) & DS86$ageb86[i] != DS86$AGEB[i]){
    DS86$AGE[i] <- DS86$AGEB[i]
    DS86$COMMENTS[i] <- "Age at banding does not agree with BBL and BSC"
  }
  if(!is.na(DS86$ageb86[i]) & is.na(DS86$AGEB[i]) ){DS86$AGE[i] <- DS86$ageb86[i]}
  if(is.na(DS86$ageb86[i]) & !is.na(DS86$AGEB[i]) ){DS86$AGE[i] <- DS86$AGEB[i]}

  #Sex stuff
  if(!is.na(DS86$sexb86[i]) & !is.na(DS86$SEXB[i]) & DS86$sexb86[i] == DS86$SEXB[i]){
    DS86$SEX[i] <- DS86$SEXB[i]
  }
  if(!is.na(DS86$sexb86[i]) & !is.na(DS86$SEXB[i]) & DS86$sexb86[i] != DS86$SEXB[i]){
    DS86$SEX[i] <- DS86$SEXB[i]
    DS86$COMMENTS[i] <- "Sex at banding does not agree with BBL and BSC"
  }
  if(!is.na(DS86$sexb86[i]) & is.na(DS86$SEXB[i]) ){
    DS86$SEX[i] <- DS86$sexb86[i]
  }
  if(is.na(DS86$sexb86[i]) & !is.na(DS86$SEXB[i]) ){
    DS86$SEX[i] <- DS86$SEXB[i]
  }

  #Date Stuff
  if(!is.na(DS86$dateb86[i]) & !is.na(DS86$DATEB[i]) & DS86$dateb86[i] == DS86$DATEB[i]){
    DS86$DATE[i] <- DS86$DATEB[i]}
  if(!is.na(DS86$dateb86[i]) & !is.na(DS86$DATEB[i]) & DS86$dateb86[i] != DS86$DATEB[i]){
    DS86$DATE[i] <- DS86$DATEB[i]
    DS86$COMMENTS[i] <- "Date at banding does not agree with BBL and BSC"
  }
  if(!is.na(DS86$dateb86[i]) & is.na(DS86$DATEB[i]) ){DS86$DATE[i] <- DS86$dateb86[i]}
  if(is.na(DS86$dateb86[i]) & !is.na(DS86$DATEB[i]) ){DS86$DATE[i] <- DS86$DATEB[i]}

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
  if(!is.na(DS86$webtag86[i]) & !is.na(DS86$WEBTAG[i]) & DS86$webtag86[i] == DS86$WEBTAG[i]){
    DS86$WEBTAGB[i] <- DS86$WEBTAG[i]}
  if(!is.na(DS86$webtag86[i]) & !is.na(DS86$WEBTAG[i]) & DS86$webtag86[i] != DS86$WEBTAG[i]){
    DS86$WEBTAGB[i] <- DS86$WEBTAG[i]
    DS86$COMMENTS[i] <- "Age at banding does not agree with BBL and BSC"
  }
  if(!is.na(DS86$webtag86[i]) & is.na(DS86$WEBTAG[i]) ){DS86$WEBTAGB[i] <- DS86$webtag86[i]}
  if(is.na(DS86$webtag86[i]) & !is.na(DS86$WEBTAG[i]) ){DS86$WEBTAGB[i] <- DS86$WEBTAG[i]}
}
#***For some reason SAS puts AGE as "A" when i think it should be "ASY". I have no idea where the A could come from
# in this or in SAS as it doesn't appear until here???????????????????????????? 
DS86 <- DS86[,!names(DS86) %in% c("NEWMETAL", "duma", "dumb", "DEL", "COUNT", "newmetal", "dumpr86",
                                  "PR86", "ageb86", "AGEB", "sexb86", "SEXB","dateb86", "DATEB", 
                                  "yearb86", "YEARB")]

DT86 <- DS86 %>% rename(AGEB = AGE, SEXB = SEX, DATEB= DATE, YEARB = YEAR, PR86 = RP86)
#Reasons why this isn't included in the rename: in year '86 we don't delete our Band and Webtag columns like
#                                               we do in year '87, so if I rename it then we'll have two col's
#                                               with the same name which isn't cool
DT86$BAND <- DT86$BANDB #I don't know why I didn't include these in the rename?? 
DT86$WEBTAG <- DT86$WEBTAGB
DT86 <- DT86[,!names(DT86) %in% c("BANDB", "WEBTAGB")]

DU86 <- DT86[, !names(DT86) %in% "COMMENTS"]
DV86 <- DS86[(!is.na(DT86$COMMENTS)), c("COMMENTS", "METAL", "BAND", "BAND86", "WEBTAG","webtag86")]


############################################################################
#Now we deal with nest data
############################################################################
NEST86 <- NEST86 %>% mutate_all(as.character)

FA86 <- NEST86[(!is.na(NEST86$BAND)),]
FA86 <- FA86[FA86$BAND != "UM",]
if(nrow(FA86) != 0){
  FA86$feband <- NA
  FA86$mateband86 <- NA
  FA86$n86 <- NA
  LOC <- c("AUC", "BIG", "COL", "EC1", "EC2", "KIG", "MCN", "MCS", "BSL", "HSC", "IC1", "IC2", "IC3", 
           "IC4", "HSL", "SSL")
  for(i in 1:nrow(FA86)){
    FA86$feband[i] <- paste0(trimws(str_replace_na(FA86$BAND[i], "")), trimws(str_replace_na(FA86$C1[i], "")))
    FA86$mateband86[i] <- paste0(trimws(str_replace_na(FA86$MATE[i], "")), trimws(str_replace_na(FA86$C2[i], "")))
    
    if(FA86$LOC[i] %in% LOC & !is.na(FA86$LOC[i])){FA86$n86[i] <- FA86$LOC[i]}else{FA86$n86[i] <- "TUT"}
  }
  FA86 <- FA86[, !names(FA86) %in% c("BAND", 'C1', 'MATE', 'C2')]
}else{
  FA86 <- FA86[, !names(FA86) %in% c("BAND", 'C1', 'MATE', 'C2')]
  FA86 <- setNames(data.frame(matrix(ncol = (length(colnames(FA86)) + 3), nrow = 0)), c(colnames(FA86), "feband", "mateband86", "n86"))
  FA86 <- FA86 %>% mutate_all(as.character)
}


FB86 <- NEST86[(!is.na(NEST86$MATE)),]
FB86 <- FB86[FB86$MATE != "UM",]
if(nrow(FB86) != 0){
  FB86$mateband86 <- NA
  FB86$maband <- NA
  for(i in 1:nrow(FB86)){
    #***I think what this for loop does is create an entry for the mate of the bird as well?? Otherwise I don't know why we do this 
    #I think I fucked up and did FA when I was suppose to do FB??
    FB86$mateband86[i] <- paste0(trimws(str_replace_na(FB86$BAND[i], "")), trimws(str_replace_na(FB86$C1[i], "")))
    FB86$maband[i] <- paste0(trimws(str_replace_na(FB86$MATE[i], "")), trimws(str_replace_na(FB86$C2[i], "")))
    
    if(FB86$LOC[i] %in% LOC & !is.na(FB86$LOC[i])){FB86$n86[i] <- FB86$LOC[i]}else{FB86$n86[i] <- "TUT"}
  }
  FB86 <- FB86[, !names(FB86) %in% c("BAND", 'C1', 'MATE', 'C2')]
}else{
  FB86 <- FB86[, !names(FB86) %in% c("BAND", 'C1', 'MATE', 'C2')]
  FB86 <- setNames(data.frame(matrix(ncol = (length(colnames(FB86)) + 3), nrow = 0)), c(colnames(FB86), "mateband86", "maband", "n86"))
  FB86 <- FB86 %>% mutate_all(as.character)
}

FC86 <- full_join(FA86, FB86) #***hmm why do I do full join here and not set??
if(nrow(FC86 != 0)){
  for(i in 1:nrow(FC86)){
    if(!is.na(FC86$feband[i])){FC86$BAND[i] <- FC86$feband[i]}
    if(!is.na(FC86$maband[i])){FC86$BAND[i] <- FC86$maband[i]}
  }
}
FC86 <- FC86[, !names(FC86) %in% c("feband", "maband")]

FD86 <- FC86
FD86$COUNT <- 1
FE86 <- group_by(FD86, BAND, .drop = FALSE) %>% mutate(COUNT = sum(COUNT)) #I don't remember why I included .drop = false here lol
CheckReplicates <- Mistakes(x = FE86, groupby = "BAND", yeardf = BA86,CheckReplicates)


FF86 <- FE86[FE86$COUNT == 1,]
FF86$DEL <- "N"

FG86 <- FE86[FE86$COUNT > 1,]
if(nrow(FG86) != 0){FG86$DEL <- "Y"}else{
  FG86 <- setNames(data.frame(matrix(ncol = (length(colnames(FG86)) + 1), nrow = 0)), c(colnames(FG86), "DEL"))
  FG86 <- FG86 %>% mutate_all(as.character)
}

FI86 <- full_join(FG86, FC86) #was left
FI86 <- FI86[which(FI86$DEL == "Y"),]
if(nrow(FI86) != 0){
  FI86$dud <- 1
}else{
  FI86 <- setNames(data.frame(matrix(ncol = (length(colnames(FI86)) + 1), nrow = 0)), c(colnames(FI86), "dud"))
}

FJ86 <- group_by(FI86, BAND) %>% summarise(dud = sum(COUNT))
#CheckReplicates <- Mistakes(x = FJ86, groupby = BAND, yeardf = BA86,CheckReplicates) #I think this is caught above w/ FE

FK86 <- FJ86[,"BAND"]
if(nrow(FK86) != 0){
  FK86$DEL <- "Y"
  FK86$n86 <- "TUT"
}else{
  FK86 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "DEL", "n86"))
  FK86 <- FK86 %>% mutate_all(as.character)
 }

FL86 <- full_join(FC86, FK86)
FL86 <- FL86[(FL86$DEL != "Y" | is.na(FL86$DEL)),] #Deletes columns with a Y in $DEL basically by pulling out everything that doesn't have a "Y" in the DEL col.

FM86 <- full_join(FL86, FK86)
FM86$CS86 <- FM86$CS; FM86$CSC86 <- FM86$CSC; FM86$E86 <- FM86$E; FM86$F86 <- FM86$F; FM86$LO86 <- FM86$LO
FM86$ID86 <- FM86$ID; FM86$HD86 <- FM86$HD; FM86$HO86 <- FM86$HO; FM86$GLN86 <- FM86$GLN; FM86$ABN86 <- FM86$M
FM86$ABD86 <- FM86$ABDT; FM86$NL86 <- FM86$LOC; FM86$O86 <- FM86$O
keep <- c("NEST", 'BAND', 'n86', 'mateband86', 'CS86', 'CSC86', 'E86', 'F86', 'ID86', 
          'LO86', 'HD86', 'HO86', 'GLN86', 'ABN86', 'ABD86', 'NL86', 'O86')
FM86 <- FM86[, keep]

FO86 <- FM86 %>% rename(REALBAND = BAND)
FO86$BAND <- FO86$mateband86
FO86 <- FO86[-which(FO86$BAND == "UM" | FO86$BAND == ""),]
if(nrow(FO86) != 0){FO86$DUM <- "Y"}else{
  FO86 <- setNames(data.frame(matrix(ncol = (length(colnames(FO86)) + 1), nrow = 0)), c(colnames(FO86), "DUM"))
  FO86 <- FO86 %>% mutate_all(as.character)
}

DU86 <- DU86 %>% mutate_all(as.character)
FP86 <- inner_join(DU86, FO86) #Inner Join since we only want the matches of FO86 in DU86, full_join will return all of DU86 which isn't what we want.

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
  FP86 <- FP86[,c('MATEM86', 'NEST', 'MATEP86', 'n86', 'CS86', 'ID86', 'HD86', 'REALBAND'),]
}else{
  FP86 <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c('MATEM86', 'NEST', 'MATEP86', 'n86', 
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

FT86 <- full_join(DU86, FR86) #Before I just used FS86 in the subset below b/c that's joined but I don't think I should anymore since we fuck with FS86 above?
FT86 <- FT86[which(is.na(FT86$METAL)), "BAND"] #Changing "." to NA
if(nrow(FT86) != 0){
  FT86$YEAR <- "86"
  FT86$COMMENTS <- "Bird seen at nest, no banding record"
}else{
  FT86 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "YEAR", "COMMENTS"))
}

###
#It's H time!!! 
###

HA86 <- BS86[(BS86$DRIVE != "NEST" | is.na(BS86$DRIVE)),]
HA86$AGEB <- NA; HA86$SEXB <- NA; HA86$DATEB <- NA; HA86$YEARB <- NA
for(i in 1:nrow(HA86)){
  if(HA86$FILE[i] == "BS"){
    HA86$AGEB[i] <- HA86$AGE[i]; HA86$SEXB[i] <- HA86$SEX[i] 
    HA86$DATEB[i] <- HA86$DATE[i]; HA86$YEARB[i] <- HA86$YEAR[i]
  }
}
HA86 <- HA86 %>% rename(aB86 = AGE, sB86 = SEX, Bc86= CUL, Bt86= TAR, Bm86=MASS, BP86=BP)
HA86$COUNT <- 1
HA86$BD86[which(HA86$COLONY == "TUT")] <- HA86$DRIVE[which(HA86$COLONY == "TUT")]
HA86$BD86[which(HA86$COLONY != "TUT" | is.na(HA86$COLONY))] <- HA86$COLONY[which(HA86$COLONY != "TUT"| is.na(HA86$COLONY))]
HA86 <- HA86[, !names(HA86) %in% c("BSTAT", "FILE", "DRIVE", "COLONY", "YEAR")]

HB86 <- HA86[(HA86$METAL == "" | is.na(HA86$METAL)), "BAND86", drop = FALSE] #Changed from "." to "" 
HB86 <- HB86 %>% rename(BAND = BAND86)
if(nrow(HB86) != 0){
  HB86$COMMENTS <- "Bird was captured with plastic and released without metal"
  HB86$YEAR <- "86"
}else{
  HB86 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "COMMENTS", "YEAR"))
  HB86 <- HB86 %>% mutate_all(as.character())
}

HC86 <- HA86[(HA86$METAL != "" & !is.na(HA86$METAL)),] #Changed from "." to ""
HC86$Bc86 <- as.numeric(HC86$Bc86)
HC86$Bt86 <- as.numeric(HC86$Bt86)
#I'm not sure about this one either on if I should pull out duplicates or not.
HD86 <- group_by(HC86, METAL) %>% summarise(meanbc86 = mean_(HC86, Bc86), meanbt86 = mean_(HC86,Bt86),
                                            DATE = min(DATE), COUNTsum = sum(COUNT))

HE86 <- full_join(HA86, HD86)
HE86 <- HE86[(!is.na(HE86$COUNTsum)), !names(HE86) %in% c("Bc86", "Bt86")]

HF86 <- HE86[,!names(HE86) %in% "COUNTsum"]
HF86 <- HF86 %>% group_by(METAL) %>% mutate(COUNT = sum(COUNT)) #%>% summarise_all(~last(.[which(!is.na(.) & (. != ""))]))
CheckReplicates <- Mistakes(HF86, groupby = "METAL", yeardf = BA86, CheckReplicates)
HF86 <- HF86[!(HF86$COUNT > 1),]

HG86 <- HF86 %>% rename(BC86 = meanbc86, BT86 = meanbt86)
HH86 <- HG86[(!is.na(HG86$NEWMETAL) ), !names(HG86) %in% "METAL"] #changed from !="." to !is.na
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

HN86 <- full_join(HL86, HM86, by = c("METAL")) %>%
  mutate(mr86 = coalesce(mr86.x, mr86.y),
         webtag86 = coalesce(webtag86.x, webtag86.y),
         BAND86 = coalesce(BAND86.x, BAND86.y),
         dbd86 = coalesce(dbd86.x, dbd86.y),
         DEL = coalesce(DEL.x, DEL.y),
         ) %>%
  select(-mr86.x, -mr86.y, -webtag86.x, -webtag86.y, -BAND86.x, -BAND86.y, -dbd86.x, -dbd86.y, -DEL.x, -DEL.y)

HO86 <- HN86[(is.na(HN86$duma) & HN86$dumb == "1"),]
HP86 <- HH86 
HP86$METAL <- HP86$mr86
HP86 <- HP86 %>% mutate_if(is.logical, as.character)

HQ86 <- full_join(FS86, HP86, by = "METAL") %>% #***I think a right join here would be quicker to get the same outcome, but idk if that could cause issues later??
  mutate(mr86 = coalesce(mr86.x, mr86.y),
         WEBTAG = coalesce(WEBTAG.x, WEBTAG.y),
         BAND86 = coalesce(BAND86.x, BAND86.y),
         PR86 = coalesce(PR86.x, PR86.y),
         AGEB = coalesce(AGEB.x, AGEB.y),
         SEXB = coalesce(SEXB.x, SEXB.y),
         DATEB = coalesce(DATEB.x, DATEB.y),
         YEARB = coalesce(YEARB.x, YEARB.y)
        ) %>%
  select(-mr86.x, -mr86.y, -WEBTAG.x, -WEBTAG.y, -BAND86.x, -BAND86.y, -PR86.x, -PR86.y, -AGEB.x, -AGEB.y,
         -SEXB.x, -SEXB.y, -DATEB.x, -DATEB.y, -YEARB.x, -YEARB.y)

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
HS86$WEBTAGB <- NA #This makes it logical
HS86$COMMENTS <- NA

#Takes a Hot Second: Do something different?
#*** For some reason this is turning webtag class into logical, maybe because I assign it to NA first?'
#   It is that do as.character(NA)
for(i in 1:nrow(HS86)){
  #RP
  if(!is.na(HS86$dumpr86[i]) & !is.na(HS86$PR86[i])){HS86$RP86[i] <- HS86$PR86[i]}else{
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
  if(is.na(HS86$sexb86[i]) & !is.na(HS86$SEXB[i]) ){HS86$SEX[i] <- HS86$SEXB[i]}
  
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
HS86 <- HS86 %>% mutate_if(is.logical, as.character)

HT86 <- HS86 %>% rename(AGEB = AGE, SEXB = SEX, DATEB = DATE, YEARB = YEAR, BAND = BANDB, WEBTAG = WEBTAGB,
                        PR86 = RP86)
HU86 <- HT86[,!names(HT86) %in% "COMMENTS"]
HV86 <- HS86[(!is.na(HT86$COMMENTS)), c('COMMENTS', "METAL")] #, 'BAND', 'WEBTAG'

###
#The J's!!!! Egg Data!!!
###

JA86 <- EGG86[(EGG86$TAG != ""),] #I think "" is right here, so far I have seen "" but no NA's so this seems good for now
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

JB86 <- EGG86[(EGG86$LENGTH != "" | EGG86$WIDTH != ""),]
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

JC86 <- JC86 %>% mutate_at(vars(D, L, W, COUNT), as.numeric)


JD86 <- JC86 %>% group_by(WEBTAG, NEST, EGG) %>% summarise(MD = mean_(JC86, D), ML = mean_(JC86, L), 
                                                           MW = mean_(JC86,W), C = sum(COUNT))
#***Come back to once you do JD87 to decide if you need to add a Mistakes()

JE86 <- JD86
JE86 <- JE86 %>% rename(WTD = MD, EGG1 = ML, EGGW = MW)
if(nrow(JE86) != 0){
  JE86$COUNT <- 1
}else{
  JE86 <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c(colnames(JE86), "COUNT"))
  JE86 <- JE86 %>% mutate_if(is.logical, as.character)
  JE86$COUNT <- as.numeric(JE86$COUNT)
}
JE86 <- JE86[, !names(JE86) %in% "C"]

JF86 <- group_by(JE86, WEBTAG) %>% mutate(COUNT = sum(COUNT))
CheckReplicates <- Mistakes(x = JF86, groupby = "WEBTAG", yeardf = BA86, CheckReplicates)
JF86 <- JE86 %>% group_by(WEBTAG) %>% summarise(COUNT = sum(COUNT))

JG86 <- JF86[(JF86$COUNT > 1), "WEBTAG"]
if(nrow(JG86) != 0){
JG86$DEL <- "Y"
}else{
  JG86 <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("WEBTAG", "DEL"))
  JG86 <- JG86 %>% mutate_all(as.character)
}

JH86 <- JA86 %>% rename(WEBTAG = TAG)
JI86 <- full_join(JG86, JH86)
JI86 <- JI86[-which(JI86$DEL == "Y"), !names(JI86) %in% "DEL"] 

JJ86 <- full_join(JH86, JG86)
JJ86 <- JJ86[which(JJ86$DEL == "Y"),c("WEBTAG", "NEST")]
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
#***Think about this one, can't just name it Count, maybe take multiple col names? Another input lmao?
#CheckReplicates <- Mistakes(x = LB86, groupby = "NEST", yeardf = BA86, CheckReplicates)
LC86 <- NEST86

LD86 <- full_join(LC86, LB86)
LD86 <- LD86[which(LD86$dum > 0 & LD86$NEST != ""),]
LD86$PARENT1P <- as.character(NA)
LD86$PARENT2P <- as.character(NA)
for(i in 1:nrow(LD86)){
  #Some cleaning up could happen here: what happens if there's a band but no color, should it be AO2_ or just AO2?
  # band <- str_pad(LD86$BAND[i], 3, side = "right") 
  # color <- str_replace_na(LD86$C1[i], "")
  
  if(!is.na(LD86$BAND[i])){
    color <- str_replace_na(LD86$C1[i], "")
    LD86$PARENT1P[i] <- paste0(LD86$BAND[i], color)
  }
  
  if(!is.na(LD86$MATE[i])){
    color2 <- str_replace_na(LD86$C2[i], "")
    LD86$PARENT2P[i] <- paste0(LD86$MATE[i], color2)
  }
  
  # band2 <- str_pad((str_replace_na(LD86$MATE[i], " ")), 3, side = 'right')
  # color2 <- str_replace_na(LD86$C2[i], "")
}
LD86 <- LD86[,c("NEST", "PARENT1P", "PARENT2P")]

LE86 <- LD86[(!is.na(LD86$PARENT1P)),] 
LE86$BAND <- LE86$PARENT1P
LE86 <- LE86[,c("NEST", "BAND")]

LF86 <- LD86[(!is.na(LD86$PARENT2P)),]
LF86$BAND <- LF86$PARENT2P
LF86 <- LF86[,c("NEST", "BAND")]

LG86 <- DU86[(!is.na(DU86$BAND) & DU86$BAND != ""), c("BAND", "METAL")] %>% rename(PARENTM = METAL)

LH86 <- full_join(LG86, LE86)
LH86 <- LH86[(!is.na(LH86$NEST)),]  
LH86$PARENTM[which(LH86$BAND == "UM")] <- "UM"
LH86$PARENTM[which(LH86$PARENTM == "")] <- "banded"
LH86 <- LH86 %>% rename(PARENT1M = PARENTM, PARENT1P = BAND)

LI86 <- full_join(LG86, LF86)
LI86 <- LI86[(!is.na(LI86$NEST)),] 
LI86$PARENTM[which(LI86$BAND == "UM")] <- "UM"
LI86$PARENTM[which(LI86$PARENTM == "")] <- "banded"
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
  NB86$YEAR <- (as.numeric(NB86$YEARB) - 1900)
  NB86 <- NB86[which(NB86$YEAR == "86"), !names(NB86) %in% "YEAR"]
}else{
  NB86 <- setNames(data.frame(matrix(nrow = 0, ncol = length(colnames(NB86)) )), colnames(NB86))
  NB86 <- NB86 %>% mutate_all(as.character) 
}

NC86 <- NB86[which(NB86$KEEP == "Y"),]
ND86 <- NB86[which(NB86$AGEB == "L" & is.na(NB86$KEEP)), c("METAL", "WEBTAG", "AGEB", "YEARB")]

NE86 <- NB86[which(NB86$AGEB == "SY"), c("METAL", "WEBTAG", "AGEB", "YEARB")]
if(nrow(NE86) != 0){
  NE86$DUMA <- "Y"
}else{
  NE86 <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("METAL", "WEBTAG", "AGEB", "YEARB", "DUMA"))
  NE86 <- NE86 %>% mutate_all(as.character)
}

NF86 <- NB86[(NB86$AGEB == "ASY"), c("METAL", "WEBTAG", "AGEB", "YEARB")]
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
#Ok so if I don't ignore it I feel like I should create it and then only populate it based on the if statements?
#Felt like by ignoring it I fuck with the original code too much
#See COMMENTS # 8 where does NHID come from why - I'm gonna ignore it

#original
# for(i in 1:nrow(NJ86)){
#   if(!is.na(NJ86$NHID86[i])){NJ86$COMMENTS <- "Double use of applied webtag"}
#   if(is.na(NJ86$NHID86[i])){}
# }
# NJ86$EGGL <- NJ86$EGGL86
# NJ86$EGGW <- NJ86$EGGW86
# NJ86$NHID <- NJ86$NHID86
# NJ86$STATE <- NJ86$STATE86
# NJ86$PILS <- NJ86$PILS86
# NJ86$TAGD <- NJ86$TAGD86
# NJ86$PARENT1M <- NJ86$PARENT1M86
# NJ86$PARENT2M <- NJ86$PARENT2M86
# NJ86$TAGD <- "   "

#Change
NJ86$NHID <- NA
NJ86[c("EGGL", "EGGW", "STATE", "PILS", "TAGD", "PARENT1M", "PARENT2M")] <- NA
for(i in 1:nrow(NJ86)){
  if(!is.na(NJ86$NHID[i]) & !is.na(NJ86$NHID86[i])){NJ86$COMMENTS[i] <- "Double use of applied webtag"}
  if(is.na(NJ86$NHID[i]) &!is.na(NJ86$NHID86[i])){
    NJ86$EGGL[i] <- NJ86$EGGL86[i]
    NJ86$EGGW[i] <- NJ86$EGGW86[i]
    NJ86$NHID[i] <- NJ86$NHID86[i]
    NJ86$STATE[i] <- NJ86$STATE86[i]
    NJ86$PILS[i] <- NJ86$PILS86[i]
    NJ86$TAGD[i] <- NJ86$TAGD86[i]
    NJ86$PARENT1M[i] <- NJ86$PARENT1M86[i]
    NJ86$PARENT2M[i] <- NJ86$PARENT2M86[i]
  }
}
NJ86$TAGD <- "   " #***Why do we do this

NK86 <- NJ86[(!is.na(NJ86$COMMENTS)),]
if(nrow(NK86) != 0){
  NK86$YEAR <- "86"
  NK86 <- NK86[,c("METAL", "BAND", "COMMENTS", "YEAR", "WEBTAG")]
}else{
  NK86 <- setNames(data.frame(matrix(nrow = 0, ncol = 5)), c("METAL", "BAND", "COMMENTS", "YEAR", "WEBTAG"))
  NK86 <- NK86 %>% mutate_all(as.character)
}

NL86 <- NJ86[,!names(NJ86) %in% c("COMMENTS", "EGGL86", "EGGW86", "NHID86", "STATE86", "PILS86", "TAGD86", 
                                  "PARENT1M86", "PARENT2M86", "PARENT1P86", "PARENT2P86")]
NL86 <- NL86[-which(!is.na(NL86$BBLYEAR) & NL86$BBLYEAR < 1967),]

ERRORS86 <- bind_rows(BW86, BX86, DB86, DV86, FT86, HB86, HV86, JJ86, NK86)

##########################
#87 start!
##########################

addToEnv(list = lists, regex = "*87")

#different between 86 and rest of years
AA87 <- NL86 #88/89 sets it to NN
AB87 <- LL86
AC87 <- LM86
#######

#rm(list = grep(pattern = "*86", names(.GlobalEnv), value = TRUE))

BA87 <- bsc87
BA87$FILE <- "BS" #Tells us what file this information came from. 
#This for loop deletes rows where there is missing information. We have to loop backwards here since if we're looping forward 
#   while deleting rows our i will become mismatched to what row we are actually on in the dataframe.
#***I think this can be done in a simpler way using the -which() ect...
for(i in nrow(BA87):1){
  if( (BA87$METAL[i] == "" | is.na(BA87$METAL[i])) & (BA87$BAND[i] == "" | is.na(BA87$BAND[i]))){
    BA87 <- BA87[-i,]
  }  
  if(BA87$BSTAT[i] == "L" & !is.na(BA87$BSTAT[i])){ 
    BA87 <- BA87[-i,]
  }
}

BA87 <- BA87[ , !names(BA87) %in% c("NINTHPRIM","BODYLEN", 'SAMPLENO', 'COMMENTS', 'NEWPLASTIC', 'NEWMETAL')] #Deletes these rows

BB87 <- recap87
BB87$FILE <- "RE"
for(i in nrow(BB87):1){
  if((BB87$METAL[i] == "" | is.na(BB87$METAL[i])) & (BB87$BAND[i] == "" | is.na(BB87$BAND[i]))){
    BB87 <- BB87[-i,]
  } 
}
BB87 <- BB87[ , !names(BB87) %in% c("NINTHPRIM","BODYLEN", 'SAMPLENO', 'COMMENTS', 'RE_STAT')]

BC87 <- bind_rows(BA87, BB87)
BC87$NEWMETAL[which((is.na(BC87$NEWMETAL)| BC87$NEWMETAL == "" )) ] <- NA
BC87$NEWPLASTIC[(BC87$NEWPLASTIC == "")] <- NA

###
#Checking for replacement metal and plastic bands
###

BCols <- c("LBAND", "PR87", "LMETAL", "mr87")
BS87 <- BC87[1,]  #Initializing BS87 to have the same columns as BC87
BS87[BCols] <- NA #Adding the new columns and setting them to NA to start
BS87[1,] <- NA #First fake row we'll get rid of later. I have it for the second for loop?? because I thinkt trying to loop through the 0 column would cause issues?

for(i in 1:nrow(BC87)){
  #If METAL > 0
  if(BC87$METAL[i] > 0 & !is.na(BC87$METAL[i])){
    #If BAND != ""
    if(BC87$BAND[i] != "" & !is.na(BC87$BAND[i])){
      #IF NEWPLASTIC != ""
      if( !is.na(BC87$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0 
        if(BC87$NEWMETAL[i] > 0 & !is.na(BC87$NEWMETAL[i])){
          #equivalent to BD87
          BS87 <- bind_rows(BS87, BC87[i,]) 
          z <- last(which(BC87$METAL[i] == BS87$METAL))
          BS87$LBAND[z] <- BC87$NEWPLASTIC[i]
          BS87$PR87[z] <- as.character(BC87$BAND[i]) 
          BS87$LMETAL[z] <- BC87$NEWMETAL[i]
          BS87$mr87[z] <- BC87$METAL[i]
          next
          #IF NEWMETAL == "." 
        }else if( is.na(BC87$NEWMETAL[i]) ){ #Changing "." to is.na
          #equivalent to BE87
          BS87 <- bind_rows(BS87, BC87[i,])
          z <- last(which(BC87$METAL[i] == BS87$METAL))
          BS87$LBAND[z] <- BC87$NEWPLASTIC[i]
          BS87$PR87[z] <- as.character(BC87$BAND[i]) 
          BS87$LMETAL[z] <- BC87$METAL[i]
          BS87$mr87[z] <- NA #changing "." to NA
          next
        }#IF NEWPLASTIC == ""
      }else if( is.na(BC87$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0 
        if(BC87$NEWMETAL[i] > 0 & !is.na(BC87$NEWMETAL[i])){
          #equivalent to BF87
          BS87 <- bind_rows(BS87, BC87[i,]) 
          z <- last(which(BC87$METAL[i] == BS87$METAL))
          BS87$LBAND[z] <- BC87$BAND[i]
          BS87$PR87[z] <- NA#"    " 
          BS87$LMETAL[z] <- BC87$NEWMETAL[i]
          BS87$mr87[z] <- BC87$METAL[i]
          next
          #IF NEWMETAL == "."
        }else if( is.na(BC87$NEWMETAL[i]) ){ #Changing from "." to is.na
          #equivalent to BG87
          BS87 <- bind_rows(BS87, BC87[i,])
          z <- last(which(BC87$METAL[i] == BS87$METAL)) #since this is updating as we go, theoretically if I do last it should work??
          BS87$LBAND[z] <- BC87$BAND[i]
          BS87$PR87[z] <- NA#"    " 
          BS87$LMETAL[z] <- BC87$METAL[i]
          BS87$mr87[z] <- NA #changing "." to NA
          next
        }
      }#If BAND == ""
    }else if(BC87$BAND[i] == "" & !is.na(BC87$BAND[i])){
      #IF NEWPLASTIC != ""
      if( !is.na(BC87$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC87$NEWMETAL[i] > 0 & !is.na(BC87$NEWMETAL[i])){
          #equivalent to BH87
          BS87 <- bind_rows(BS87, BC87[i,]) 
          z <- last(which(BC87$METAL[i] == BS87$METAL))
          BS87$LBAND[z] <- BC87$NEWPLASTIC[i]
          BS87$PR87[z] <- "PBA "
          BS87$LMETAL[z] <- BC87$NEWMETAL[i]
          BS87$mr87[z] <- BC87$METAL[i]
          next
          #IF NEWMETAL == "."
        }else if( is.na(BC87$NEWMETAL[i]) ){ #Changing == "." to is.na
          #equivalent to BI87
          BS87 <- bind_rows(BS87, BC87[i,]) 
          z <- last(which(BC87$METAL[i] == BS87$METAL))
          BS87$LBAND[z] <- BC87$NEWPLASTIC[i]
          BS87$PR87[z] <- "PBA "
          BS87$LMETAL[z] <- BC87$METAL[i]
          BS87$mr87[z] <- NA #"."
          next
        }#IF NEWPLASTIC == ""
      }else if( is.na(BC87$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC87$NEWMETAL[i] > 0 & !is.na(BC87$NEWMETAL[i])){
          #equivalent to BJ87
          BS87 <- bind_rows(BS87, BC87[i,]) 
          z <- last(which(BC87$METAL[i] == BS87$METAL))
          BS87$LBAND[z] <- NA#""
          BS87$PR87[z] <- NA#"    "
          BS87$LMETAL[z] <- BC87$NEWMETAL[i]
          BS87$mr87[z] <- BC87$METAL[i]
          next
          #IF NEWMETAL == "."
        }else if( is.na(BC87$NEWMETAL[i]) ){ #Changing from "."
          #equivalent to BK87
          BS87 <- bind_rows(BS87, BC87[i,]) 
          z <- last(which(BC87$METAL[i] == BS87$METAL))
          BS87$LBAND[z] <- NA#""
          BS87$PR87[z] <- NA#"    "
          BS87$LMETAL[z] <- BC87$METAL[i]
          BS87$mr87[z] <- NA #"."
          next
        }
      }
    }
    #If METAL == "." 
  }else if(BC87$METAL[i] == "" | is.na(BC87$METAL[i]) ){ #*********Changing "." to ""; maybe needs to be NA? Can't find what it'd actually be in data so far, but I think it's just empty
    #If BAND != ""
    if(BC87$BAND[i] != "" & !is.na(BC87$BAND[i])){
      #IF NEWPLASTIC != ""
      if( !is.na(BC87$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC87$NEWMETAL[i] > 0 & !is.na(BC87$NEWMETAL[i])){
          #equivalent to BL87
          BS87 <- bind_rows(BS87, BC87[i,]) 
          z <- last(which(BC87$METAL[i] == BS87$METAL))
          BS87$LBAND[z] <- BC87$NEWPLASTIC[i]
          BS87$PR87[z] <- as.character(BC87$BAND[i]) 
          BS87$LMETAL[z] <- BC87$NEWMETAL[i]
          BS87$mr87[z] <- "12345"
          next
          #IF NEWMETAL == "."
        }else if( is.na(BC87$NEWMETAL[i]) ){ #Changing from "." to is.na
          #equivalent to BM87
          BS87 <- bind_rows(BS87, BC87[i,]) 
          z <- last(which(BC87$METAL[i] == BS87$METAL))
          BS87$LBAND[z] <- BC87$NEWPLASTIC[i]
          BS87$PR87[z] <- as.character(BC87$BAND[i]) 
          BS87$LMETAL[z] <- NA#"."
          BS87$mr87[z] <- NA #"."
          next
        }#IF NEWPLASTIC == ""
      }else if( is.na(BC87$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC87$NEWMETAL[i] > 0 & !is.na(BC87$NEWMETAL[i])){
          #equivalent to BN87
          BS87 <- bind_rows(BS87, BC87[i,]) 
          z <- last(which(BC87$METAL[i] == BS87$METAL))
          BS87$LBAND[z] <- BC87$BAND[i]
          BS87$PR87[z] <- NA #"    "
          BS87$LMETAL[z] <- BC87$NEWMETAL[i]
          BS87$mr87[z] <- "12345"
          next
          #IF NEWMETAL == "."
        }else if( is.na(BC87$NEWMETAL[i]) ){
          #equivalent to BO87
          BS87 <- bind_rows(BS87, BC87[i,]) 
          z <- last(which(BC87$METAL[i] == BS87$METAL))
          BS87$LBAND[z] <- BC87$BAND[i]
          BS87$PR87[z] <- NA #"    "
          BS87$LMETAL[z] <- NA #"."
          BS87$mr87[z] <- NA #"."
          next
        }
      }#If BAND == ""
    }else if(BC87$BAND[i] == "" & !is.na(BC87$BAND[i])){
      #IF NEWPLASTIC != ""
      if( !is.na(BC87$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC87$NEWMETAL[i] > 0 & !is.na(BC87$NEWMETAL[i])){
          #equivalent to BP87
          BS87 <- bind_rows(BS87, BC87[i,]) 
          z <- last(which(BC87$METAL[i] == BS87$METAL))
          BS87$LBAND[z] <- BC87$NEWPLASTIC[i]
          BS87$PR87[z] <- "PBA "
          BS87$LMETAL[z] <- BC87$NEWMETAL[i]
          BS87$mr87[z] <- "12345"
          next
          #IF NEWMETAL == "."
        }else if(is.na(BC87$NEWMETAL[i]) ){ #Changing from "." to is.na
          #equivalent to BQ87
          BS87 <- bind_rows(BS87, BC87[i,])
          z <- last(which(BC87$METAL[i] == BS87$METAL))
          BS87$LBAND[z] <- BC87$NEWPLASTIC[i]
          BS87$PR87[z] <- "PBA "
          BS87$LMETAL[z] <- NA #"."
          BS87$mr87[z] <- NA #"."
          next
        }#IF NEWPLASTIC == ""
      }else if( is.na(BC87$NEWPLASTIC[i]) ){
        #IF NEWMETAL > 0
        if(BC87$NEWMETAL[i] > 0 & !is.na(BC87$NEWMETAL[i])){
          #equivalent to BR87
          BS87 <- bind_rows(BS87, BC87[i,]) 
          z <- last(which(BC87$METAL[i] == BS87$METAL))
          BS87$LBAND[z] <- NA#""
          BS87$PR87[z] <- NA #"    " idk if this will fuck anything else up we'll see I guessssss
          BS87$LMETAL[z] <- BC87$NEWMETAL[i]
          BS87$mr87[z] <- "12345"
          next
        }
      }
    } 
  }
}
BS87  <- BS87 [-1,] #Delete our first fake column
BS87   <- BS87  [, !names(BS87  ) %in% c("BAND", "NEWPLASTIC")] #Deletes the two columns that is specified in SAS
BS87$BAND87 <- as.character(BS87$LBAND)
BS87 <- BS87[,!names(BS87) %in% c("LBAND", "LMETAL")] #We don't do anything with LMETAL. I think I want to save it in something else in case I want to use it later?


###
#Checking if a metal/plastic band occurs more than once and creating a df to flag it. 
###

BT87 <- BS87
BT87$COUNT <- 1
BT87 <- BT87[(BT87$FILE == "BS"), c("METAL", "BAND87", "COUNT")]
BU87 <- BT87 %>% group_by(METAL) %>% mutate(COUNT = sum(COUNT))
CheckReplicates <- Mistakes(x = BU87, groupby = "METAL", yeardf = BA87, CheckReplicates)
BV87 <- BT87 %>% group_by(BAND87) %>% mutate(COUNT = sum(COUNT))
CheckReplicates <- Mistakes(x = BV87, groupby = "BAND87", yeardf = BA87, CheckReplicates)

#Different from 86 #idk why I said this now I can't see if there's a difference?? past sage why did you say this what secrets do you hold
#check if a metal band occurs more than once and create a dataframe to flag it
if(length(which(BU87$COUNT > 1)) != 0){
  BW87 <-  BU87[(BU87$COUNT > 1),]
  BW87$COMMENTS <-  "This metal was put on 2x"
  BW87$YEAR <- "87"
  BW87 <- BX87[,!names(BX87) %in% "COUNT"]
}else{BW87 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("COMMENTS", "METAL", "YEAR"))}

#Check if a plastic band appears more than once and create a dataframe to flag it
if(length(which(BV87$COUNT > 1)) != 0){
  BX87 <-  BV87[(BV87$COUNT > 1),]
  BX87$COMMENTS <-  "This plastic was put on 2x"
  BX87$YEAR <- "87"
  colnames(BX87)[colnames(BX87) == "BAND87"] <- "BAND"
  BX87 <- BX87[,!names(BX87) %in% "COUNT"]
}else{BX87 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("COMMENTS", "BAND", "YEAR"))}


DA87 <- BS87[(BS87$DRIVE == "NEST"), ]
DA87$AGEB <- NA
DA87$SEXB <- NA
DA87$DATEB <- NA
DA87$YEARB <- NA
for(i in 1:nrow(DA87)){
  if(DA87$FILE[i] == "BS"){
    DA87$AGEB[i] <- DA87$AGE[i]
    DA87$SEXB[i] <- DA87$SEX[i]
    DA87$DATEB[i] <- DA87$DATE[i]
    DA87$YEARB[i] <- DA87$YEAR[i]
  }
}
#Is there a better way to do the below?? I can put it on less lines with ; but not necessarily(sp) more efficient
DA87$DATEB <- DA87$DATE; DA87$an87 <- DA87$AGEB; DA87$sN87 <- DA87$SEXB; DA87$Nc87 <- DA87$CUL
DA87$Nt87 <- DA87$TAR; DA87$Nm87 <- DA87$MASS; DA87$COUNT <- 1; DA87$n87 <- DA87$COLONY
DA87 <- DA87[,!names(DA87) %in% c("AGE", "SEX","BSTAT", "FILE", "DRIVE", "COLONY", "BP", "YEAR", "CUL", "TAR", "MASS")]

if(length(which(DA87$METAL == "" | is.na(DA87$METAL))) != 0){ #Changed from "."
  DB87 <- DA87[(DA87$METAL == ""),]
  DB87$COMMENTS <- "Bird was captured with plastic and realeased without metal"
  DB87$YEAR <- "87"
  colnames(DB87)[colnames(DB87) == "BAND87"] <- "BAND"
  DB87 <- DB87[,c("BAND", "COMMENTS", "YEAR")]
}else{DB87 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "COMMENTS", "YEAR"))}

#Create a dataframe with all the observations where metal is not "."
DC87 <- DA87[(DA87$METAL != "" & !is.na(DA87$METAL)),] #changed from "."


#This is temporary, but may need to do something at the beginning so it's just nice the whole way through.
DC87$DATE <- as.integer(DC87$DATE)
DC87$Nc87 <- as.integer(DC87$Nc87)
DC87$Nt87 <- as.integer(DC87$Nt87)

#This is for when there are instances of one metal occuring more than once, when that happens we take the mean
#of Nc(cul) and Nt(tar) of the different instances, the min of the dates that occur, and we sum up how many times
#it happened more than once with count.
DD87 <- group_by(DC87, METAL) %>% summarise(meanNc87 = mean_(DC87, Nc87), meanNt87 = mean_(DC87,Nt87), 
                                            DATE = min(DATE), COUNTsum = sum(COUNT))
#Again I think summarise is fine since it originally is from an rbind from two different file types? Recap 
#  and bsc? 

DE87 <- merge(DA87, DD87, by = c("METAL", "DATE"))
DE87 <- DE87[(!is.na(DE87$COUNTsum)), !names(DE87) %in% c("Nc87", "Nt87")]

#DF87 <- group_by(DE87, METAL) %>% slice(sum(COUNT))
DF87 <- DE87 %>% group_by(METAL) %>% mutate(COUNT = sum(COUNT)) #%>% summarise_all(~last(.[which(!is.na(.) & (. != ""))]))
CheckReplicates <- Mistakes(x = DF87, groupby = "METAL", yeardf = BA87, CheckReplicates)
DF87 <- DF87[!(DF87$COUNTsum > 1),!names(DE87) %in% c("n87", "COUNTsum")]

DG87 <- DF87 %>% rename(NC87 = meanNc87, NT87 = meanNt87)

DH87 <- DG87[(!is.na(DG87$NEWMETAL)),] #Changed from "."
if(nrow(DH87) == 0){DH87 <- DH87 %>% rename(DEL = METAL); DH87 <- DH87 %>% select(-DEL,DEL)}else{
  DH87$DEL <- "Y"
  DH87 <- DH87[,!names(DH87) %in% "METAL"]
}

DI87 <- DH87[,c("mr87", "DEL")]
DI87 <- DI87 %>% rename(METAL = mr87)

DJ87 <- DH87[,c("NEWMETAL", "DEL")]
DJ87 <- DJ87 %>% rename(METAL = NEWMETAL)

DK87 <- bind_rows(DI87, DJ87)

#Delete required metals from the PREA/old bands data
if(nrow(DK87) == 0){DL87 <- AA87; DL87$DEL <- ""}else{ #Only runs if our DK df w/ the metals to deletes has stuff in it.
  DL87 <- full_join(AA87, DK87, by = "METAL")
  DL87 <- DL87[-which(DL87$DEL=="Y"),]                   #Deletes the columns we have set to delete
}
DL87$duma <- "1" #what's this dooo

#Delete required metals from the subset of our BS/recap files. DG is the nest drives and BS files from that data.
if(nrow(DK87) == 0){DM87 <- DG87; DM87$DEL <- ""}else{
  DM87 <- merge(DG87, DK87, by = "METAL")
  DM87 <- DM87[-which(DM87$DEL == "Y")]
}
DM87$dumb <- "1"
DM87 <- DM87 %>% rename(dumpr87 = PR87, webtag87 = WEBTAG, ntd87 = DATE, ageb87 = AGEB, sexb87 = SEXB,
                        dateb87 = DATEB, yearb87 = YEARB)
#Merge our cleaned up dataframes together

DN87 <- full_join(DM87, DL87, by = c("METAL", "DEL")) 

DO87 <- DN87[which(DN87$duma == "" & DN87$dumb == "1"),] #***When will duma ever not be 1???

DP87 <- DH87
DP87$METAL <- DP87$mr87
DQ87 <- full_join(AA87, DP87)
DQ87 <- DQ87[which(DQ87$DEL == "Y"),!names(DQ87) %in% "METAL"]

DR87 <- DQ87 %>% rename(dumpr87 = PR87, dbd87 = DATE, webtag87 = WEBTAG, yearb87 = YEARB, dateb87 = DATEB,
                        ageb87 = AGEB, sexb87 = SEXB)
DR87$METAL <- DR87$NEWMETAL
DR87 <- DR87[,!names(DR87) %in% "DEL"]

DS87 <- bind_rows(DN87, DR87)
#Old columns that we use to compare but don't actually have
DS87$PR87 <- NA

#New columns we're adding
DCols <- c("RP87", "AGE", "SEX", "DATE", "YEAR", "COMMENTS", "BANDB", "WEBTAGB")
DS[DCols] <- NA

DS87$RP87 <- NA #DS87$dumpr87
DS87$AGE <- NA
DS87$SEX <- NA
DS87$DATE <- NA #DS87$dateb87
DS87$YEAR <- NA
DS87$COMMENTS <- NA
DS87$BANDB <- NA
DS87$WEBTAGB <- NA #DS87$webtag87

#***Temporary b/c i'm lazy?
#Could add this in earlier when populating YEARB and yearb86 ?? Initialize it as NA?
#idk but put it somewhere smarter later
DS87$YEARB[which(DS87$YEARB == "")] <- NA
DS87$yearb87[which(DS87$yearb87 == "")] <- NA

for(i in 1:nrow(DS87)){
  #Pr stuff
  #No PR87 column still, so this one's a lil odd still idk. I just initialized it to NA 
  if(!is.na(DS87$dumpr87[i]) & !is.na(DS87$PR87[i])){DS87$RP87[i] <- DS87$PR87[i]}else{DS87$RP87[i] <- DS87$dumpr87[i]} 
  
  #Age stuff
  if(!is.na(DS87$ageb87[i]) & !is.na(DS87$AGEB[i]) & DS87$ageb87[i] == DS87$AGEB[i]){
    DS87$AGE[i] <- DS87$AGEB[i]}
  if(!is.na(DS87$ageb87[i]) & !is.na(DS87$AGEB[i]) & DS87$ageb87[i] != DS87$AGEB[i]){
    DS87$AGE[i] <- DS87$AGEB[i]
    DS87$COMMENTS[i] <- "Age at banding does not agree with BBL and BSC"
  }
  if(!is.na(DS87$ageb87[i]) & is.na(DS87$AGEB[i]) ){DS87$AGE[i] <- DS87$ageb87[i]}
  if(is.na(DS87$ageb87[i]) & !is.na(DS87$AGEB[i]) ){DS87$AGE[i] <- DS87$AGEB[i]}
  
  #Sex stuff
  if(!is.na(DS87$sexb87[i]) & !is.na(DS87$SEXB[i]) & DS87$sexb87[i] == DS87$SEXB[i]){
    DS87$SEX[i] <- DS87$SEXB[i]
  }
  if(!is.na(DS87$sexb87[i]) & !is.na(DS87$SEXB[i]) & DS87$sexb87[i] != DS87$SEXB[i]){
    DS87$SEX[i] <- DS87$SEXB[i]
    DS87$COMMENTS[i] <- "Sex at banding does not agree with BBL and BSC"
  }
  if(!is.na(DS87$sexb87[i]) & is.na(DS87$SEXB[i]) ){
    DS87$SEX[i] <- DS87$sexb87[i]
  }
  if(is.na(DS87$sexb87[i]) & !is.na(DS87$SEXB[i]) ){
    DS87$SEX[i] <- DS87$SEXB[i]
  }
  
  #Date Stuff
  if(!is.na(DS87$dateb87[i]) & !is.na(DS87$DATEB[i]) & DS87$dateb87[i] == DS87$DATEB[i]){
    DS87$DATE[i] <- DS87$DATEB[i]}
  if(!is.na(DS87$dateb87[i]) & !is.na(DS87$DATEB[i]) & DS87$dateb87[i] != DS87$DATEB[i]){
    DS87$DATE[i] <- DS87$DATEB[i]
    DS87$COMMENTS[i] <- "Date at banding does not agree with BBL and BSC"
  }
  if(!is.na(DS87$dateb87[i]) & is.na(DS87$DATEB[i]) ){DS87$DATE[i] <- DS87$dateb87[i]}
  if(is.na(DS87$dateb87[i]) & !is.na(DS87$DATEB[i]) ){DS87$DATE[i] <- DS87$DATEB[i]}
  
  #Year Stuff
  if(!is.na(DS87$yearb87[i]) & !is.na(DS87$YEARB[i]) & DS87$yearb87[i] == DS87$YEARB[i]){
    DS87$YEAR[i] <- DS87$YEARB[i]
  }
  if(!is.na(DS87$yearb87[i]) & !is.na(DS87$YEARB[i]) & DS87$yearb87[i] != DS87$YEARB[i]){
    DS87$YEAR[i] <- DS87$YEARB[i]
    DS87$COMMENTS[i] <- "Year at banding does not agree with BBL and BSC"
  }
  if(!is.na(DS87$yearb87[i]) & is.na(DS87$YEARB[i]) ){
    DS87$YEAR[i] <- DS87$yearb87[i]
  }
  if(is.na(DS87$yearb87[i]) & !is.na(DS87$YEARB[i]) ){
    DS87$YEAR[i] <- DS87$YEARB[i]
  }
  
  #Band Stuff
  if(!is.na(DS87$BAND87[i]) & !is.na(DS87$BAND[i]) & DS87$BAND87[i] == DS87$BAND[i]){
    DS87$BANDB[i] <- DS87$BAND[i]
  }
  if(!is.na(DS87$BAND87[i]) & !is.na(DS87$BAND[i]) & DS87$BAND87[i] != DS87$BAND[i]
     & !is.na(DS87$RP87[i])){
    DS87$BANDB[i] <- DS87$BAND87[i]
    #my own COMMENTS b/c he didn't include one??
    DS87$COMMENTS[i] <- "Band does not agree with BBL and BSC"
  }
  if(!is.na(DS87$BAND87[i]) & !is.na(DS87$BAND[i]) & DS87$BAND87[i] != DS87$BAND[i]
     & is.na(DS87$RP87[i])){
    DS87$BANDB[i] <- DS87$BAND[i]
    DS87$COMMENTS[i] <- "Plastic bands do not agree for this recapture with original BSC"
  }
  if(!is.na(DS87$BAND87[i]) & is.na(DS87$BAND[i]) ){
    DS87$BANDB[i] <- DS87$BAND87[i]
  }
  if(is.na(DS87$BAND87[i]) & !is.na(DS87$BAND[i]) ){
    DS87$BANDB[i] <- DS87$BAND[i]
  }
  
  #Webtag stuff
  if(!is.na(DS87$webtag87[i]) & !is.na(DS87$WEBTAG[i]) & DS87$webtag87[i] == DS87$WEBTAG[i]){
    DS87$WEBTAGB[i] <- DS87$WEBTAG[i]}
  if(!is.na(DS87$webtag87[i]) & !is.na(DS87$WEBTAG[i]) & DS87$webtag87[i] != DS87$WEBTAG[i]){
    DS87$WEBTAGB[i] <- DS87$WEBTAG[i]
    DS87$COMMENTS[i] <- "Age at banding does not agree with BBL and BSC"
  }
  if(!is.na(DS87$webtag87[i]) & is.na(DS87$WEBTAG[i]) ){DS87$WEBTAGB[i] <- DS87$webtag87[i]}
  if(is.na(DS87$webtag87[i]) & !is.na(DS87$WEBTAG[i]) ){DS87$WEBTAGB[i] <- DS87$WEBTAG[i]}
}
#This is different from year '86 and i'm not entirely sure why
DS87 <- DS87[,!names(DS87) %in% c("NEWMETAL", "duma", "dumb", "DEL", "COUNT", "newmetal", "dumpr87",
                                  "PR87", "ageb87", "AGEB", "sexb87", "SEXB", "dateb87", "DATEB",
                                  "yearb87", "YEARB", "BAND87", "BAND", "webtag87", "WEBTAG")]

DT87 <- DS87 %>% rename(AGEB = AGE, SEXB = SEX, DATEB= DATE, YEARB = YEAR, WEBTAG = WEBTAGB,
                        PR87 = RP87)
DT87$BAND <- DT87$BANDB # i don't know why i decided to include webtag in rename but not in 86 wtf sage
DT87 <- DT87[,!names(DT87) %in% "BANDB"]

DU87 <- DT87[, !names(DT87) %in% "COMMENTS"]
DV87 <- DS87[which(!is.na(DT87$COMMENTS)), c("COMMENTS", "METAL")]


############################################################################
#Now we deal with nest data
############################################################################
NEST87 <- NEST87 %>% mutate_all(as.character)

FA87 <- NEST87[(!is.na(NEST87$BAND)),]
if(nrow(FA87) != 0){
  FA87 <- FA87[FA87$BAND != "UM",]
  FA87$feband <- NA
  FA87$mateband87 <- NA
  FA87$n87 <- NA
  LOC <- c("AUC", "BIG", "COL", "EC1", "EC2", "KIG", "MCN", "MCS", "BSL", "HSC", "IC1", "IC2", "IC3",
           "IC4", "HSL", "SSL")
  for(i in 1:nrow(FA87)){
    FA87$feband[i] <- paste0(trimws(str_replace_na(FA87$BAND[i], "")), trimws(str_replace_na(FA87$C1[i], "")))
    FA87$mateband87[i] <- paste0(trimws(str_replace_na(FA87$MATE[i], "")), trimws(str_replace_na(FA87$C2[i], "")))

    if(FA87$LOC[i] %in% LOC & !is.na(FA87$LOC[i])){FA87$n87[i] <- FA87$LOC[i]}else{FA87$n87[i] <- "TUT"}
  }
  FA87 <- FA87[, !names(FA87) %in% c("BAND", 'C1', 'MATE', 'C2')]
}else{
  FA87 <- FA87[, !names(FA87) %in% c("BAND", 'C1', 'MATE', 'C2')]
  FA87 <- setNames(data.frame(matrix(ncol = (length(colnames(FA87)) + 3), nrow = 0)), c(colnames(FA87), "feband", "mateband87", "n87"))
  FA87 <- FA87 %>% mutate_all(as.character)
}

FB87 <- NEST87[(!is.na(NEST87$MATE)),]
if(nrow(FB87) != 0){
  FB87 <- FB87[FB87$MATE != "UM",]
  FB87$mateband87 <- NA
  FB87$maband <- NA
  for(i in 1:nrow(FB87)){
    FB87$mateband87[i] <- paste0(trimws(str_replace_na(FB87$BAND[i], "")), trimws(str_replace_na(FB87$C1[i], "")))
    FB87$maband[i] <- paste0(trimws(str_replace_na(FB87$MATE[i], "")), trimws(str_replace_na(FB87$C2[i], "")))

    if(FB87$LOC[i] %in% LOC & !is.na(FB87$LOC[i])){FB87$n87[i] <- FB87$LOC[i]}else{FB87$n87[i] <- "TUT"}
  }
  FB87 <- FB87[, !names(FB87) %in% c("BAND", 'C1', 'MATE', 'C2')]
}else{
  FB87 <- FB87[, !names(FB87) %in% c("BAND", 'C1', 'MATE', 'C2')]
  FB87 <- setNames(data.frame(matrix(ncol = (length(colnames(FB87)) + 3), nrow = 0)), c(colnames(FB87), "mateband87", "maband", "n87"))
  FB87 <- FB87 %>% mutate_all(as.character)
}

FC87 <- full_join(FA87, FB87) #Check here again why I use full_join and not set
for(i in 1:nrow(FC87)){
  if(!is.na(FC87$feband[i])){FC87$BAND[i] <- FC87$feband[i]}
  if(!is.na(FC87$maband[i])){FC87$BAND[i] <- FC87$maband[i]}
}
FC87 <- FC87[, !names(FC87) %in% c("feband", "maband")]
FD87 <- FC87
FD87$COUNT <- 1
FE87 <- group_by(FD87, BAND, .drop = FALSE) %>% mutate(COUNT = sum(COUNT))
CheckReplicates <- Mistakes(x = FE87, groupby = "BAND", yeardf = BA87, CheckReplicates)


FF87 <- FE87[(FE87$COUNT == 1),]
FF87$DEL <- "N"

FG87 <- FE87[(FE87$COUNT > 1),]
if(nrow(FG87) != 0){FG87$DEL <- "Y"}else{
  FG87 <- setNames(data.frame(matrix(ncol = (length(colnames(FG87)) + 1), nrow = 0)), c(colnames(FG87), "DEL"))
  FG87 <- FG87 %>% mutate_all(as.character)
}

FI87 <- full_join(FG87, FC87) #was left join
FI87 <- FI87[which(FI87$DEL == "Y"),]
if(nrow(FI87) != 0){
    FI87$dud <- 1
}else{
  FI87 <- setNames(data.frame(matrix(ncol = (length(colnames(FI87)) + 1), nrow = 0)), c(colnames(FI87), "dud")) 
}

FJ87 <- group_by(FI87, BAND) %>% summarise(dud = sum(COUNT))

FK87 <- FJ87[,"BAND"]
if(nrow(FK87) != 0){
  FK87$DEL <- "Y"
  FK87$n87 <- "TUT"
}else{
  FK87 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "DEL", "n87"))
  FK87 <- FK87 %>% mutate_all(as.character)
}

FL87 <- full_join(FC87, FK87)
FL87 <- FL87[(FL87$DEL != "Y" | is.na(FL87$DEL)),] #Deletes columns with a Y in $DEL

FM87 <- full_join(FL87, FK87)
FM87$CS87 <- FM87$CS; FM87$CSC87 <- FM87$CSC; FM87$E87 <- FM87$E; FM87$F87 <- FM87$F; FM87$LO87 <- FM87$LO
FM87$ID87 <- FM87$ID; FM87$HD87 <- FM87$HD; FM87$HO87 <- FM87$HO; FM87$GLN87 <- FM87$GLN; FM87$ABN87 <- FM87$M
FM87$ABD87 <- FM87$ABDT; FM87$NL87 <- FM87$LOC; FM87$O87 <- FM87$O
keep <- c("NEST", 'BAND', 'n87', 'mateband87', 'CS87', 'CSC87', 'E87', 'F87', 'ID87',
          'LO87', 'HD87', 'HO87', 'GLN87', 'ABN87', 'ABD87', 'NL87', 'O87')
FM87 <- FM87[, keep]

FO87 <- FM87 %>% rename(REALBAND = BAND)
FO87$BAND <- FO87$mateband87
FO87 <- FO87[-which(FO87$BAND == "UM" | FO87$BAND == "" | is.na(FO87$BAND)),]
if(nrow(FO87) != 0){FO87$DUM <- "Y"}else{
  FO87 <- setNames(data.frame(matrix(ncol = (length(colnames(FO87)) + 1), nrow = 0)), c(colnames(FO87), "DUM"))
  FO87 <- FO87 %>% mutate_all(as.character)
}

#This takes a while, track classes throughout to make it better
DU87 <- DU87 %>% mutate_all(as.character)
FP87 <- inner_join(DU87, FO87)

if(nrow(FP87) != 0){
  FP87$MATEM87 <- NA
  FP87$MATEP87 <- NA
  FP87$NMSEX87 <- NA #Different from '86
  for(i in 1:nrow(FP87)){
    if(FP87$DUM[i] == "Y" & !is.na(FP87$DUM[i])){
      FP87$MATEM87[i] <- FP87$METAL[i]
      FP87$MATEP87[i] <- FP87$mateband87[i]
      FP87$NMSEX87[i] <- FP87$SEXB[i]
    }
  }
  FP87 <- FP87[,c('MATEM87', 'NEST', 'MATEP87', 'NMSEX87','n87', 'CS87', 'ID87', 'HD87', 'REALBAND'),]
}else{
  FP87 <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c('MATEM87', 'NEST', 'MATEP87', 'N87',
                                                             'CS87', 'ID87', 'HD87', 'REALBAND'))
  FP87 <- FP87 %>% mutate_all(as.character)
}

FQ87 <- FP87[,c("REALBAND", "MATEM87", "MATEP87", "NMSEX87")]
FQ87 <- FQ87 %>% rename(BAND = REALBAND)

FR87 <- full_join(FQ87, FM87)
FR87$MATEP87[which(FR87$mateband87 == "UM")] <- FR87$mateband87[which(FR87$mateband87 == "UM")]
FR87 <- FR87[, !names(FR87) %in% "mateband87"]

FS87 <- full_join(DU87, FR87)
FS87 <- FS87[(!is.na(FS87$METAL)),]
FS87 <- FS87[, !names(FS87) %in% "NEST"]

FT87 <- full_join(DU87, FR87)
FT87 <- FT87[(is.na(FT87$METAL)), "BAND"] #******* I'm not sure if I should use NA here instead of "." but I'm going to until that proves dumb
if(nrow(FT87) != 0){
  FT87$YEAR <- "87"
  FT87$COMMENTS <- "Bird seen at nest, no banding record"
}else{
  FT87 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "YEAR", "COMMENTS"))
}

###
#It's H time!!!
###

HA87 <- BS87[(BS87$DRIVE != "NEST" | is.na(BS87$DRIVE)),]
HA87$AGEB <- NA; HA87$SEXB <- NA; HA87$DATEB <- NA; HA87$YEARB <- NA
for(i in 1:nrow(HA87)){
  if(HA87$FILE[i] == "BS"){
    HA87$AGEB[i] <- HA87$AGE[i]; HA87$SEXB[i] <- HA87$SEX[i] 
    HA87$DATEB[i] <- HA87$DATE[i]; HA87$YEARB[i] <- HA87$YEAR[i]
  }
}
HA87 <- HA87 %>% rename(aB87 = AGE, sB87 = SEX, Bc87= CUL, Bt87= TAR, Bm87=MASS, BP87=BP)
HA87$COUNT <- 1
HA87$BD87[which(HA87$COLONY == "TUT")] <- HA87$DRIVE[which(HA87$COLONY == "TUT")]
HA87$BD87[which(HA87$COLONY != "TUT" | is.na(HA87$COLONY))] <- HA87$COLONY[which(HA87$COLONY != "TUT"| is.na(HA87$COLONY))]
HA87 <- HA87[, !names(HA87) %in% c("BSTAT", "FILE", "DRIVE", "COLONY", "YEAR")]


HB87 <- HA87[which(HA87$METAL == "" | is.na(HA87$METAL)), c("BAND87"), drop = FALSE] #Changed from "."
HB87 <- HB87 %>% rename(BAND = BAND87)
if(nrow(HB87) != 0){
  HB87$COMMENTS <- "Bird was captured with plastic and released without metal"
  HB87$YEAR <- "87"
}else{
  HB87 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("BAND", "COMMENTS", "YEAR"))
  HB87 <- HB87 %>% mutate_all(as.character())
}

HC87 <- HA87[(HA87$METAL != "" & !is.na(HA87$METAL)),] #Changed from "."
HC87$Bc87 <- as.numeric(HC87$Bc87)
HC87$Bt87 <- as.numeric(HC87$Bt87)
HD87 <- group_by(HC87, METAL) %>% summarise(meanbc87 = mean_(HC87, Bc87), meanbt87 = mean_(HC87,Bt87),
                                            DATE = min(DATE, na.rm = T), COUNTsum = sum(COUNT, na.rm = T))
#Again originally from BS which is rbinded of bsc and nest files so I think it's ok to summarise here

HE87 <- full_join(HA87, HD87)
HE87 <- HE87[(!is.na(HE87$COUNTsum)), !names(HE87) %in% c("Bc87", "Bt87")]


HF87 <- HE87[,!names(HE87) %in% "COUNTsum"]
HF87 <- HF87 %>% group_by(METAL) %>% mutate(COUNT = sum(COUNT)) #%>% summarise_all(~last(.[which(!is.na(.) & (. != ""))]))
CheckReplicates <- Mistakes(x = HF87, groupby = "METAL", yeardf = BA87, CheckReplicates)
HF87 <- HF87[!(HF87$COUNT > 1),]

HG87 <- HF87 %>% rename(BC87 = meanbc87, BT87 = meanbt87)
HH87 <- HG87[(!is.na(HG87$NEWMETAL)), !names(HG87) %in% "METAL"] #changed from "."
HH87$DEL <- "Y"


HI87 <- HH87[,c("mr87","DEL")] %>% rename(METAL = mr87)
HJ87 <- HH87[,c("NEWMETAL", "DEL")] %>% rename(METAL = NEWMETAL)

HK87 <- bind_rows(HI87, HJ87)
HL87 <- full_join(FS87, HK87)
HL87 <- HL87[-which(HL87$DEL == "Y"),]
HL87$duma <- "1"

HM87 <- full_join(HG87, HK87)
HM87 <- HM87[-which(HM87$DEL == "Y"),]
HM87$dumb <- "1"
HM87 <- HM87 %>% rename(dumpr87 = PR87, webtag87 = WEBTAG, dbd87 = DATE, ageb87 = AGEB, sexb87 = SEXB,
                        dateb87 = DATEB, yearb87 = YEARB)

HN87 <- full_join(HL87, HM87, by = "METAL") %>% #was left join
  mutate(mr87 = coalesce(mr87.x, mr87.y),
         #webtag87 = coalesce(webtag87.x, webtag87.y), #different from '86
         #BAND87 = coalesce(BAND87.x, BAND87.y),
         dbd87 = coalesce(dbd87.x, dbd87.y),
         DEL = coalesce(DEL.x, DEL.y),
  ) %>%
  select(-mr87.x, -mr87.y, -dbd87.x, -dbd87.y, -DEL.x, -DEL.y)

HO87 <- HN87[(is.na(HN87$duma) & HN87$dumb == "1"),]
HP87 <- HH87
HP87$METAL <- HP87$mr87
HQ87 <- full_join(FS87, HP87, by = "METAL") %>% #***I think a right join here would be quicker to get the same outcome, but idk if that could cause issues later??
  mutate(mr87 = coalesce(mr87.x, mr87.y),
         WEBTAG = coalesce(WEBTAG.x, WEBTAG.y),
         #BAND87 = coalesce(BAND87.x, BAND87.y), #different from '86
         PR87 = coalesce(PR87.x, PR87.y),
         AGEB = coalesce(AGEB.x, AGEB.y),
         SEXB = coalesce(SEXB.x, SEXB.y),
         DATEB = coalesce(DATEB.x, DATEB.y),
         YEARB = coalesce(YEARB.x, YEARB.y)
  ) %>%
  select(-mr87.x, -mr87.y, -WEBTAG.x, -WEBTAG.y, -PR87.x, -PR87.y, -AGEB.x, -AGEB.y,
         -SEXB.x, -SEXB.y, -DATEB.x, -DATEB.y, -YEARB.x, -YEARB.y)

HQ87 <- HQ87[which(HQ87$DEL == "Y"), !names(HQ87) %in% "METAL"]
HR87 <- HQ87 %>% rename(dumpr87 = PR87, yearb87 = YEARB, dateb87 = DATEB,
                        ageb87 = AGEB, sexb87 = SEXB)
HR87$webtag87 <- HR87$WEBTAG
HR87$dbd87 <- HR87$DATE
HR87$METAL <- HR87$NEWMETAL
HR87 <- HR87[, !names(HR87) %in% c("WEBTAG", "DEL", "DATE")]

HS87 <- bind_rows(HN87, HR87)
HS87$RP87 <- NA
HS87$AGE <- NA
HS87$SEX <- NA
HS87$DATE <- NA
HS87$YEAR <- NA
HS87$BANDB <- NA
HS87$WEBTAGB <- NA
HS87$COMMENTS <- NA
#Takes a Hot Second: Do something different?

#***There's literally one instance for this with metal 105700584
#So maybe there's a place where it's assigned but I don't think so? So not sure if there's a better fix than
#just doing this which is a tad silly but better for other years later in case this happens
HS87$BAND[which(HS87$BAND == "")] <- NA

for(i in 1:nrow(HS87)){
  #RP
  if((!is.na(HS87$dumpr87[i]) & HS87$dumpr87[i] != "") & !is.na(HS87$PR87[i])){HS87$RP87[i] <- HS87$PR87[i]}else{
    HS87$RP87[i] <- HS87$dumpr87[i]
  }

  #Age
  if(!is.na(HS87$ageb87[i]) & !is.na(HS87$AGEB[i]) & HS87$ageb87[i] == HS87$AGEB[i]){HS87$AGE[i] <- HS87$AGEB[i]}
  if(!is.na(HS87$ageb87[i]) & !is.na(HS87$AGEB[i]) & HS87$ageb87[i] != HS87$AGEB[i]){
    HS87$AGE[i] <- HS87$AGEB[i]
    HS87$COMMENTS[i] <- "Age at banding does not agree with BBL and BSC"
  }
  if(!is.na(HS87$ageb87[i]) & is.na(HS87$AGEB[i]) ){HS87$AGE[i] <- HS87$ageb87[i]}
  if(is.na(HS87$ageb87[i]) & !is.na(HS87$AGEB[i]) ){HS87$AGE[i] <- HS87$AGEB[i]}

  #Sex
  if(!is.na(HS87$sexb87[i]) & !is.na(HS87$SEXB[i]) & HS87$sexb87[i] == HS87$SEXB[i]){HS87$SEX[i] <- HS87$SEXB[i]}
  if(!is.na(HS87$sexb87[i]) & !is.na(HS87$SEXB[i]) & HS87$sexb87[i] != HS87$SEXB[i]){
    HS87$SEX[i] <- HS87$SEXB[i]
    HS87$COMMENTS[i] <- "Sex at banding does not agree with BBL and BSC"
  }
  if(!is.na(HS87$sexb87[i]) & is.na(HS87$SEXB[i]) ){HS87$SEX[i] <- HS87$sexb87[i]}
  if(is.na(HS87$sexb87[i]) & !is.na(HS87$SEXB[i]) ){HS87$SEX[i] <- HS87$SEXB[i]}

  #DATE
  if(!is.na(HS87$dateb87[i]) & !is.na(HS87$DATEB[i]) & HS87$dateb87[i] == HS87$DATEB[i]){HS87$DATE[i] <- HS87$DATEB[i]}
  if(!is.na(HS87$dateb87[i]) & !is.na(HS87$DATEB[i]) & HS87$dateb87[i] != HS87$dateb87[i]){
    HS87$DATE[i] <- HS87$DATEB[i]
    HS87$COMMENTS[i] <- "Date at banding does not agree with BBL and BSC"
  }
  if(!is.na(HS87$dateb87[i]) & is.na(HS87$DATEB[i]) ){HS87$DATE[i] <- HS87$dateb87[i]}
  if(is.na(HS87$dateb87[i]) & !is.na(HS87$DATEB[i]) ){HS87$DATE[i] <- HS87$DATEB[i]}

  #Year
  if(!is.na(HS87$yearb87[i]) & !is.na(HS87$YEARB[i]) & HS87$yearb87[i] == HS87$YEARB[i]){HS87$YEAR[i] <- HS87$YEARB[i]}
  if(!is.na(HS87$yearb87[i]) & !is.na(HS87$YEARB[i]) & HS87$yearb87[i] != HS87$YEARB[i]){
    HS87$YEAR[i] <- HS87$YEARB[i]
    HS87$COMMENTS[i] <- "Year at banding does not agree with BBL and BSC"
  }
  if(!is.na(HS87$yearb87[i]) & is.na(HS87$YEARB[i]) ){HS87$YEAR[i] <- HS87$yearb87[i]}
  if(is.na(HS87$yearb87[i]) & !is.na(HS87$YEARB[i]) ){HS87$YEAR[i] <- HS87$YEARB[i]}

  #Band
  if(!is.na(HS87$BAND87[i]) & !is.na(HS87$BAND[i]) & HS87$BAND87[i] == HS87$BAND[i]){HS87$BANDB[i] <- HS87$BAND[i]}
  if(!is.na(HS87$BAND87[i]) & !is.na(HS87$BAND[i]) & HS87$BAND87[i] != HS87$BAND[i] & !is.na(HS87$RP87[i])){
    HS87$BANDB[i] <- HS87$BAND87[i]
  }
  if(!is.na(HS87$BAND87[i]) & !is.na(HS87$BAND[i]) & HS87$BAND87[i] != HS87$BAND[i] & is.na(HS87$RP87[i])){
    HS87$BANDB[i] <- HS87$BAND[i]
    HS87$COMMENTS[i] <- "Plastic bands do not agree for this recapture with original BSC"
  }
  if(!is.na(HS87$BAND87[i]) & is.na(HS87$BAND[i]) ){HS87$BANDB[i] <- HS87$BAND87[i]}
  if(is.na(HS87$BAND87[i]) & !is.na(HS87$BAND[i]) ){HS87$BANDB[i] <- HS87$BAND[i]}

  #Webtag
  if(!is.na(HS87$webtag87[i]) & !is.na(HS87$WEBTAG[i]) & HS87$webtag87[i] == HS87$WEBTAG[i]){HS87$WEBTAGB[i] <- HS87$WEBTAG[i]}
  if(!is.na(HS87$webtag87[i]) & !is.na(HS87$WEBTAG[i]) & HS87$webtag87[i] != HS87$WEBTAG[i]){
    HS87$WEBTAGB[i] <- HS87$WEBTAG[i]
    HS87$COMMENTS[i] <- "Webtag code has changed"
  }
  if(!is.na(HS87$webtag87[i]) & is.na(HS87$WEBTAG[i]) ){HS87$WEBTAGB[i] <- HS87$webtag87[i]}
  if(is.na(HS87$webtag87[i]) & !is.na(HS87$WEBTAG[i]) ){HS87$WEBTAGB[i] <- HS87$WEBTAG[i]}

  if(!is.na(HS87$PR87[i]) & is.na(HS87$RP87[i])){HS87$RP87[i] <- HS87$PR87[i]}
}

HS87 <- HS87[,!names(HS87) %in% c("NEWMETAL", "duma", "dumb", "DEL", "COUNT", "newmetal", "dumpr87",
                                  "PR87", "ageb87", "AGEB", "sexb87", "SEXB", "dateb87", "DATEB", "yearb87",
                                  "YEARB", "BAND87", "BAND", "webtag87", "WEBTAG" )]

HT87 <- HS87 %>% rename(AGEB = AGE, SEXB = SEX, DATEB = DATE, YEARB = YEAR, BAND = BANDB, WEBTAG = WEBTAGB,
                        PR87 = RP87)
HU87 <- HT87[,!names(HT87) %in% "COMMENTS"]
HV87 <- HT87[which(!is.na(HT87$COMMENTS)), c('COMMENTS', "METAL", 'BAND', 'WEBTAG')]

###
#The J's!!!! Egg Data!!!
###

JA87 <- EGG87[(EGG87$TAG != ""),] #This may need to be NA not "", check a different year
keep <- c("COUNT", "NEST", "EGG", 'TAGD', "STATE", "DATE", "TAG")
if(nrow(JA87) != 0){
  JA87$EGG <- JA87$EGGB
  JA87$TAGD <- JA87$DATE
  JA87$COUNT <- "1"
  JA87 <- JA87[,keep]
}else{
  JA87 <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), keep)
  JA87 <- JA87 %>% mutate_all(as.character)
}

JB87 <- EGG87[(EGG87$LENGTH != "" | EGG87$WIDTH != ""),]
keep <- c("NEST", "EGG", "LENGTH", "WIDTH")
if(nrow(JA87) != 0){
  JB87$EGG <- JB87$EGGA
  JB87 <- JB87[,keep]
}else{
  JB87 <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), keep)
  JB87 <- JB87 %>% mutate_all(as.character)
}

JC87 <- full_join(JA87, JB87)
JC87$WEBTAG <- JC87$TAG
JC87 <- JC87[which(JC87$COUNT == 1),]
JC87$D <- JC87$TAGD
JC87$L <- JC87$LENGTH
JC87$W <- JC87$WIDTH
JC87 <- JC87[,!names(JC87) %in% c("TAGD", "LENGTH", "WIDTH", "TAG")]

JC87 <- JC87 %>% mutate_at(vars(D, L, W, COUNT), as.numeric)

JD87 <- JC87 %>% group_by(WEBTAG, NEST, EGG) %>% summarise(MD = mean_(JC87, D), ML = mean_(JC87, L), 
                                                MW = mean_(JC87, W), C = sum(COUNT))
#***ughh not sure if I should include Mistakes() here, I don't think I will for now

JE87 <- JD87
JE87 <- JE87 %>% rename(WTD = MD, EGG1 = ML, EGGW = MW)
if(nrow(JE87) != 0){
  JE87$COUNT <- 1
}else{
  JE87 <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c(colnames(JE87), "COUNT"))
  JE87 <- JE87 %>% mutate_if(is.logical, as.character)
  JE87$COUNT <- as.numeric(JE87$COUNT)
}
JE87 <- JE87[, !names(JE87) %in% "C"]

JF87 <- JE87 %>% group_by(WEBTAG) %>% mutate(COUNT = sum(COUNT)) 
CheckReplicates <- Mistakes(x = JF87, groupby = "WEBTAG", yeardf = BA87, CheckReplicates)
JF87 <- JE87 %>% group_by(WEBTAG) %>% summarise(COUNT = sum(COUNT))

JG87 <- JF87[(JF87$COUNT > 1), "WEBTAG"]
if(nrow(JG87) != 0){
  JG87$DEL <- "Y"
}else{
  JG87 <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("WEBTAG", "DEL"))
  JG87 <- JG87 %>% mutate_all(as.character)
}

JH87 <- JA87 %>% rename(WEBTAG = TAG)
JI87 <- full_join(JG87, JH87)
JI87 <- JI87[-which(JI87$DEL == "Y"), !names(JI87) %in% "DEL"] 

JJ87 <- full_join(JG87, JH87)
JJ87 <- JJ87[which(JJ87$DEL == "Y"),c("WEBTAG", "NEST")]
if(nrow(JJ87) != 0){
  JJ87$YEAR <- "87"
  JJ87$COMMENTS <- "This webtag was put on 2 different eggs"
}else{
  JJ87 <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), keep)
  JJ87 <- JJ87 %>% mutate_all(as.character)
}

JK87 <- JI87 #This is all the remaining webtags without the duplicates
if(nrow(JK87) != 0){
  JK87$DUMA <- "Y"
}else{
  JK87 <- setNames(data.frame(matrix(ncol = (length(colnames(JK87)) + 1), nrow = 0)), c(colnames(JK87), "DUMA"))
  JK87 <- JK87 %>% mutate_all(as.character)
}

JK87$COUNT <- as.numeric(JK87$COUNT)

JL87 <- full_join(JK87, JE87)
JL87 <- JL87[which(JL87$DUMA == "Y"), !names(JL87) %in% c("DUMA", "COUNT", "WTD")] 

###
#SAS COMMENTS: Starts dealing with parent info for this years webtags;
#               but identifies webtags from prior years caught this year for the first time.
###

LA87 <- FM87
LA87$dum <- 1

LB87 <- LA87 %>% group_by(NEST) %>% summarise(dum = sum(dum))

LC87 <- NEST87

LD87 <- full_join(LC87, LB87)
LD87 <- LD87[which(LD87$dum > 0 & LD87$NEST != ""),]
LD87$PARENT1P <- NA
LD87$PARENT2P <- NA
for(i in 1:nrow(LD87)){
  #Some cleaning up could happen here: what happens if there's a band but no color, should it be AO2_ or just AO2?
  # band <- str_pad(LD87$BAND[i], 3, side = "right") 
  # color <- str_replace_na(LD87$C1[i], "")
  
  if(!is.na(LD87$BAND[i])){
    color <- str_replace_na(LD87$C1[i], "")
    LD87$PARENT1P[i] <- paste0(LD87$BAND[i], color)
  }
  
  if(!is.na(LD87$MATE[i])){
    color2 <- str_replace_na(LD87$C2[i], "")
    LD87$PARENT2P[i] <- paste0(LD87$MATE[i], color2)
  }
  
  # band2 <- str_pad((str_replace_na(LD87$MATE[i], " ")), 3, side = 'right')
  # color2 <- str_replace_na(LD87$C2[i], "")
}
LD87 <- LD87[,c("NEST", "PARENT1P", "PARENT2P")]

LE87 <- LD87[which(!is.na(LD87$PARENT1P)),] 
LE87$BAND <- LE87$PARENT1P
LE87 <- LE87[,c("NEST", "BAND")]

LF87 <- LD87[which(!is.na(LD87$PARENT2P)),]
LF87$BAND <- LF87$PARENT2P
LF87 <- LF87[,c("NEST", "BAND")]

LG87 <- DU87[which(!is.na(DU87$BAND) & DU87$BAND != ""), c("BAND", "METAL")] %>% rename(PARENTM = METAL)

LH87 <- full_join(LG87, LE87)
LH87 <- LH87[(!is.na(LH87$NEST)),]
LH87$PARENTM[which(LH87$BAND == "UM")] <- "UM"
LH87$PARENTM[which(LH87$PARENTM == "")] <- "banded"
LH87 <- LH87 %>% rename(PARENT1M = PARENTM, PARENT1P = BAND)

LI87 <- full_join(LG87, LF87)
LI87 <- LI87[(!is.na(LI87$NEST)),]
LI87$PARENTM[which(LI87$BAND == "UM")] <- "UM"
LI87$PARENTM[which(LI87$PARENTM == "")] <- "banded"
LI87 <- LI87 %>% rename(PARENT2M = PARENTM, PARENT2P = BAND)

LJ87 <- full_join(LH87, LI87)
LK87 <- full_join(JL87, LJ87)
LK87 <- LK87[-which(is.na(LK87$WEBTAG)),]
if(nrow(LK87) !=0){LK87$KEEP <- "Y"}else{
  LK87 <- setNames(data.frame(matrix(ncol = (length(colnames(LK87)) +1), nrow = 0)), c(colnames(LK87), "KEEP"))
  LK87 <- LK87 %>% mutate_all(as.character)
}
keep <- c("WEBTAG", "NEST", "STATE", "EGG", "TAGD", "EGG1", "EGGW","PARENT1M", "PARENT1P", 'PARENT2M', 'PARENT2P', 'KEEP')
LK87 <- LK87[,keep]

LL87 <- LK87
if(nrow(LL87) !=0){LL87$YEAR <- "87"}else{
  LL87 <- setNames(data.frame(matrix(ncol = (length(colnames(LL87)) +1), nrow = 0)), c(colnames(LL87), "YEAR"))
  LL87 <- LL87 %>% mutate_all(as.character)
}

LM87 <- bind_rows(LL87, AC8)

NA87 <- HG87[,c("METAL", "WEBTAG", "AGEB", "YEARB")]

NB87 <- full_join(NA87, LK87)
NB87 <- NB87[which(NB87$WEBTAG != "" & NB87$METAL != "" & !is.na(NB87$WEBTAG), !is.na(NB87$METAL)),] 
NB87 <- NB87[(!is.na(NB87$WEBTAG) & !is.na(NB87$METAL)),]  #!is.na here?
if(nrow(NB87) != 0){
  NB87$YEAR <- (as.numeric(NB87$YEARB) - 1900)
  NB87 <- NB87[which(NB87$YEAR == "87"), !names(NB87) %in% "YEAR"]
}else{
  NB87 <- setNames(data.frame(matrix(nrow = 0, ncol = length(colnames(NB87)) )), colnames(NB87))
  NB87 <- NB87 %>% mutate_all(as.character)
}

NC87 <- NB87[which(NB87$KEEP == "Y"),]
ND87 <- NB87[which(NB87$AGEB == "L" & is.na(NB87$KEEP)), c("METAL", "WEBTAG", "AGEB", "YEARB")]

NE87 <- NB87[which(NB87$AGEB == "SY"), c("METAL", "WEBTAG", "AGEB", "YEARB")]
if(nrow(NE87) != 0){
  NE87$DUMA <- "Y"
}else{
  NE87 <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("METAL", "WEBTAG", "AGEB", "YEARB", "DUMA"))
  NE87 <- NE87 %>% mutate_all(as.character)
}

NF87 <- NB87[which(NB87$AGEB == "ASY"), c("METAL", "WEBTAG", "AGEB", "YEARB")]
if(nrow(NF87) != 0){
  NF87$DUMA <- "Y"
}else{
  NF87 <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("METAL", "WEBTAG", "AGEB", "YEARB", "DUMB"))
  NF87 <- NF87 %>% mutate_all(as.character)
}

NG87 <- full_join(NE87, AB87) #Different from 86
NG87 <- NG87[which(NG87$DUMA == "Y"), !names(NG87) %in% "DUMA"]

NH87 <- full_join(NF87, AC87)#Diff from 86
NH87 <- NH87[which(NH87$DUMB == "Y"), !names(NH87) %in% "DUMB"]

###
#Sas COMMENTS: each row must have the NH column filled in
###

NI87 <- bind_rows(NC87, ND87, NG87, NH87) #Diff from 86
if(nrow(NI87) != 0){
  NI87$NEST[which(is.na(NI87$NEST))] <- "noinfo"
}
NI87 <- NI87 %>% rename(NHID87 = NEST, EGGL87 = EGG1, EGGW87 = EGGW, STATE87 = STATE, PILS87 = EGG, TAGD87 = TAGD,
                        PARENT1M87 = PARENT1M, PARENT1P87 = PARENT1P, PARENT2M87 = PARENT2M, PARENT2P87 = PARENT2P)
NI87 <- NI87[,!names(NI87) %in% c("AGEB", "YEARB", "KEEP", "YEAR")]

###
#After lunch start here b/c NJ is just wrong lol
###

NJ87 <- full_join(HU87, NI87)
NJ87$COMMENTS <- NA
#See COMMENTS # 8 where does NHID come from why I'm gonna ignore it
NJ87$NHID <- NA
NJ87[c("EGGL", "EGGW", "STATE", "PILS", "TAGD", "PARENT1M", "PARENT2M")] <- NA
for(i in 1:nrow(NJ87)){
  if(!is.na(NJ87$NHID[i]) & !is.na(NJ87$NHID87[i])){NJ87$COMMENTS[i] <- "Double use of applied webtag"}
  if(is.na(NJ87$NHID[i]) & !is.na(NJ87$NHID87[i])){
    NJ87$EGGL[i] <- NJ87$EGGL87[i]
    NJ87$EGGW[i] <- NJ87$EGGW87[i]
    NJ87$NHID[i] <- NJ87$NHID87[i]
    NJ87$STATE[i] <- NJ87$STATE87[i]
    NJ87$PILS[i] <- NJ87$PILS87[i]
    NJ87$TAGD[i] <- NJ87$TAGD87[i]
    NJ87$PARENT1M[i] <- NJ87$PARENT1M87[i]
    NJ87$PARENT2M[i] <- NJ87$PARENT2M87[i]
  }
}
# NJ87$TAGD <- "   " #Different from 

NK87 <- NJ87[which(!is.na(NJ87$COMMENTS)),]
if(nrow(NK87) != 0){
  NK87$YEAR <- "87"
  NK87 <- NK87[,c("METAL", "BAND", "COMMENTS", "YEAR", "WEBTAG")]
}else{
  NK87 <- setNames(data.frame(matrix(nrow = 0, ncol = 5)), c("METAL", "BAND", "COMMENTS", "YEAR", "WEBTAG"))
  NK87 <- NK87 %>% mutate_all(as.character)
}

NL87 <- NJ87[,!names(NJ87) %in% c("COMMENTS", "EGGL87", "EGGW87", "NHID87", "STATE87", "PILS87", "TAGD87",
                                  "PARENT1M87", "PARENT2M87", "PARENT1P87", "PARENT2P87")]
#NL87 <- NL87[-which(!is.na(NL87$BBLYEAR) & NL87$BBLYEAR < 1967),] Diff from 86

#ERRORS87 <- bind_rows(BW87, BX87, DB87, DV87, FT87, HB87, HV87, JJ87, NK87) Diff from 86

#88 Different it has SPRING data
#90 Different, it includes winter stuff

#Diff from 86
TOW87A <- TOWER[(TOWER$YEAR == "1987"),] %>% rename(REALBAND = BAND)

TOW87B <- TOW87A %>% rename(BAND = MATE)
TOW87B <- TOW87B[-which(is.na(TOW87B$BAND) | TOW87B$BAND == "UM" | TOW87B$BAND == "NO"),]

TOW87C <- full_join(TOW87B, NL87)
TOW87C <- TOW87C[-which(is.na(TOW87C$BRSIZE)),]
TOW87C <- TOW87C %>% rename(DTO87 = DATE, TMATEM87 = METAL, TMATEP87 = BAND, 
                                                          TMSEX87 = SEXB, TBS87 = BRSIZE)
TOW87C <- TOW87C[,c("REALBAND", "DTO87", "TMATEM87", "TMATEP87", "TMSEX87", "TBS87")]

TOW87D <- TOW87C[(is.na(TOW87C$TMATEM87)), !names(TOW87C) %in% c('TMATEM87', 'TMSEX87')] %>% 
              rename(PR87 = TMATEP87)

TOW87E <- NL87[(!is.na(NL87$PR87)),]

TOW87F <- full_join(TOW87E, TOW87D)
TOW87F <- TOW87F %>% rename(TMATEM87 = METAL, TMATEP87 = BAND, TMSEX87 = SEXB)
TOW87F <- TOW87F[-which(is.na(TOW87F$REALBAND) | is.na(TOW87F$TMATEM87)), c('REALBAND', 'DTO87', 'TBS87', 'TMATEM87', 'TMATEP87', 'TMSEX87')]

TOW87G <- bind_rows(TOW87C, TOW87F) 
TOW87G <- TOW87G %>% rename(BAND = REALBAND)

TOW87H <- TOWER[which(TOWER$YEAR == "1987" & (is.na(TOWER$MATE) | TOWER$MATE == "UM" | TOWER$MATE == "NO")), !names(TOWER) %in% "YEAR"]
TOW87H <- TOW87H %>% rename(DTO87 = DATE, TMATEP87 = MATE, TBS87 = BRSIZE)
TOW87H$TMATEM87 <- as.character(NA)


TOW87I <- full_join(TOW87H, TOW87G) %>% full_join(.,NL87)
TOW87I <- TOW87I[-which(!is.na(TOW87I$METAL)), c("BAND","DTO87", "TMATEM87", "TMATEP87", "TBS87")] %>% rename(PR87 = BAND)

TOW87J <- NL87[(!is.na(NL87$PR87)),] #same as tow87e?

TOW87K <- full_join(TOW87J, TOW87I)
#***Double check below when there's a year that fits it b/c I'm not sure the which statement is correct.
TOW87K <- TOW87K[-which(is.na(TOW87K$METAL) & (is.na(TOW87K$DTO87) & is.na(TOW87K$TMATEP87))), c("BAND", "DTO87", "TMATEM87", "TMATEP87", "TBS87")]

TOW87L <- bind_rows(TOW87G, TOW87H, TOW87K)
TOW87L$COUNT <- 1

#find duplicate bands from tow87L
TOW87M <- TOW87L %>% group_by(BAND) %>% mutate(COUNT = sum(COUNT))
CheckReplicates <- Mistakes(x = TOW87M, groupby = "BAND", yeardf = BA87, CheckReplicates)
TOW87M <- TOW87M[!(TOW87M$COUNT > 1), !names(TOW87M) %in% "COUNT"]

NBR87A <- NBBAND[(NBBAND$YEAR == "1987"),] %>% rename(REALBAND = BAND, NBC87 = NBCOL)

NBR87B <- NBR87A %>% rename(BAND = MATE)
NBR87B <- NBR87B[-which(is.na(NBR87B$BAND) | NBR87B$BAND == "UM" | NBR87B$BAND == "NO"),]

NBR87C <- full_join(NBR87B, NL87) 
NBR87C <- NBR87C[-which(is.na(NBR87C$REALBAND)), c("REALBAND", "DATE", "METAL", "BAND", "SEXB", "NBC87")] %>% 
              rename(NBD87 = DATE, NBMATEM87 = METAL, NBMATEP87 = BAND, NBMSEX87 = SEXB)
NBR87D <- NBR87C[which(is.na(NBR87C$NBMATEM87)), !names(NBR87C) %in% c("NBMATEM87", "NBMSEX87")] %>% rename(PR87 = NBMATEP87)

NBR87E <- NL87[(!is.na(NL87$PR87)),]

NBR87F <- full_join(NBR87E, NBR87D)
NBR87F <- NBR87F[-which(is.na(NBR87F$REALBAND) | is.na(NBR87F$METAL)), 
                 c("REALBAND", "METAL", "NBD87", "BAND", "SEXB", "NBC87")] %>% rename(NBMATEM87 = METAL, 
                                                                             NBMATEP87 = BAND, NBMSEX87 = SEXB)
NBR87G <- bind_rows(NBR87C, NBR87F) %>% rename(BAND = REALBAND)

NBR87H <- NBBAND[which(NBBAND$YEAR == "1987" & (is.na(NBBAND$MATE) | NBBAND$MATE == "UM" | 
                                                 NBBAND$MATE == "NO")), !names(NBBAND) %in% "YEAR"]
NBR87H <- NBR87H %>% rename(NBD87 = DATE, NBMATEP87 = MATE, NBC87 = NBCOL)
NBR87H$NBMATEM87 <- as.character(NA)

NBR87I <- full_join(NBR87G, NBR87H) %>% full_join(., NL87)
NBR87I <- NBR87I[-which(!is.na(NBR87I$METAL)),c("BAND", "NBC87","NBD87", "NBMATEM87", "NBMATEP87")] %>% 
              rename(PR87 = BAND)
NBR87J <- NL87[(!is.na(NL87$PR87)),] #same as something we did above in like tower or something 

NBR87K <- full_join(NBR87J, NBR87I) 
NBR87K <- NBR87K[!(is.na(NBR87K$METAL) | (is.na(NBR87K$NBD87) & is.na(NBR87K$NBMATEP87)) ), 
                 c("BAND", "NBD87", "NBMATEM87", "NBMATEP87", "NBC87")] #*** Check Operators here again they confuse me a lot lol
NBR87L <- bind_rows(NBR87G, NBR87H, NBR87K)
NBR87L$COUNT <- 1

#find duplicate bands from nbr87L
NBR87M <- NBR87L %>% group_by(BAND) %>% mutate(COUNT = sum(COUNT))
CheckReplicates <- Mistakes(x = NBR87M, groupby = "BAND", yeardf = BA87, CheckReplicates)
NBR87M <- NBR87M[!(NBR87M$COUNT > 1), !names(NBR87M) %in% "COUNT"]

NN87 <- full_join(NL87, TOW87M) %>% full_join(., NBR87M)
NN87 <- NN87[!(is.na(NN87$METAL)),]

OA87 <- NN87[!(is.na(NN87$MATEM87) & is.na(NN87$NBMATEM87) & is.na(NN87$TMATEM87) & is.na(NN87$MATEP87) &
                 is.na(NN87$NBMATEP87) & is.na(NN87$TMATEP87)),]
OA87$COMMENTS <- as.character(NA)

for(i in 1:nrow(OA87)){
  if( (!is.na(OA87$MATEP87[i]) & !is.na(OA87$TMATEP87[i]) & (OA87$MATEP87[i] != OA87$TMATEP87[i])) |
      (!is.na(OA87$MATEP87[i]) & !is.na(OA87$NBMATEP87[i]) & (OA87$MATEP87[i] != OA87$NBMATEP87[i])) |
      (!is.na(OA87$TMATEP87[i]) & !is.na(OA87$NBMATEP87[i]) & (OA87$TMATEP87[i] != OA87$NBMATEP87[i]))
    ){
    OA87$COMMENTS[i] <- "Mate changed within year"
  }
}
keep <- c("METAL", "BAND", "MATEM87", "MATEP87", "NBD87", "NBMATEM87", "NBMATEP87", "DTO87", 
          "TMATEM87", "TMATEP87", "TBS87", "COMMENTS")
OA87 <- OA87[!(is.na(OA87$COMMENTS)), keep]

OB87 <- NN87[!(is.na(NN87$NMSEX87) & is.na(NN87$TMSEX87) & is.na(NN87$NBMSEX87)),]
OB87$COMMENTS <- as.character(NA)
OB87$COMMENTS[(OB87$NMSEX87 == OB87$SEXB) | (OB87$TMSEX87 == OB87$SEXB)| 
              (OB87$NBMSEX87 == OB87$SEXB)] <- "Same sex pair" #hell yeah gay bird rights
keep <- c("METAL", "BAND", "SEXB", "MATEM87", "MATEP87", "NBD87", "NBMATEM87", "NBMATEP87", "NBMSEX87", 
          "DTO87", "TMATEM87", "TMATEP87", "TBS87", "TMSEX87", "COMMENTS")
OB87 <- OB87[!is.na(OB87$COMMENTS), keep]

OC87 <- NN87
OC87$COMMENTS <- as.character(NA)
OC87$COMMENTS[((OC87$BBLAGE == "L" & OC87$BBLYEAR == "1986" & !is.na(OC87$n87))|
              (OC87$BBLAGE == "L" & OC87$BBLYEAR == "1986" & OC87$TBS87 > 0))] <- "Breeding SY bird"
OC87 <- OC87[!is.na(OC87$COMMENTS), keep] #Columns saved are the same as keep above so not changing it

OD87 <- full_join(NL87, NBR87M)
OD87$COMMENTS <- as.character(NA)
OD87$COMMENTS[is.na(OD87$METAL)] <- "Bird seen NB, no banding record"
OD87 <- OD87[!is.na(OD87$COMMENTS), c("BAND", "SEXB", "COMMENTS", "NBD87", "NBMATEM87", "NBMATEP87")]

OE87 <- full_join(NL87, TOW87M)
OE87$COMMENTS <- as.character(NA)
OE87$COMMENTS[is.na(OE87$METAL)] <- "Bird seen tower, no banding record"
OE87 <- OE87[!is.na(OE87$COMMENTS),c("BAND", "SEXB", "COMMENTS", "DTO87", "TMATEM87", "TMATEP87", "TBS87")]

ERRORS87 <- bind_rows(BW87, BX87, DV87, FT87, HB87, HB87, JJ87, NK87, OA87, OB87, OC87, OD87, OE87)

#Starting in 96 there's a P section

