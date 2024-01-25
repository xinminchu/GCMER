###############
# Read in data
###############

# Change data directory as needed
data.dir <- "~/Documents/WORK/GitHub/Data-Analytics-Lab-Prof.Degras/EntityResolution/DataResource/" # for Prof.

data.dir <- "D:/GitHub/Data-Analytics-Lab-Prof.Degras/EntityResolution/DataResource/" # for Xinmin
data <- read.csv(file.path(data.dir, "affiliationstrings_ids.csv"))

data.dir <- "D:/GitHub/Data-Analytics-Lab-Prof.Degras/EntityResolution/MoreCode/data analysis/affiliation/aff_modification/" # for Xinmin
data <- read.csv(file.path(data.dir, "new.clean_affiliations.csv"))

#####################
# Data preprocessing
#####################

# Idea: one approach to calculating dissimilarities between the records
# is to directly with with the raw records as single strings. 
# This is very fast but will likely produce poor results in ER 
# because all different fields in the record are lumped together. 
# In addition, these fields (company name, street address, state, country, 
# zip code, ...) may be entered in different orders in the records.  

# It is more time consuming but will also result in higher clustering accuracy
# to detect and isolate the fields of each record. For example the first 
# record of the data would be handled like this: 

# Raw entry
# , IBM Almaden Research Center, 650 Harry Road, CA 95120, San Jose, USA

# Processed
# Name 1: IBM Almaden Research Center
# Name 2: 
# Name 3:
# Street: 650 Harry Road
# City: San Jose
# State: CA 
# Zip code: 95120
# Country: USA

# How to do this? 
# Rough idea: 
# 1) Split each record on commas (",") to separate character strings 
# --> list of character vectors. Convert the result to a csv or Excel file. 
# The file should have columns names "ID", "Name1", "Name2", ... "Country"
# The file also has a last column "DONE" that indicates if a row has been 
# entirely preprocessed. Write "y" if that's the case and leave it empty 
# otherwise. See file "affiliations_clean.csv"

# 2) Open the file in Excel. 

# 3) Split cells of the file whenever needed. 
# For example, split the cell "ca 95134" in 2 cells "ca" (State) and "95134" (Zipcode)).

# 4) Rearrange the fields/strings/cells of each row in the right order 
# to match the column type. You can do some of this in an automated way
# according to rules, but you will have to do quite a bit of it by hand too. 

# I have created from the data three files that can be useful to classify the 
# fields: "keywords.txt" (for company names), "cities.txt", "states.txt", 
# and "countries.txt".

# Here are examples of rules: 
# * If a string contains a keyword such as "ibm" or "university", 
# then it's a company name (1, 2, or 3). 
# * If a string is a word in "cities.txt", then it's a city. If a string 
# contains a word in "cities.txt" but also other characters, then it's 
# probably a company name (e.g., "Université Paris Sud", "UMass Boston").
# * Same thing for states and countries. 
# * If a string has only numbers, it's probably a zip code. 
# * The company name is almost always in first position; in rare cases, 
# the string in first position is not a company name. You can check for that. 

# Basically, to do step 4, you may have to go back and forth between R 
# (to classify and rearrange things automatically according to rules) 
# and Excel (or any convenient table editor) to rearrange things by hand.  

# Below I start organizing the file "clean_affiliations.csv". 
# You will have to do the rest. I hope it does not take too much time! 
# try to do a little (or a lot) every day. 




#######
# Code 
#######


## Convert the strings to lower case
# x <- tolower(data[,2])

## Split strings on commas (",")
fields <- strsplit(data[,2], ",", fixed = TRUE)

## Remove empty strings
fields <- lapply(fields, function(x) x[nchar(x) > 0])

## Trim white spaces
fields <- lapply(fields, trimws)



## Get the number of strings/fields for each row of the database 
len <- sapply(fields, length)
table(len)
# len
# 1   2   3   4   5   6   7   8   9  10  12 
# 394 331 768 541 148  44  23   5   4   1   1 


# A few rows have far too many strings. 
## Clean them up manually by removing unwanted strings 
## or split them in case of double affiliation
idx <- which(len > 7)
fields[idx]
idx2 <- idx[c(3,6,8,9)] # double affiliation
idx1 <- setdiff(idx, idx2) # single affiliation
keep1 <- list(1:5, 1:7,  1:6, 1:5, 1:5, 1:6, 1:4)
for (i in 1:length(idx1)) 
  fields[[idx1[i]]] <- fields[[idx1[i]]][keep1[[i]]]

## Handle double affiliations	
keep2 <- list(1:5, 1:4, 1:4, 1:4)
n <- length(fields)
nrep <- rep(1, n)
nrep[idx2] <- 2
idx <- rep(1:n, nrep)
id <- data[idx,1] # expand id vector to accommodate splits
fields <- fields[idx] # same for list of fields
double <- data[idx2,1]
first <- match(double, id)
for (i in 1:length(idx2)) {
  j <- first[[i]]
  fields[[j+1]] <- fields[[j]][-keep2[[i]]]
  fields[[j]] <- fields[[j]][keep2[[i]]]
}

# Note: some rows show a double affiliation --> split tem difficult

#######################################################
## second round (after manual modification)
fields <- cleanaff
#######################################################


## Recalculate numbers of strings per row
len <- sapply(fields, length)

## Column names for data matrix
cnames <- c("Name1", "Name2", "Name3", "Street1", "Street2",
            "City", "State", "Zipcode", "Country")
ncols <- length(cnames)

## Add NA fields to rows with less than 9 strings
n <- length(fields)
idx <- which(len < ncols)
nadd <- ncols - len
for (i in 1:n) 
  fields[[i]] <- c(fields[[i]], rep(NA, nadd[i]))


## Create the data table in R (character matrix)
fields <- matrix(unlist(fields), n, ncols, byrow = TRUE)
colnames(fields) <- cnames

## Replace empty fields by NA
fields[nchar(fields) == 0] <- NA

## Import city, state, and country names (curated manually)
file.dir <- "~/Documents/WORK/GitHub/Data-Analytics-Lab-Prof.Degras/EntityResolution/MoreCode/data analysis/affiliation/" # for Prof.

file.dir <- "D:/GitHub/Data-Analytics-Lab-Prof.Degras/EntityResolution/MoreCode/data analysis/affiliation/" # for Xinmin
file.dir <- "D:/GitHub/Data-Analytics-Lab-Prof.Degras/EntityResolution/MoreCode/data analysis/affiliation/aff_modification/" # for Xinmin's new dictionaries


streets <- readLines(paste0(file.dir, "streets.txt"))
cities <- readLines(paste0(file.dir, "cities.txt"))
states <- readLines(paste0(file.dir, "states.txt"))
countries <- readLines(paste0(file.dir, "countries.txt"))

## When adding new entries to the .txt files, 
## uncomment the lines below and run them
# streets <- sort(unique(tolower(streets)))
# cities <- sort(unique(tolower(cities)))
# states <- sort(unique(tolower(states)))
# countries <- sort(unique(tolower(countries)))
# write(streets, paste0(file.dir, "streets.txt"))
# write(cities, paste0(file.dir, "cities.txt"))
# write(states, paste0(file.dir, "states.txt"))
# write(countries, paste0(file.dir, "countries.txt"))


## Put street information in the right column
for (j in 1:ncols) {
  if (cnames[j] %in% c("Street1", "Street2")) next
  ## Indices of rows whose column j is a city name 
  idx <- sapply(streets, FUN = "grep", x = tolower(fields[,j]))
  idx <- unlist(idx, use.names = FALSE)
  ## For these rows:
  if (length(idx) > 0) {
    ## Swap column j and column "Street1" if "Street1" is NA
    idx1 <- idx[is.na(fields[idx,"Street1"])]
    fields[idx1, "Street1"] <- fields[idx1,j]
    fields[idx1,j] <- NA
    idx <- setdiff(idx, idx1)
    ## Swap column j and column "Street2" if "Street2" is NA 
    ## but not "Street1"
    idx2 <- idx[is.na(fields[idx,"Street2"])]
    fields[idx2, "Street2"] <- fields[idx2,j]
    fields[idx2,j] <- NA
    idx <- setdiff(idx, idx2)
    ## Swap column j and column "Street1" 
    ## if both "Street1" and "Street2" are not NA 
    if (length(idx) > 0) {
      tmp <- fields[idx, "Street1"]
      fields[idx, "Street1"] <- fields[idx,j]
      fields[idx,j] <- tmp
    }
  }
}

## Put city information in the right column
for (j in 1:ncols) {
  if (cnames[j] == "City") next
  ## Indices of rows whose column j is a city name 
  idx <- sapply(cities, FUN = "==", x = tolower(fields[,j]))
  idx <- which(apply(idx, 1, any))
  ## Swap column j and column "City" for these rows
  if (length(idx) > 0) {
    tmp <- fields[idx, "City"]
    fields[idx, "City"] <- fields[idx,j]
    fields[idx,j] <- tmp
  }
}

## Put state information in the right column
for (j in 1:ncols) {
  if (cnames[j] == "State") next
  ## Indices of rows whose column j is a state name 
  idx <- sapply(states, FUN = "==", x = tolower(fields[,j]))
  idx <- which(apply(idx, 1, any))
  ## Swap column j and column "Country" for these rows
  if (length(idx) > 0) {
    tmp <- fields[idx, "State"]
    fields[idx, "State"] <- fields[idx,j]
    fields[idx,j] <- tmp
  }
}

## Put country information in the right column
for (j in 1:ncols) {
  if (cnames[j] == "Country") next
  ## Indices of rows whose column j is a country name 
  idx <- sapply(countries, FUN = "==", x = tolower(fields[,j]))
  idx <- which(apply(idx, 1, any))
  ## Swap column j and column "Country" for these rows
  if (length(idx) > 0) {
    tmp <- fields[idx, "Country"]
    fields[idx, "Country"] <- fields[idx,j]
    fields[idx,j] <- tmp
  }
}

## Make sure that if available, the name and street information 
## are in first position.
idx <- is.na(fields[,"Name1"])
fields[idx,"Name1"] <- fields[idx,"Name2"]
fields[idx,"Name2"] <- NA
idx <- is.na(fields[,"Name2"])
fields[idx,"Name2"] <- fields[idx,"Name3"]
fields[idx,"Name3"] <- NA
idx <- is.na(fields[,"Street1"])
fields[idx,"Street1"] <- fields[idx,"Street2"]
fields[idx,"Street2"] <- NA



## Write out the data table to a csv file
fields[is.na(fields)] <- "" # replace NA fields by "" to facilitate reading

nrep <- rep(1, n)
write.table(x = cbind(ID = rep(data[,1], nrep), fields), 
            file = file.path(file.dir, "clean_affiliations.csv"), sep = ",", 
            quote = FALSE, row.names = FALSE)


# save for second round
write.table(x = cbind(ID = rep(data[,1], nrep), fields), 
            file = file.path(file.dir, "clean_affiliations2.csv"), sep = ",", 
            quote = FALSE, row.names = FALSE)

# After that, you need to go through the file line by line
# and make any necessary modification by hand

cleanaff <- cbind(ID = rep(data[,1], nrep), fields)


## Manually solve the mess of Name (Name2 and 3) with Streets

#################################
x <- cleanaff[,2] # Name1
out <- table(x) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))
out

key1 <- as.vector(out[grepl("[0-9]", out[,1]), 1])
key1 #modified ID = 7761, 9492, 9493, 9495

id <- cleanaff[fields[, "Name1"] == key1[11], "ID"]

cleanaff[cleanaff[,"ID"] == id, ]

tmp <- cleanaff[cleanaff[,"ID"] == id, "Name1"]

cleanaff[cleanaff[,"ID"] == id, "State"] <- cleanaff[cleanaff[,"ID"] == id, "Street2"]
cleanaff[cleanaff[,"ID"] == id, "Street2"] <- cleanaff[cleanaff[,"ID"] == id, "Name1"]
cleanaff[cleanaff[,"ID"] == id, "Name1"] <- cleanaff[cleanaff[,"ID"] == id, "Name2"]
cleanaff[cleanaff[,"ID"] == id, "Name2"] <- ""

#################################
x <- cleanaff[,3] # Name2
out <- table(x) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))
out


key2 <- as.vector(out[grepl("[0-9]", out[,1]), 1])
key2  
cleanaff[fields[, "Name2"] %in% key2, "ID"]
#[1] "7927" "9264" "7737" "7861" "9383" "9792" "7082" "9251" "8614" "7762" "7763" "7721" "7786" "7345" "8746" "7840" "8798"
#[18] "7849" "9338" "6904" "9429" "9341" "8765" "7793" "6510" "6607" "6856" "7934" "7413" "6551" "8652" "7876" "9433" "7131"
#[35] "7802" "8609" "7801" "9382" "9494" "9434" "9368" "8872" "6888" "7673" "8649" "6745" "9384" "8611" "7712" "7860" "7023"
#[52] "7194" "7674" "7675" "7847" "8047" "7724" "6859" "9261" "9266" "6902"

i <- 1

i <- i + 1 # run from here to the next
id <- cleanaff[fields[, "Name2"] == key2[i], "ID"]
i;id
idx <- id
#idx <- id[1]
#idx <- id[2]
cleanaff[cleanaff[,"ID"] == idx, ]

cleanaff[cleanaff[,"ID"] == idx, "Country"] <- "USA"
cleanaff[cleanaff[,"ID"] == idx, "Zipcode"] <- "98052-6399"
cleanaff[cleanaff[,"ID"] == idx, "Name2"] <- ""

cleanaff[cleanaff[,"ID"] == idx, "State"] <- "WA"



cleanaff[cleanaff[,"ID"] == idx, "Street2"] <- cleanaff[cleanaff[,"ID"] == idx, "Name2"]

cleanaff[cleanaff[,"ID"] == idx, "Name3"] <- ""

cleanaff[cleanaff[,"ID"] == idx, "Street1"] <- ""
cleanaff[cleanaff[,"ID"] == idx, "Street2"] <- ""

cleanaff[cleanaff[,"ID"] == idx, "City"] <- "Meylan"

cleanaff[cleanaff[,"ID"] == idx, "Name2"] <- cleanaff[cleanaff[,"ID"] == idx, "Name3"]


#################################
x <- cleanaff[,3] # Name3
out <- table(x) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))
out


key3 <- as.vector(out[grepl("[0-9]", out[,1]), 1])
key3  

cleanaff[fields[, "Name3"] %in% key3, "ID"]
#[1] "7761" "7730" "7710"

idx <- 7710 # change the id number one by one
cleanaff[cleanaff[,"ID"] == idx, ]

cleanaff[cleanaff[,"ID"] == idx, "Country"] <- "USA"
cleanaff[cleanaff[,"ID"] == idx, "Zipcode"] <- "07932"
cleanaff[cleanaff[,"ID"] == idx, "Name2"] <- ""

cleanaff[cleanaff[,"ID"] == idx, "State"] <- "NJ"

cleanaff[cleanaff[,"ID"] == idx, "Street2"] <- cleanaff[cleanaff[,"ID"] == idx, "Name3"]
cleanaff[cleanaff[,"ID"] == idx, "Name3"] <- ""


## Solve the city/state/country in Name2

# read and add new cities/states/countries
streets <- cleanaff[, "Street1"]
## Remove empty strings
streets <- lapply(streets, function(x) x[nchar(x) > 0])
streets <- lapply(streets, trimws)
len <- sapply(streets, length)
idx <- which(len > 0)
streets[idx]

item.check <- function(data, item){
  dat <- data[, item]
  ## Remove empty strings
  dat <- lapply(dat, function(x) x[nchar(x) > 0])
  dat <- lapply(dat, trimws)
  dat <- unique(dat)
  len <- sapply(dat, length)
  idx <- which(len > 0)
  dat[idx]
}


#cnames <- c("Name1", "Name2", "Name3", "Street1", "Street2",
#            "City", "State", "Zipcode", "Country")

street1 <- item.check(cleanaff, "Street1")
street2 <- item.check(cleanaff, "Street2")
street1 <- item.check(cleanaff, "Street1")
city <- item.check(cleanaff, "City")
state <- item.check(cleanaff, "State")
zipcode <- item.check(cleanaff, "Zipcode")
country <- item.check(cleanaff, "Country")

streets <- sort(unique(c(tolower(street1), tolower(street2))))
cities <- sort(unique(tolower(city)))
states <- sort(unique(tolower(state)))
countries <- sort(unique(tolower(country)))

file.dir <- "D:/Github/Data-Analytics-Lab-Prof.Degras/EntityResolution/MoreCode/data analysis/"
write(streets, paste0(file.dir, "streets.txt"))
write(cities, paste0(file.dir, "cities.txt"))
write(states, paste0(file.dir, "states.txt"))
write(countries, paste0(file.dir, "countries.txt"))


x <- cleanaff[,3] # Name2
out <- table(x) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))
out

x <- tolower(out[,1])

## Split strings on commas (",")
x.words <- strsplit(x, " ", fixed = TRUE)

## Trim white spaces
x.words <- lapply(x.words, trimws)

## Get the number of strings/fields for each row of the database 
len <- sapply(x.words, length)
table(len)
#0   1   2   3   4   5   6   7   8   9  10 
#1 138 130 129  70  35  20  10   5   3   3 



#####################
# Don't forget to save it frequently
save(cleanaff, file = "clean.aff.RData")

write.table(x = cleanaff, 
            file = file.path(file.dir, "new.clean_affiliations.csv"), sep = ",", 
            quote = FALSE, row.names = FALSE)

ID <- rep(data[,1], nrep)
which(ID == 8425)
fields[20,]
ID[1:30]


data.dir <- "D:/GitHub/Data-Analytics-Lab-Prof.Degras/EntityResolution/MoreCode/data analysis/affiliation/" # for Xinmin
data <- read.csv(file.path(data.dir, "new.clean_affiliations.csv"))

names(data)
#[1] "ID"      "Name1"   "Name2"   "Name3"   "Street1" "Street2" "City"    "State"   "Zipcode" "Country"

x <- data[, 4] #Name3
out <- table(x) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))
out[,1]

key <- as.vector(out[grepl("[0-9]", out[,1]), 1])
key <- as.vector(out[substr(out[,1],1,3) == "USA", 1])

key

data[data[, 4] %in% key, "ID"]

data[substr(data[,4],1,2) == "Mu", "ID"]
data[data[,4] == "IL", "ID"]

#[1] 7805 7866
#[1] 7745 7670 "USA"
#[1] 7671 9257
#[1] 7990 7925 7991 7936 CA
#[1] 7992 AZ
#[1] 7916 7984 7922 MA
#[1] 9791 7727 MD
#[1] 7917 RI
#[1] 7989 TX
#[1] 7948 8021 WA
#[1] 7910 WI
#[1] 7958 IN
#[1] 8019 MI
#[1] 8012 NJ
#[1] 7956 WA
#[1] 8754 Germany
#[1] 6535 Paris
#[1] 7771 Sydney
#[1] 7795 Newark
#[1] 7883 Stanford
#[1] 6918 8095 Singapore
#[1] 8763 7781 Tainan
#[1] 1965 Tubingen
#[1] 6186 Saudi Arabia
#[1] 5280 1810 1812 8687 7747 1527 1522  516  515 8686 9486 1769  
#840 1767 1387 1390  268 5325 6166 5709 5566 5710 5554 Hong Kong
#[1] 920 922 923 Taipei
#[1]  677  855 6860 6843 1492 8411 5651 8273 5977 7998 Santa *** city names?
#[1] 1483 6563 1839 Tucson
#[1] 8484 5802 state CT?
#[1] 5945 state UT?
#[1] 8505 ROC
#[1] 8042 SA??
#[1] 851 SP?
#[1] 9502 Rio city?
#[1] 963 New Delhi
#[1] 7904 Maryland state?
#[1] 7146 Nashua NH
#[1] 8308 OK
#[1] 5445 OR
#[1] 9306  887 city?
#[1] 1148 Zurich
#[1] 5737 NSW 2006 state and zipcode
#[1] 7807 Greece country
#[1] 6076 DC
#[1] 2966 4075 IL
#[1] 6524 6718 6574 6509 6652 6649 7837 street?
#[1] 6477 6475 Mumbai

idx <- 6475 # change the id number one by one
data[data[,"ID"] == idx, ]

data[data[,"ID"] == idx, "Street1"] <- data[data[,"ID"] == idx, "Name3"]
data[data[,"ID"] == idx, "Street2"] <- data[data[,"ID"] == idx, "Name3"]
data[data[,"ID"] == idx, "Name3"] <- ""
data[data[,"ID"] == idx, "Street1"] <- paste(data[data[,"ID"] == idx, "Name3"], data[data[,"ID"] == idx, "Street1"], sep = " ")

data[data[,"ID"] == idx, "State"] <- data[data[,"ID"] == idx, "Name3"]
data[data[,"ID"] == idx, "Name3"] <- ""

data[data[,"ID"] == idx, "City"] <- data[data[,"ID"] == idx, "Name2"]
data[data[,"ID"] == idx, "Name2"] <- ""

data[data[,"ID"] == idx, "State"] <- ""


zp <- data[data[,"ID"] == idx, 4]; zp

data[data[,"ID"] == idx, "Zipcode"] <- substr(zp, start = 4, stop = nchar(zp))
data[data[,"ID"] == idx, "State"] <- substr(zp, 1, 2)

data[data[,"ID"] == idx, "Country"] <- "Germany"
data[data[,"ID"] == idx, "Zipcode"] <- "400076"

data[data[,"ID"] == idx, "City"] <- data[data[,"ID"] == idx, "Street2"]

data[data[,"ID"] == idx, "City"] <- "Mumbai"
data[data[,"ID"] == idx, "Street2"] <- ""

# TO BE CONT. from here
x <- data[, 5] #street1
out <- table(x) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))
out[,1]

data[substr(data[,5],1,3) == "USA", "ID"]
data[data[,5] == "Hong Kong", "ID"]
data[substr(data[,5],1,2) == "TX", "ID"]

statename <- c("MA", "MD", "CA")
data[substr(data[,5],1,6) %in% statename, "ID"]
idx <- data[substr(data[,5],1,2) %in% statename, "ID"]

countryname <- c("Canada", "Denmar", "German", "Greece") #incomplete here
idx <- data[substr(data[,5],1,6) %in% countryname, "ID"]
data[data[,"ID"] %in% idx, ]

#[1] 9136? 8804?  540? 8612 9385  762 1274 1034? 1525? 8683 9486  694?  760? 4795?    6 Hong Kong
#[1] 5671  570 1345 1415 6789 5571 IL
#[1] 7699 9428 8545 9267 8795 8823 9300 9260 9489 8756 8674 8684 8677 8831 9487 
#8799 8800 9426 9388 8701 9294 7785 9262 9432 8887 7877 8819 9333 8889 9258 7692 USA
#[1] 7751 7858 WA
#[1] 7856 7812 IL
#[1] 6449 DC??
#[1]  865 7665  864 PA
#[1] 7664 NH
#[1] 7913 7717 7711 NJ
#[1] 7827 FL
#[1]  804?? 7857 8171 7792 MA / MD / CA
#[1] 1416 LA ??
#[1] 7888 TX

#idx <- 7751
idx <- data[substr(data[,5],1,2) == "NJ", "ID"]
data[data[,"ID"] %in% idx, ]
for(i in 1:length(idx)){
  zp <- data[data[,"ID"] == idx[i], 5]
  data[data[,"ID"] == idx[i], "Zipcode"] <- substr(zp, start = 4, stop = nchar(zp))
  data[data[,"ID"] == idx[i], "State"] <- substr(zp, start = 1, stop = 2)
  data[data[,"ID"] == idx[i], "Street1"] <- data[data[,"ID"] == idx[i], "Name3"]
  data[data[,"ID"] == idx[i], "Street2"] <- ""
  data[data[,"ID"] == idx[i], "Country"] <- "USA"
  data[data[,"ID"] == idx[i], "Name3"] <- ""
}

idx <- 7888
data[data[,"ID"] %in% idx, ]

zp <- data[data[,"ID"] == idx, 5]
data[data[,"ID"] == idx, "Zipcode"] <- substr(zp, start = 4, stop = nchar(zp))
data[data[,"ID"] == idx, "State"] <- substr(zp, start = 1, stop = 2)

data[data[,"ID"] == idx, "State"] <- "TX"
data[data[,"ID"] == idx, "Country"] <- "USA"
data[data[,"ID"] == idx, "Street1"] <- ""
data[data[,"ID"] == idx, "Street1"] <- data[data[,"ID"] == idx, "Name3"]
data[data[,"ID"] == idx, "City"] <- data[data[,"ID"] == idx, "Name3"]
data[data[,"ID"] == idx, "Street2"] <- ""
data[data[,"ID"] == idx, "Zipcode"] <- "78666"
data[data[,"ID"] == idx, "Name3"] <- ""

idx <- c(9385, 762, 1274, 8683, 9486, 6,5671,570,1345,1415,6789,5571) #Hong Kong (incomplete) and IL
data[data[,"ID"] %in% idx, ]
for(i in 1:length(idx)){
  data[data[,"ID"] == idx[i], "State"] <- data[data[,"ID"] == idx[i], "Street1"]
  data[data[,"ID"] == idx[i], "Street1"] <- ""
}

idx <- c(9136, 8804, 540, 1034, 1525, 694, 760, 4795) #city, state, country???
data[data[,"ID"] %in% idx, ]
for(i in 1:length(idx)){
  data[data[,"ID"] == idx[i], "State"] <- data[data[,"ID"] == idx[i], "Street1"]
  data[data[,"ID"] == idx[i], "Street1"] <- ""
}

idx <- c(9428, 8545, 9267, 8795, 8823, 9300, 9260, 9489, 8756, 8674, 8684, 
         8677, 8831, 9487, 8799, 8800, 9426, 9388, 8701, 9294, 9262, 9432, 
         8887, 8819, 9333, 8889, 9258) # USA and zip code

data[data[,"ID"] %in% idx, ]

for(i in 1: length(idx)){
  zp <- data[data[,"ID"] == idx[i], 5]
  data[data[,"ID"] == idx[i], "Zipcode"] <- substr(zp, start = 5, stop = nchar(zp))
  data[data[,"ID"] == idx[i], "Street1"] <- ""
  data[data[,"ID"] == idx[i], "Country"] <- "USA"
}



idx <- c(7699, 7785, 7877, 7692) # USA
data[data[,"ID"] %in% idx, ]
for(i in 1:length(idx)){
  data[data[,"ID"] == idx[i], "Country"] <- "USA"
  data[data[,"ID"] == idx[i], "Street1"] <- ""
}



library(tidyverse)


save(data, file = "clean.aff.RData")

write.table(x = data, 
            file = file.path(data.dir, "new.clean_affiliations.csv"), sep = ",", 
            quote = FALSE, row.names = FALSE)
