# aim1: reading authors_part: columns are not aligned properly
# aim2: get correct firstnames
# date: 2018-09-14
# contact: h.j.lamberink@umcutrecht.nl
######################################################################################


.libPaths( c( .libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) ) 


#####################################
# FUNCTIONS
#####################################

###
# adjusted version of rscopus::replace_non_ascii
##
replace_non_ascii2 <- function (string) 
{
  raws = structure(list(S = as.raw(c(197, 160)), s = as.raw(c(197,161)), 
                        Z = as.raw(c(197, 189)), z = as.raw(c(197, 190)), 
                        A = as.raw(c(195, 128)), A = as.raw(c(195, 129)), 
                        A = as.raw(c(195, 130)), A = as.raw(c(195, 131)), 
                        A = as.raw(c(195, 132)), A = as.raw(c(195, 133)), 
                        A = as.raw(c(195, 134)), C = as.raw(c(195, 135)), 
                        E = as.raw(c(195, 136)), E = as.raw(c(195, 137)), 
                        E = as.raw(c(195, 138)), E = as.raw(c(195, 139)), 
                        I = as.raw(c(195, 140)), I = as.raw(c(195, 141)), 
                        I = as.raw(c(195, 142)), I = as.raw(c(195, 143)), 
                        N = as.raw(c(195, 145)), O = as.raw(c(195, 146)), 
                        O = as.raw(c(195, 147)), O = as.raw(c(195, 148)), 
                        O = as.raw(c(195, 149)), O = as.raw(c(195, 150)), 
                        O = as.raw(c(195, 152)), U = as.raw(c(195, 153)), 
                        U = as.raw(c(195, 154)), U = as.raw(c(195, 155)), 
                        U = as.raw(c(195, 156)), Y = as.raw(c(195, 157)), 
                        B = as.raw(c(195, 158)), Ss = as.raw(c(195, 159)), 
                        a = as.raw(c(195, 160)), a = as.raw(c(195, 161)), 
                        a = as.raw(c(195, 162)), a = as.raw(c(195, 163)), 
                        a = as.raw(c(195, 164)), a = as.raw(c(195, 165)), 
                        a = as.raw(c(195, 166)), c = as.raw(c(195, 167)), 
                        e = as.raw(c(195, 168)), e = as.raw(c(195, 169)), 
                        e = as.raw(c(195, 170)), e = as.raw(c(195, 171)), 
                        i = as.raw(c(195, 172)), i = as.raw(c(195, 173)), 
                        i = as.raw(c(195, 174)), i = as.raw(c(195, 175)), 
                        o = as.raw(c(195, 176)), n = as.raw(c(195, 177)), 
                        o = as.raw(c(195, 178)), o = as.raw(c(195, 179)), 
                        o = as.raw(c(195, 180)), o = as.raw(c(195, 181)), 
                        o = as.raw(c(195, 182)), o = as.raw(c(195, 184)), 
                        u = as.raw(c(195, 185)), u = as.raw(c(195, 186)), 
                        u = as.raw(c(195, 187)), y = as.raw(c(195, 189)), 
                        y = as.raw(c(195, 189)), b = as.raw(c(195, 190)), 
                        y = as.raw(c(195, 191)),
                        c = as.raw(c(196, 135)), u = as.raw(c(195, 188))), 
                   .Names = c("S", "s", "Z", "z", "A", "A", 
                              "A", "A", "A", "A", "A", "C", "E", "E", "E", "E", "I", 
                              "I", "I", "I", "N", "O", "O", "O", "O", "O", "O", "U", 
                              "U", "U", "U", "Y", "B", "Ss", "a", "a", "a", "a", "a", 
                              "a", "a", "c", "e", "e", "e", "e", "i", "i", "i", "i", 
                              "o", "n", "o", "o", "o", "o", "o", "o", "u", "u", "u", 
                              "y", "y", "b", "y", "c", "u"))
  unwanted_array = names(raws)
  unwanted_array_names = sapply(raws, rawToChar)
  names(unwanted_array) = unwanted_array_names
  for( i in 1:length( unwanted_array ) )
  {
    string <- gsub( unwanted_array_names[i], unwanted_array[i], string)
  } 
  
  return(string)
}


#####################################
# END FUNCTIONS
#####################################



# first half & second half (authors)
total1 <- read.csv("authors_with_details_part.csv", stringsAsFactors = F, 
                   skip = 1, header = F) # manually fixed quotation mark in text file, line 91976 "State Institute of \"Pediatrics"
print(dim(total1))
first <- data.frame(total1[c(1:39110),]) # up to pmid 7068863
second <- data.frame(total1[c(39111:48960),]) # from pmid 7068865 onward
third <- data.frame(total1[c(48961:70217),]) # from pmid 8017468
fourth <- data.frame(total1[c(70218:length(total1[,1])),]) # from pmid 9670875



# intended headers
heads <- readLines("authors_with_details_part.csv", n = 1)
headsFirst <- c("pmid","authors","firstnames","affiliations","includedPDF",
                "includedScopus","included","yearpub","authorOne",
                "authorLast","authorOne_fn","authorLast_fn","authorOne_ln",
                "authorLast_ln","nAuthors","affFirst","affLast","affNumber",
                "ncitations","fullnames","scopusIDs","scopusID.first",
                "scopusID.last","affil_sc.first","affil_scID.first","h.first",
                "h.firstCurrent","nPubs.first","nPubs.firstCurrent",
                "nCollabor.first","acadAge_first","acadPresence_first",
                "affil_sc.last","affil_scID.last","h.last","h.lastCurrent",
                "nPubs.last","nPubs.lastCurrent","nCollabor.last",
                "acadAge_last","acadPresence_last", 
                "firstsname_first", "firstname_last")

headsSecond <- c("pmid","authors","firstnames","affiliations","includedPDF",
                 "includedScopus","included","yearpub","authorOne",
                 "authorLast","authorOne_fn","authorLast_fn","authorOne_ln",
                 "authorLast_ln","nAuthors","affFirst","affLast","affNumber",
                 "affil_sc.last","affil_scID.last","h.last","h.lastCurrent",
                 "nPubs.last","nPubs.lastCurrent","nCollabor.last",
                 "firstname_last","acadAge_last","acadPresence_last", # up to V28
                 "ncitations","fullnames","scopusIDs","scopusID.first",
                 "scopusID.last",
                 "affil_sc.first","affil_scID.first",
                 "h.first",
                 "h.firstCurrent","nPubs.first","nPubs.firstCurrent",
                 "nCollabor.first","firstsname_first","acadAge_first","acadPresence_first"  )

headsThird <- c("pmid","authors","firstnames","affiliations","includedPDF",
                "includedScopus","included","yearpub","authorOne",
                "authorLast","authorOne_fn","authorLast_fn","authorOne_ln",
                "authorLast_ln","nAuthors",
                "affil_sc.last","affil_scID.last","h.last","h.lastCurrent",
                "nPubs.last","nPubs.lastCurrent","nCollabor.last",
                "firstname_last","acadAge_last","acadPresence_last",
                "affFirst","affLast","affNumber",
                "affil_sc.first","affil_scID.first","h.first",
                "h.firstCurrent","nPubs.first","nPubs.firstCurrent",
                "nCollabor.first","firstsname_first","acadAge_first","acadPresence_first",
                "ncitations","fullnames","scopusIDs","scopusID.first",
                "scopusID.last" )

headsFourth <- c("pmid","authors","firstnames","affiliations","includedPDF",
                 "includedScopus","included","yearpub","authorOne",
                 "authorLast","authorOne_fn","authorLast_fn","authorOne_ln",
                 "authorLast_ln","nAuthors",
                 "ncitations","fullnames","scopusIDs","scopusID.first",
                 "scopusID.last","affil_sc.first","affil_scID.first","h.first",
                 "h.firstCurrent","nPubs.first","nPubs.firstCurrent",
                 "nCollabor.first", "firstsname_first",
                 "acadAge_first","acadPresence_first",
                 "affil_sc.last","affil_scID.last","h.last","h.lastCurrent",
                 "nPubs.last","nPubs.lastCurrent","nCollabor.last",
                 "firstname_last","acadAge_last","acadPresence_last", 
                 "affFirst","affLast","affNumber")

names(first) <- headsFirst
names(second) <- headsSecond 
names(third) <- headsThird 
names(fourth) <- headsFourth 



# first half and second half (auBackward)
total2 <- read.csv("../auBackward/authors_with_details_part.csv", stringsAsFactors = F,
                   skip = 1, header = F) # --- manually fixed line 57164 quotation mark before Alexandra in "General District Hospital \"Alexandra"
print(dim(total2))
back1 <- data.frame(total2[c(1:60757),]) #until 60757
back2 <- data.frame(total2[c(60758:94658),]) # 60758, from pmid 23989999
back3 <- data.frame(total2[94659:length(total2[,1]),]) # from pmid 21653904


headsBack1 <- c("pmid","authors","firstnames","affiliations","includedPDF",
               "includedScopus","included","yearpub","authorOne",
               "authorLast","authorOne_fn","authorLast_fn","authorOne_ln",
               "authorLast_ln","nAuthors",
               "ncitations","fullnames","scopusIDs","scopusID.first",
               "scopusID.last","affil_sc.first","affil_scID.first","h.first",
               "h.firstCurrent","nPubs.first","nPubs.firstCurrent",
               "nCollabor.first", "firstsname_first",
               "acadAge_first","acadPresence_first",
               "affil_sc.last","affil_scID.last","h.last","h.lastCurrent",
               "nPubs.last","nPubs.lastCurrent","nCollabor.last",
               "firstname_last","acadAge_last","acadPresence_last", 
               "affFirst","affLast","affNumber")

headsBack2 <- c("pmid","authors","firstnames","affiliations","includedPDF",
                "includedScopus","included","yearpub","authorOne",
                "authorLast","authorOne_fn","authorLast_fn","authorOne_ln",
                "authorLast_ln","nAuthors",
                "ncitations","fullnames","scopusIDs","scopusID.first",
                "scopusID.last","affil_sc.first","affil_scID.first","h.first",
                "h.firstCurrent","nPubs.first","nPubs.firstCurrent",
                "nCollabor.first", "firstsname_first",
                "acadAge_last","acadPresence_first",
                "affil_sc.last","affil_scID.last","h.last","h.lastCurrent",
                "nPubs.last","nPubs.lastCurrent","nCollabor.last",
                "firstname_last","acadPresence_last",
                "affFirst","affLast","affNumber", "acadAge_first")

headsBack3 <- c("pmid","authors","firstnames","affiliations","includedPDF",
                "includedScopus","included","yearpub","authorOne",
                "authorLast","authorOne_fn","authorLast_fn","authorOne_ln",
                "authorLast_ln","nAuthors",
                "affil_sc.last","affil_scID.last","h.last","h.lastCurrent",
                "nPubs.last","nPubs.lastCurrent","nCollabor.last",
                "firstname_last","acadAge_last","acadPresence_last", 
                "affFirst","affLast","affNumber",
                "ncitations","fullnames","scopusIDs","scopusID.first",
                "scopusID.last",
                "affil_sc.first","affil_scID.first",
                "h.first",
                "h.firstCurrent","nPubs.first","nPubs.firstCurrent",
                "nCollabor.first","firstsname_first","acadAge_first","acadPresence_first")



names(back1) <- headsBack1
names(back2) <- headsBack2
names(back3) <- headsBack3



##############
# combine first four and three backs and make into correct variable types. rbind the two
##############
forward <- rbind(first,second,third,fourth)
backward <- rbind(back1,back2,back3)

# all numeric variables as numeric
forward[,c("ncitations", "h.first", "h.firstCurrent", "nPubs.first", "nPubs.firstCurrent",
      "nCollabor.first", "acadAge_first", "acadPresence_first",
      "h.last", "h.lastCurrent", "nPubs.last", "nPubs.lastCurrent",
      "nCollabor.last", "acadAge_last", "acadPresence_last")] <- 
  lapply(forward[,c("ncitations", "h.first", "h.firstCurrent", "nPubs.first", "nPubs.firstCurrent",
               "nCollabor.first", "acadAge_first", "acadPresence_first",
               "h.last", "h.lastCurrent", "nPubs.last", "nPubs.lastCurrent",
               "nCollabor.last", "acadAge_last", "acadPresence_last")], 
         as.numeric)

backward[,c("ncitations", "h.first", "h.firstCurrent", "nPubs.first", "nPubs.firstCurrent",
           "nCollabor.first", "acadAge_first", "acadPresence_first",
           "h.last", "h.lastCurrent", "nPubs.last", "nPubs.lastCurrent",
           "nCollabor.last", "acadAge_last", "acadPresence_last")] <- 
  lapply(backward[,c("ncitations", "h.first", "h.firstCurrent", "nPubs.first", "nPubs.firstCurrent",
                    "nCollabor.first", "acadAge_first", "acadPresence_first",
                    "h.last", "h.lastCurrent", "nPubs.last", "nPubs.lastCurrent",
                    "nCollabor.last", "acadAge_last", "acadPresence_last")], 
         as.numeric)

# rbind
author_df <- rbind(forward, backward)
author_df[author_df == ""] <- NA

print(dim(author_df))

# remove 3998 double entries ## checked: all double entries are similar
author_df <- author_df[!duplicated(author_df$pmid),]

# change some erronous values to NA
author_df[!is.na(author_df$acadAge_first) & author_df$acadAge_first > 80,]$acadAge_first <- NA
author_df[!is.na(author_df$acadAge_last) & author_df$acadAge_last > 80,]$acadAge_last <- NA
author_df[!is.na(author_df$acadPresence_first) & author_df$acadPresence_first > 80,]$acadPresence_first <- NA
#author_df[!is.na(author_df$acadPresence_last) & author_df$acadPresence_last > 80,]$acadPresence_last <- NA

# replace non ascii characters in fullnames column
author_df$fullnames <- replace_non_ascii2(author_df$fullnames)
author_df$firstnames <- replace_non_ascii2(author_df$firstnames)



#### TODO ####
# 1) for names first and last author, when firstname is missing, check whether column fullnames has the first name
# 2) for names in between, extract from fullnames and/or pubmed itself

author_df$firstnames_scopus <- author_df$firstname_first <- NA


#for(i in  1:10) {
for(i in  1:length(author_df[,1])) {
  pmedfnames <- scnames <- NA
  rw <- author_df[i,]

  # process firstnames as given by pubmed
  pmedfnames <- strsplit( rw$firstnames, "\\|" )[[1]]
  pmedfnames <- sapply(pmedfnames, function(x) {
    ns <- gsub("[[:punct:]]", " ", x)
    ns <- strsplit(tolower(ns), split = " " )[[1]]
    ns <- ns[nchar(ns) > 1]
    if(length(ns) == 0) ns <- NA
    if(length(ns) > 1) ns <- ns[1]
    return(ns)
    })
  
  # if no firstnames, process fullnames as downloaded by scopus  
  if(sum(is.na(pmedfnames)) == length(pmedfnames)) {
    scnames <- gsub("([[:upper:]])([[:upper:]])", "\\1 \\2", rw$fullnames)
    scnames <- strsplit( tolower(scnames), "\\|" )[[1]]
    scnames <- scnames[!duplicated(scnames)]
    scnames <- sapply(scnames, function(x) {
      ns <- gsub("\\.", " ", x)
      ns <- strsplit(ns, split = " " )[[1]]
      ns <- ns[1:length(ns)-1]
      ns <- ns[nchar(ns) > 1]
      ns <- ns[!ns %in% c("del", "de", "da", "la", "van", "der", "el")]
      if(length(ns) == 0) ns <- NA
      if(length(ns) > 1) ns <- ns[1]
      return(ns)
    })
    if(sum(is.na(scnames)) > (length(scnames) / 2) ) scnames <- NA ## when more than 50% of names are missing, (1) the variable becomes useless and (2) some second last names have been mistaken for first names
    firstnames_scopus <- paste( scnames, collapse = "|" )
    firstname_first <- as.character(scnames[1])
    firstname_last <- as.character(scnames[length(scnames)])
  } else { 
    firstnames_scopus <- paste( pmedfnames, collapse = "|" ) 
    firstname_first <- as.character(pmedfnames[1])
    firstname_last <- as.character(pmedfnames[length(pmedfnames)])
    }
  
  if(is.na(firstname_first)) firstname_first <- rw$firstsname_first
  if(is.na(firstname_last)) firstname_last <- rw$firstname_last
  
  author_df[i,]$firstnames_scopus <- firstnames_scopus
  author_df[i,]$firstname_first <- firstname_first
  author_df[i,]$firstname_last <- firstname_last
  
  # provide info on progress
  if(i %in% seq(1, 300000, 10000)) print(paste("progress:", i, "in", length(author_df[,1])))
}

# change "NA" back to NA
author_df[author_df == "NA"] <- NA

###TODO
# remove names that were taken from pubmed and were actually initials
# step 1, identify all first names with one or two characters
# step 2, select those combinations with 20 or more mentions
# step 3, remove names with one character and those with two characters that don't belong
ffn <- as.data.frame(table(author_df[!is.na(author_df$firstname_first) & nchar(author_df$firstname_first) < 3,]$firstname_first))
lfn <- as.data.frame(table(author_df[!is.na(author_df$firstname_last) & nchar(author_df$firstname_last) < 3,]$firstname_last))

to.rm1 <- ffn[ffn$Freq < 20,]
to.rm2 <- lfn[lfn$Freq < 20,]
to.rm <- merge(to.rm1, to.rm2, by = "Var1", all = T)

to.rm$Freq <- NA
for(i in 1:length(to.rm[,1])) {
  rw <- to.rm[i,]
  to.rm[i,]$Freq <- sum( rw$Freq.x, rw$Freq.y, na.rm = T )
}

print("not removed:")
print(ffn[ffn$Freq > 19,])
print(lfn[lfn$Freq > 19,])
to.rmlist <- c(as.character(to.rm[to.rm$Freq < 20,]$Var1), "md", "jr")
to.rmlist <- to.rmlist[!to.rmlist %in% c("zu", "xi", "wu")]

# remove the selected items from firstname_first and firstname_last
if(sum(!is.na(author_df$firstname_first) & nchar(author_df$firstname_first) == 1) > 0) {
  author_df[!is.na(author_df$firstname_first) & nchar(author_df$firstname_first) == 1,]$firstname_first <- NA
}
author_df[!is.na(author_df$firstname_first) & author_df$firstname_first %in% to.rmlist,]$firstname_first <- NA

if(sum(!is.na(author_df$firstname_last) & nchar(author_df$firstname_last) == 1) > 0 ) {
  author_df[!is.na(author_df$firstname_last) & nchar(author_df$firstname_last) == 1,]$firstname_last <- NA
}
author_df[!is.na(author_df$firstname_last) & author_df$firstname_last %in% to.rmlist,]$firstname_last <- NA

to.rmlist <- data.frame( removed = to.rmlist )
print(to.rmlist)

###TODO # 3) with these, check with genderize data and determine additional genders.



# write data
write.csv(author_df, "author_scopus_data_full.csv", row.names = F)
write.csv(to.rmlist, "author_scopus_data_removed_firstnames.csv", row.names = F)

cols.to.save <- c("pmid","affiliations","yearpub",
                  "nAuthors","affFirst","affLast","affNumber",
                  "ncitations","scopusIDs","affil_sc.first","affil_scID.first","h.first",
                  "h.firstCurrent","nPubs.first","nPubs.firstCurrent",
                  "nCollabor.first","acadAge_first","acadPresence_first",
                  "affil_sc.last","affil_scID.last","h.last","h.lastCurrent",
                  "nPubs.last","nPubs.lastCurrent","nCollabor.last",
                  "acadAge_last","acadPresence_last", 
                  "firstname_first", "firstname_last", "firstnames_scopus")
write.csv(author_df[,cols.to.save], "author_scopus_data_clean.csv", row.names = F)

# quit R session
q("no")
