# aim: extract and summarize first names from acknowledgements 
# date: 2018-08-09
# h.j.lamberink@umcutrecht.nl
##############################################################################

.libPaths(c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary"))

# libraries
library(stringr)
library(dplyr)

#################################
# FUNCTIONS
#################################

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


#################################
# END FUNCTIONS
#################################

# acknowledgement sections
#df <- read.csv("acknowledgements.csv", stringsAsFactors = F)
df <- read.csv("results/acknowledgement_data_with_firstnames.csv", stringsAsFactors = F)
acks <- df[!is.na(df$acknowledgement),]$acknowledgement
acks <- paste0( " ", acks )  # some sections start with a name. include those
pmids <- df[!is.na(df$acknowledgement),]$pmid


# cut acknowledgements up into before and after Thank/gratful etc
a_split_thank <- str_split(acks, paste0( (?i), "thank|grate|gratit|acknow|appreciat") )

a_thank <- lapply(a_split_thank, function(x) {
  col1 <- col2 <- NA
  
  col1 <- x[1]
  if( length(x) > 1 ) col2 <- paste0( "[WORD REMOVED]", x[2:length(x)], collapse = " " )
  
  out <- data.frame(acknowledgement = col1,
                    thrown_out = col2,
                    stringsAsFactors = F)
})


thank_df <- data.frame(matrix(unlist(a_thank), nrow = length(a_thank), byrow = T))
names(thank_df) <- c("before", "after")
thank_df$acknowledgement <- acks




# cut acknowledgements up into before and after conflicts of interest etc
a_split <- str_split(thank_df$after, paste0( (?i), "conflict|author contribution|funded by|supported by|sponsored by|disclaimer|declaration|funding|financial support") )

a_thrown <- lapply(a_split, function(x) {
  col1 <- col2 <- NA
  
  col1 <- x[1]
  if( length(x) > 1 ) col2 <- paste0( "[WORD(S) REMOVED]", x[2:length(x)], collapse = " " )
  
  out <- data.frame(acknowledgement = col1,
                    thrown_out = col2,
                    stringsAsFactors = F)
})


use_df <- data.frame(matrix(unlist(a_thrown), nrow = length(a_thrown), byrow = T))
names(use_df) <- c("remaining", "trown_out")
use_df$acknowledgement <- acks
use_df$pmid <- df$pmid





# first names
fn <- read.csv2("fnames.csv", stringsAsFactors = F)
# source: https://github.com/MatthiasWinkelmann/firstname-database, extracted on August 9, 2018
fn <- fn[fn$name != "e" &
           fn$name != "a" &
           fn$name != "n" &
           fn$name != "ije" &
           fn$name != "ica" &
           fn$name != "de" &
           fn$name != "nas" &
           fn$name != "D" &
           fn$name != "F" &
           fn$name != "G" &
           fn$name != "J" &
           fn$name != "S" &
           fn$name != "",]
fn$name <- replace_non_ascii2(fn$name)
fn$name <- gsub("\\+", "[[:punct:]]", fn$name)


##########################
# first names only
##########################
#
#df$acknames_first <- NA
#namelist <- NULL
#
#print(Sys.time())
#
#
#for( i in 1:length(acks)) {
#  pmid <- pmids[i]
#  txt <- acks[i]
#  
#
#  # find all names from list that are present in text
#  ns <- NULL
#  for(z in fn$name){
#    if(grepl(paste0(z," "),txt)) {ns <- c(ns,z); print(z)}
#  }
#  ns <- unique(ns)
#  ns <- paste0(" ", ns, "[,\\.[:space:]]" )
#  print(ns)
#  
#  # extract the full names from text
#  a <- str_extract_all(txt, ns)
#  a <- unlist(a)
#  a <- gsub("[[:punct:]]", "", a)
#  a <- gsub("[[:space:]]", "", a)
#  a <- a[nchar(a)>1]
#  df[i,]$acknames_full <- paste0(a, collapse = "|")
#  
#  namelist <- c(namelist, a)
#
#  
#  if(i %in% seq(0,60000, 100)) print(paste("finished:", i, "of", length(acks)))
#  
#  if(i %in% seq(0,60000,10000)) {
#    print("writing ./results/most_frequent_namesFirst_partial.csv")
#    namelist <- gsub("[,\\.[:space:]]", "", namelist)
#    namelist <- namelist[nchar(namelist)>1]
#    
#    outframe <- data.frame(table(namelist))
#    outframe <- arrange(outframe, -Freq)
#    outframe$rank <- row.names(outframe)
#    
#    write.csv2(outframe, "results/most_frequent_namesFirst_partial.csv", row.names = F)
#  }
#}
#
#
#outframe <- data.frame(table(namelist))
#outframe <- arrange(outframe, -Freq)
#outframe$rank <- row.names(outframe)
#
#write.csv2(outframe, "results/most_frequent_namesFirst.csv", row.names = F)
#write.csv(df[,c("pmid", "acknowledgement", "acknames_first")], "results/acknowledgement_data_with_firstnames.csv", row.names = F)

################################
# first and last names together 
################################



df$acknames_full <- df$acknames_initials <- df$ackn_used <- NA
namelist <- initiallist <- NULL

print(Sys.time())

for( i in 1:length(acks)) {
#for(i in 1:10){
  pmid <- use_df$pmids[i]
  txt <- as.character(use_df$remaining[i])
  df$ackn_used[i] <- txt
  
  # find all names from list that are present in text
  ns <- NULL
  for(z in fn$name){
    if(grepl(paste0(z," "),txt)) ns <- c(ns,z)
  }
  ns <- unique(ns)
  ns <- paste0(" ", ns, "[[:space:]][[A-Z](\\.)*[:space:]]*[A-Z][[:alpha:]']*" )
  
  # extract the full names from text
  a <- str_extract_all(txt, ns)
  a <- unlist(a)
  a <- sub("^ ", "", a)
  a <- gsub("[[:punct:]]", "", a)
  a <- gsub("([A-Z])([[:space:]])([A-Z]+)([[:space:]])", "\\1\\3\\4", a)
  a <- a[nchar(a)>1]
  df[i,]$acknames_full <- paste0(a, collapse = "|")
  
  namelist <- c(namelist, a)
  
  #if( length(a) == 0 ) {
    # extract names with only initials from text
    ptrn <- "[[A-Z]]+(\\.)*[[:space:]]*[[A-Z](\\.)*[:space:]]*[A-Z]('[A-Z])*[[:lower:]]+"
    b <- str_extract_all(txt, ptrn)
    b <- unlist(b)
    b <- sub("^ ", "", b)
    b <- gsub("[[:punct:]]", "", b)
    b <- gsub("([A-Z])([[:space:]])([A-Z]+)([[:space:]])", "\\1\\3\\4", b)
    b <- b[nchar(b)>1]
    df[i,]$acknames_initials <- paste0(b, collapse = "|")
    
    initiallist <- c(initiallist, b)
  #}
  
  
  if(i %in% seq(0,60000, 100)) print(paste("finished:", i, "of", length(acks)))
  
  if(i %in% seq(0,60000,10000)) {
    print("writing ./results/most_frequent_namesFull_initials_partial.csv and ..noInitials_partial.csv")
    
    namelist2 <- gsub("[A-Z]\\.", "", namelist)
    namelist2 <- gsub("[A-Z][[:space:]]+", "", namelist2)
    namelist2 <- gsub("[[:space:]]{2,}", " ", namelist2)
    
    outframe <- data.frame(table(namelist))
    outframe <- arrange(outframe, -Freq)
    outframe$rank <- row.names(outframe)
    
    outframe2 <- data.frame(table(namelist2))
    outframe2 <- arrange(outframe2, -Freq)
    outframe2$rank <- row.names(outframe2)
    
    outframe3 <- data.frame(table(initiallist))
    outframe3 <- arrange(outframe3, -Freq)
    outframe3$rank <- row.names(outframe3)
    
    
    write.csv2(outframe, "results/most_frequent_namesFull_initials_partial.csv", row.names = F)
    write.csv2(outframe2, "results/most_frequent_namesFull_noInitials_partial.csv", row.names = F)
    write.csv2(outframe3, "results/most_frequent_namesInitials_partial.csv", row.names = F)
  }
}

df[df == ""] <- NA
df$ack_names_combined <- df$acknames_full
df[is.na(df$ack_names_combined),]$ack_names_combined <- df[is.na(df$ack_names_combined),]$acknames_initials


namelist2 <- gsub("[A-Z]\\.", "", namelist)
namelist2 <- gsub("[A-Z][[:space:]]+", "", namelist2)
namelist2 <- gsub("[[:space:]]{2,}", " ", namelist2)

outframe <- data.frame(table(namelist))
outframe <- arrange(outframe, -Freq)
outframe$rank <- row.names(outframe)

outframe2 <- data.frame(table(namelist2))
outframe2 <- arrange(outframe2, -Freq)
outframe2$rank <- row.names(outframe2)

outframe3 <- data.frame(table(initiallist))
outframe3 <- arrange(outframe3, -Freq)
outframe3$rank <- row.names(outframe3)

totalnames <- unlist(strsplit(df$ack_names_combined, "\\|"))
totalnames <- totalnames[!is.na(totalnames)]
outframetotal <- data.frame(table(totalnames))
outframetotal <- arrange(outframetotal, -Freq)
outframetotal$rank <- row.names(outframetotal)




write.csv2(outframe, "results/most_frequent_namesFull_initials.csv", row.names = F)
write.csv2(outframe2, "results/most_frequent_namesFull_noInitials.csv", row.names = F)
write.csv2(outframe3, "results/most_frequent_namesInitials.csv", row.names = F)
write.csv2(outframetotal, "results/most_frequent_namesTotal.csv", row.names = F)
write.csv(df[,c("pmid", "acknowledgement", "acknames_first", "acknames_full", "ackn_used", "acknames_initials")], "results/acknowledgement_data_with_firstAndLastnames.csv", row.names = F)



# quit R session
q("no")

