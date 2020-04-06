# aim: determine number of names thanked in acknowledgement
# date: 2018-09-07
# contact: h.j.lamberink@umcutrecht.nl
################################################################################

# libraries
library(plyr)

########################
# FUNCTIONS
########################


########################
# END FUNCTIONS
########################


# read data
df <- read.csv("results/acknowledgement_data_with_firstAndLastnames.csv", stringsAsFactors = F)
nf <- read.csv2("results/most_frequent_namesTotal.csv", stringsAsFactors = F) # summary of names in df$acknames_full
nf2 <- read.csv2("results/most_frequent_namesInitials.csv", stringsAsFactors = F) # summary of names in df$acknames_initials

# remove NA columns
nf <- nf[!is.na(nf$totalnames),]

# create column acknames_original and remove acknames_first
df$acknames_original <- df$acknames_full


df[is.na(df$acknames_original),]$acknames_original <- df[is.na(df$acknames_original),]$acknames_initials
df$acknames_first <- df$acknames_full <- df$acknames_initials <- NULL

#### steps executed below ####
# 1a) split nf$totalnames into first and last name; summarize last names and identify 'Designed', 'Contributed' etc
# 1b) repeat this for nf2$initiallist
# 2) save all these names in list to remove from df
# 3) identify companies, funders etc. and save all these names in the same list to remove from df
# 4) remove all names from nf and from df
# 5) create column df$nnames_ack
#### steps executed below ####



# 1a) split nf$totalnames
ns_split1 <- strsplit(nf$totalnames, " ")
ns_split <- lapply( ns_split1, function(x) {
    if(!is.na(x)) {
      ns <- data.frame(
        fname = x[1],
        lname = x[length(x)],
        mname = NA,
        stringsAsFactors = F
      )
      if(length(x) > 2) ns$mname = paste( x[2:(length(x)-1)], collapse = " " )
    }
    return(ns)
})

# list to dataframe
namez <- data.frame(matrix(unlist(ns_split), ncol = 3, byrow = T), stringsAsFactors = F)
names(namez) <- c("fname", "lname", "mname")

# investigate lastnames and firstnames
lnames <- as.data.frame(table(namez$lname))
fnames <- as.data.frame(table(namez$fname))

lnames <- arrange(lnames, -Freq)
fnames <- arrange(fnames, -Freq)
View(lnames)

# 1b) split nf2$initiallist
ns_split3 <- strsplit(nf2$initiallist, " ")
ns_split2 <- lapply( ns_split3, function(x) {
  if(!is.na(x)) {
    ns <- data.frame(
      fname = x[1],
      lname = x[length(x)],
      mname = NA,
      stringsAsFactors = F
    )
    if(length(x) > 2) ns$mname = paste( x[2:(length(x)-1)], collapse = " " )
  }
  return(ns)
})

# list to dataframe
namez2 <- data.frame(matrix(unlist(ns_split2), ncol = 3, byrow = T), stringsAsFactors = F)
names(namez2) <- c("fname", "lname", "mname")

# investigate lastnames and firstnames
lnames2 <- as.data.frame(table(namez2$lname))
fnames2 <- as.data.frame(table(namez2$fname))

lnames2 <- arrange(lnames2, -Freq)
fnames2 <- arrange(fnames2, -Freq)
View(lnames2)


# 2) make list of names to remove. fn.containing is specifically for fnames2 (initials only)
ln.to.remove <- c("Hospital", "Ziekenhuis", "Hypertension", "Education", "University", "Medical", "Analyzed", "Wrote", "Performed", "Department", "Contributed", "Health", "Research", "The", "Study", "Foundation", "Clinical", "MD", "A", "Grant", "Institute", "PhD", "M", "Trial", "J", "We", "St", "School", "Center", "Centre", "Pharmaceuticals", "S", "Ltd", "C", "Cancer", "G", "National", "Pharma", "Clinic", "International", "This", "Childrens", "R", "P", "RN", "Data", "F", "Laboratory", "Author", "County", "E", "Street", "Laboratories", "Pharmaceutical", "Inc", "O", "State", "Univer", "Company", "Healthcare", "General", "H", "Family", "K", "Uni", "D", "Division", "Dr", "L", "B", "MSc", "BSc", "City", "College", "MS", "UK", "Australia", "Central", "GlaxoSmithKline", "Ph", "Senior", "T", "V", "W", "Ministry", "Academic", "Canada", "Community", "Cooperative", "France", "Global", "Hospi", "Network", "Nutrition", "Pathology", "Program", "Investigators", "Steering", "Group", "Corporation", "Gmb", "Project", "Designed", "Funding", "Protocol", "Reviewed", "Conceived", "Interpreted", "Manuscript", "Statistical", "Technologies", "Biomedical", "Principal", "Programme", "Scientific", "Supervised", "Support", "Systems", "Pharma", "Limited", "Contrib", "Random", "Agree", "All")
fn.to.remove <- c("St", "The", "", "NHS", "AIDS", "NIH", "HIV", "WHO", "MRC", "GSK", "USA", "EORCT")
fulln.containing <- c("CONFLICT", "DECLARATION", "AUTHORS", "DISCLOSURE", "ROLE", "SCOPE", "APPENDIX", "ARTICLE", "ASTRA", "FUNDING", "F Hoffmann", "[Ss]cience", "Royal", "Netherlands", "Pan African", "Comprehensive")

# 3) identify companies, funders etc. and save in list
#View(nf)
names.to.remove <- c(nf$totalnames[c(1:53, 55, 57:63, 65:69, 71:72, 75, 77:80, 82, 85:94,
                                     98:100, 103:112, 115:117, 121:130, 134:135, 137:138,
                                     140:142, 145:152, 154:155, 157:161, 164:165, 180,
                                     182:185)])
#View(nf2)
names.to.remove2 <- c(nf2$initiallist[c(1:12, 14:31, 36, 38:45, 47:54, 57:59, 63:71, 
                                        73:76, 78:82, 84:85, 87, 89:91, 94:96, 98:99,
                                        101:106, 108, 112, 114:117, 119:120, 122:129,
                                        132:133, 136:144, 146, 149:150, 153, 156, 159,
                                        160:165, 169:173, 175:178, 181:182, 184:186, 
                                        188:192, 201:202, 205, 207, 210:214, 221:222,
                                        228, 230:232, 235:238, 247, 251:260, 263, 
                                        265:270 )])


# 4) remove all names from nf and from df
# first, identify rows where last name is only one letter
onechar <- unique( namez[nchar(namez$lname) == 1,]$lname )
onechar2 <- unique( namez2[nchar(namez2$lname) == 1,]$lname )
onechar <- c(onechar, onechar2)

# second, identify rows where last name is all caps 
allcaps <- namez[!grepl("[[:lower:]]", namez$lname),]$lname
allcaps2 <- namez2[!grepl("[[:lower:]]", namez2$lname),]$lname
allcaps <- c(allcaps, allcaps2)

# third, identify names where first name equals last name (actually being only one name but counted twice)
singlenm <- namez[namez$fname == namez$lname,]$lname
singlenm2 <- namez2[namez2$fname == namez2$lname,]$lname
singlenm <- unique(c(singlenm, singlenm2))

# fourth, identify names which contain erronous full-caps statements
statements <- statements2 <- NULL
for( z in fulln.containing ) {
  statement <- nf[grepl(z, nf$totalnames),]$totalnames
  statements <- c(statements, statement)
  
  statement2 <- nf2[grepl(z, nf2$totalnames),]$totalnames
  statements2 <- c(statements2, statement2)
}
statements <- unique(c(statements, statements2))

# lastly, combine onechar, allcaps, fn.to.remove, ln.to.remove and names.to.remove
# rows to remove from nf
rows.to.remove <- row.names(namez[namez$lname %in% ln.to.remove,])
rows.to.remove <- unique(c(rows.to.remove, as.character( as.numeric(row.names(nf[nf$totalnames %in% statements,])) - 1)))
rows.to.remove <- unique(c(rows.to.remove, 
                           row.names(namez[!is.na(namez$fname) & !grepl("[[:lower:]]", namez$fname ) & 
                                             !is.na(namez$mname) & !grepl("[[:lower:]]", namez$mname),]))) #both first and middle name all caps
rows.to.remove <- unique(c(rows.to.remove, row.names(namez[namez$fname %in% fn.to.remove,])))
rows.to.remove <- unique(c(rows.to.remove, row.names(namez[namez$fname %in% singlenm,])))
rows.to.remove <- unique(c(rows.to.remove, row.names(namez[namez$lname %in% onechar,])))
rows.to.remove <- unique(c(rows.to.remove, row.names(namez[namez$lname %in% allcaps,])))
rows.to.remove <- sort(as.integer(rows.to.remove))

rows.to.remove2 <- row.names(namez2[namez2$lname %in% ln.to.remove,])
rows.to.remove2 <- unique(c(rows.to.remove2, as.character( as.numeric(row.names(nf2[nf2$totalnames %in% statements,])) - 1)))
rows.to.remove2 <- unique(c(rows.to.remove2, 
                            row.names(namez2[!is.na(namez2$fname) & !grepl("[[:lower:]]", namez2$fname ) & 
                                               !is.na(namez2$mname) & !grepl("[[:lower:]]", namez2$mname),]))) #both first and middle name all caps
rows.to.remove2 <- unique(c(rows.to.remove2, row.names(namez2[namez2$fname %in% fn.to.remove,])))
rows.to.remove2 <- unique(c(rows.to.remove2, row.names(namez2[namez2$fname %in% singlenm,])))
rows.to.remove2 <- unique(c(rows.to.remove2, row.names(namez2[namez2$lname %in% onechar,])))
rows.to.remove2 <- unique(c(rows.to.remove2, row.names(namez2[namez2$lname %in% allcaps,])))
rows.to.remove2 <- sort(as.integer(rows.to.remove2))

# names to remove from df
names.to.remove <- unique(c(names.to.remove, nf[rows.to.remove,]$totalnames))
names.to.remove2 <- unique(c(names.to.remove2, nf2[rows.to.remove2,]$initiallist))
names.to.remove <- unique(c(names.to.remove, names.to.remove2))

df$nnames_ack <- df$acknames_cleaned <- NA
progbar <- txtProgressBar(min = 0, max = length(df[,1]), style = 3)

# 5) create column df$nnames_ack
for( i in 1:length(df[,1])) {
  
  setTxtProgressBar(progbar, i)
  
  sp_names <- strsplit(df[i,]$acknames_original, "\\|")[[1]]
  sp_names <- sp_names[!(sp_names %in% names.to.remove)]
  nnames <- length(sp_names)
  if(length(sp_names) == 0) sp_names <- NA
  suppressWarnings(if(is.na(sp_names)) nnames <- 0 )
  
  sp_names <- paste(sp_names, collapse = "|")
  sp_names[sp_names == "NA"] <- NA
  
  df[i,]$acknames_cleaned <- sp_names
  df[i,]$nnames_ack <- nnames
}




write.csv(df, "acknowledgement_names.csv", row.names = F)
