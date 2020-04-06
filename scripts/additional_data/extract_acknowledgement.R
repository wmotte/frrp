#
# contact: h.j.lamberink@umcutrecht.nl
# date: 2018-05-16
#
# aim: extract data from XML, analyse acknowledgement sections
#################################################################

.libPaths( c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) ) 


library( XML )
library( plyr )
#library( tei2r )
library( stringr )
library(pbapply)



######################################
# FUNCTIONS
######################################

###
# extract acknowledgement from GROBID XML
##
extract_grobidXML <- function( theFile ) 
{
  caughtTry <- tryCatch(
    {
      # parse into nodes and records
      newData <- xmlParse(theFile)
      
      # for every item extract data
      title <- as.character(xpathSApply(newData,"/TEI/teiHeader/fileDesc/titleStmt/title", xmlValue))
      title[ title == "" ] <- NA
      acknowl <- as.character(xpathSApply(newData,"//div[@type='acknowledgement']", xmlValue))
      acknowl <- sub("acknowledgement[s]?|acknowledgment[s]?", "", acknowl, ignore.case = T)
      acknowl[ acknowl == "" ] <- NA
      date <- as.character(xpathSApply(newData,"//publicationStmt/date", xmlValue))
      year <- as.numeric(str_extract(string = date, pattern = "19[0-9][0-9]|20[0-9][0-9]" ))
      if( length(year) == 0 ) year <- NA
      if( is.na(year) ) {
        yearnote <- as.character(xpathSApply(newData, "//sourceDesc/biblStruct/note", xmlValue))[1]
        year <- str_extract( string = yearnote, pattern = ", 19[0-9][0-9]|20[0-9][0-9]" )
        year <- as.numeric( str_extract( year, "[1|2][0|1|9][0-9][0-9]" ))
      }
      if( is.na(year) ) { 
        yearnote <- gsub( " ", "", yearnote )
        year <- str_extract( string = yearnote, pattern = ",19[0-9][0-9]|20[0-9][0-9]" )
        year <- as.numeric( str_extract( year, "[1|2][0|1|9][0-9][0-9]" ))
      }
      if( is.na(year) ){
        abstract <- as.character(xpathSApply(newData,"//profileDesc/abstract/p", xmlValue))
        yearinfo <- str_extract(abstract, "[0-9], 19[0-9][0-9]|20[0-9][0-9]")
        year <- as.numeric( str_extract( yearinfo, "[1|2][0|1|9][0-9][0-9]" ))
      }
      #coi1 <- as.character(xpathSApply(newData,"//ce:para[@id='para480']", xmlValue))
      #affil
      #country
      #finSupport
      
      # replace empty values by NA
      if( length(title) == 0 ) title <- NA
      if( length(acknowl) == 0 ) acknowl <- NA
      if( length(year) == 0 ) year <- NA
      
      
      theDF <- data.frame( 
        title = title,
        acknowledgement = acknowl,
        year = year,
        stringsAsFactors = F
      )
      
      
      # replace every instance of ';' by ':'
      theDF <- data.frame(lapply(theDF, gsub, pattern = ";", replacement = ":"), 
                          stringsAsFactors = F)
      
      
      return(theDF)
      
    },
    error=function(err) { message(err) },
    warning=function(war) { message(paste( war, "\n" ) ) }
  )
} 


###
# extract acknowledgement from scopus XML
##
extract_scopusXML <- function( theFile ) 
{
  caughtTry <- tryCatch(
    {
      # parse into nodes and records
      newData <- xmlParse(theFile)
      
      # for every item extract data
      title <- xpathSApply(newData,"//dc:title", xmlValue)
      acknowledgement <- xpathSApply(newData,"//ce:acknowledgment", xmlValue)
      acknowledgement <- gsub("acknowledgement[s]?|acknowledgment[s]?", "", acknowledgement, ignore.case = T)
      date <- xpathSApply(newData,"//prism:coverDate", xmlValue)
      year <- str_extract(date, "19[0-9][0-9]|20[0-9][0-9]")
      coi <- as.character(xpathSApply(newData,"//ce:para[@id='para480']", xmlValue)) 
      names_last <- xpathSApply(newData,"//ce:author/ce:surname", xmlValue)
      names_first <- xpathSApply(newData,"//ce:author/ce:given-name", xmlValue)
      
      
      affs <- xpathSApply(newData,"//ce:author/ce:cross-ref", xmlAttrs)
      if(is.matrix(affs)) {
        t <- data.frame(t(affs))
        selector <- grep("aff", t$refid)
        aff_id <- xpathSApply(newData,"//ce:author/ce:cross-ref", xmlValue)[selector]
      } else {
        selector <- grep("aff",affs)
        aff_id <- xpathSApply(newData,"//ce:author/ce:cross-ref", xmlValue)[selector]
      }
      aff_labs <- xpathSApply(newData,"//ce:affiliation/ce:label", xmlValue)
      aff_names <- xpathSApply(newData,"//ce:affiliation/ce:textfn", xmlValue)
      if(length(aff_names) > 1 & length(aff_names) == length(aff_labs)) {
        aff_df <- data.frame( labs = aff_labs,
                              names = aff_names,
                              stringsAsFactors = F)
        
        aff_authorOne <- aff_df[ aff_df$labs == aff_id[1], ]$names
        aff_authorLast <- aff_df[ aff_df$labs == aff_id[length(aff_id)], ]$names
        
      } else if(length(aff_names) == 1) {
        aff_authorOne <- aff_names
        aff_authorLast <- aff_names
      } else {
        aff_authorOne <- NA
        aff_authorLast <- NA
      }
      
      
      # in later stage, replace_non_ascii2() for authors, affiliations, others maybe?
      
      #country
      #finSupport
      
      # replace empty values by NA
      if( length(title) == 0 ) title <- NA
      if( length(acknowledgement) == 0 ) acknowledgement <- NA
      if( length(coi) == 0 ) coi <- NA
      if( length(aff_authorOne) == 0 ) aff_authorOne <- NA
      if( length(aff_authorLast) == 0 ) aff_authorLast <- NA
      if( length(names_first) == 0 ) names_first <- NA
      if( length(names_last) == 0 ) names_last <- NA
      
      
      
      theDF <- data.frame( 
        title = title,
        acknowledgement = acknowledgement,
        year = year,
        #coi = coi,
        #last_names = paste0(names_last, collapse = "|"),
        #first_names = paste0(names_first, collapse = "|"),
        #aff_authorFirst = aff_authorOne,
        #aff_authorLast = aff_authorLast,
        stringsAsFactors = F
      )
      
      # replace every instance of ';' by ':'
      theDF <- data.frame(lapply(theDF, gsub, pattern = ";", replacement = ":"), 
                          stringsAsFactors = F)
      
      return(theDF)
      
    },
    error=function(err) { message(err) },
    warning=function(war) { message(paste( war, "\n" ) ) }
  )
} 



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


######################################
# END FUNCTIONS
######################################

# read input data
#filezG1 <- list.files("O:/Psychiatrie/Predictie verantwoordonderzoek/testdata")
#filezS <- list.files("O:/Psychiatrie/Predictie verantwoordonderzoek/testdatascopus")
#pmidsG <- gsub( ".xml", "", filezG)
#pmidsS <- gsub( ".xml", "", filezS)


filezG1 <- list.files("/mnt/data/live02/stress/hlamberink/grobid/xmlout/")
filezG2 <- list.files("/mnt/data/live02/stress/hlamberink/grobid/xmloutNEW/")
filezS <- list.files("/mnt/data/live02/stress/hlamberink/scopus/xmlsComplete/")
filezS2 <- list.files("/mnt/data/live02/stress/hlamberink/scopus/xmlGrobid/")
pmidsG1 <- gsub( ".xml", "", filezG1)
pmidsG2 <- gsub( ".xml", "", filezG2)
pmidsS <- gsub( ".xml", "", filezS)
pmidsS2 <- gsub( ".xml", "", filezS2)
print(length(filezG1))
print(length(filezG2))
print(length(filezS))
print(length(filezS2))

Sout2 <- pblapply(filezS2, FUN = function(x){
  xml <- readLines(paste0("/mnt/data/live02/stress/hlamberink/scopus/xmlGrobid/", x))
  if(length(xml) > 1 ){
    xml <- c("<TEI>", xml[3:length(xml)])
    extract_grobidXML(xml)
  } else {
    data.frame(title = as.character(NA),
               acknowledgement = as.character(NA),
               year = as.character(NA),
               stringsAsFactors = F)
  }
})

outS2 <- ldply( Sout2, data.frame )
outS2$pmid <- pmidsS2
write.csv(outS2, "outS2_temp.csv", row.names = F)

# acknowledgement from scopus xml
Sout <- pblapply(filezS, FUN = function(x){
  xml <- readLines(paste0("/mnt/data/live02/stress/hlamberink/scopus/xmlsComplete/", x))
  extract_scopusXML(xml)
} )





# acknowledgement from GROBID xml
Gout1 <- pblapply(filezG1, FUN = function(x){
  xml <- readLines(paste0("/mnt/data/live02/stress/hlamberink/grobid/xmlout/", x))
  xml <- gsub( "\t", "", xml)
  
  if( length( xml ) > 1 ) {
    xml <- c("<TEI>", xml[c(6:length(xml))])
    extract_grobidXML(xml)
  } else {
    NA
  }
} )



Gout2 <- pblapply(filezG2, FUN = function(x){
  xml <- readLines(paste0("/mnt/data/live02/stress/hlamberink/grobid/xmloutNEW/", x))
  xml <- gsub( "\t", "", xml)
  
  if( length( xml ) > 1 ) {
    xml <- c("<TEI>", xml[c(6:length(xml))])
    extract_grobidXML(xml)
  } else {
    NA
  }
} )



# to dataframe
outG1 <- ldply( Gout1, data.frame )
outG2 <- ldply( Gout2, data.frame )
outS <- ldply( Sout, data.frame )
outS$pmid <- pmidsS
write.csv(outS, "outS_temp.csv", row.names = F)

outG1$pmid <- pmidsG1
outG2$pmid <- pmidsG2

write.csv(outG1, "outG1_temp.csv", row.names = F)
write.csv(outG2, "outG2_temp.csv", row.names = F)
outG1 <- read.csv("outG1_temp.csv", stringsAsFactors = F)
outG2 <- read.csv("outG2_temp.csv", stringsAsFactors = F)






outG1$source <- "grobid1"
outG2$source <- "grobid2"
outS$source <- "scopus_partial"
outS2$source <- "scopus2"

write.csv(outG1, "temp/outG1_temp.csv", row.names = F)
write.csv(outG2, "temp/outG2_temp.csv", row.names = F)
write.csv(outS, "temp/outS_temp.csv", row.names = F)
write.csv(outS2, "temp/outS2_temp.csv", row.names = F)

#outG1 <- read.csv("temp/outG1_temp.csv", stringsAsFactors = F)
#outG2 <- read.csv("temp/outG2_temp.csv", stringsAsFactors = F)
#outS <-  read.csv("temp/outS_temp.csv", stringsAsFactors = F)
#outS2 <- read.csv("temp/outS2_temp.csv", stringsAsFactors = F)

# merge scopus2 (all 60k) with scopus (19k), to see whether algorithm to detect
# acknowledgement sections could be improved
outS$ackScopusScript <- outS$acknowledgement
outScopus <- merge( outS2, outS[,c("pmid", "ackScopusScript")],
                    by = "pmid",
                    all.x = T, all.y = F)
# conclusion <- extract_grobidXML works better then extract_scopusXML. Funny but true.


# combine
out <- rbind(outG1,outG2,outS2)
out <- as.data.frame(apply(out, 1, gsub, pattern = "\t", replacement = ""))
out <- as.data.frame(apply(out, 1, gsub, pattern = "\n", replacement = ""))

out[ out == "\\s*" ] <- NA
out[ out == "" ] <- NA

# replace non-ascii characters
out$title <- replace_non_ascii2(out$title)
out$acknowledgement <- replace_non_ascii2(out$acknowledgement)
# also for out$coi, out$names_last etc.....


# sort on type of acknowledgement
out$fund <- out$auCont <- out$coi <- out$thankful <- NA
out[!is.na(out$acknowledgement), c("fund", "auCont", "coi", "thankful") ] <- F
out[grepl("author contributions",out$acknowledgement, ignore.case = T),]$auCont <- T
out[grepl("fund",out$acknowledgement, ignore.case = T) |
      grepl("financial supp",out$acknowledgement, ignore.case = T),]$fund <- T
out[grepl("conflict[s]? of interest|disclosure",out$acknowledgement, ignore.case = T),]$coi <- T
out[grepl("thank", out$acknowledgement, ignore.case = T) | 
      grepl("grate", out$acknowledgement, ignore.case = T) |
      grepl("gratit", out$acknowledgement, ignore.case = T) |
      grepl("acknow", out$acknowledgement, ignore.case = T) |
      grepl("appreciat", out$acknowledgement, ignore.case = T),]$thankful <- T



'
INCLUSIONS
-thank
-grate
-gratitude
-acknow
-appreciation


EXCLUSIONS
none

'
df <- out[!is.na(out$thankful) & out$thankful,]



'
SUBGROUPS
patient/participant/children/families
died/deceased
statistical
editor/reviewer --> slijmbal?
god/allah/jesus/prophet/deo 
dog/rabbit/cat/pet

'
df$animal <- df$jesus <- df$god <- df$reviewer <- df$editor <- df$statistics <- df$died <- df$participant <- F
df[grepl("patient",df$acknowledgement,ignore.case = T) |
     grepl("participant",df$acknowledgement,ignore.case = T) |
     grepl("children",df$acknowledgement,ignore.case = T) |
     grepl("famil",df$acknowledgement,ignore.case = T),]$participant <- T
df[grepl("died",df$acknowledgement,ignore.case = T) |
     grepl("deceased",df$acknowledgement,ignore.case = T) |
     grepl("passed away",df$acknowledgement,ignore.case = T),]$died <- T
df[grepl("statistic",df$acknowledgement,ignore.case = T),]$statistics <- T
df[grepl("editor ",df$acknowledgement,ignore.case = T),]$editor <- T
df[grepl("reviewer",df$acknowledgement,ignore.case = T),]$reviewer <- T
df[grepl(" god ",df$acknowledgement,ignore.case = T) |
     grepl(" allah ",df$acknowledgement,ignore.case = T) |
     grepl(" prophet ",df$acknowledgement,ignore.case = T) |
     grepl(" buddha ",df$acknowledgement,ignore.case = T) |
     grepl(" deo ",df$acknowledgement,ignore.case = T),]$god <- T
df[grepl(" jesus ",df$acknowledgement,ignore.case = T),]$jesus <- T 
df[grepl(" dog ",df$acknowledgement,ignore.case = T) |
     grepl("rabbit",df$acknowledgement,ignore.case = T) |
     grepl(" cat ",df$acknowledgement,ignore.case = T) |
     grepl("goldfish",df$acknowledgement,ignore.case = T),]$animal <- T




'
voor analyse:
geslacht eerste (en laatste) auteur
land van eerste auteur
jaar van pub
medical discipline
IF
'




# write output
write.csv(out, "xml_data.csv", row.names = F)
write.csv(df, "acknowledgements.csv", row.names = F)


# quit R session
q( save = "no" )