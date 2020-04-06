#
# contact: h.j.lamberink@umcutrecht.nl
# date: 2018-05-16
#
# aim: convert XML data from grobid file to data
#################################################################


#.libPaths( c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) ) 


library( XML )
library( plyr )
#library( tei2r )
library( stringr )


######################################
# FUNCTIONS
######################################

xmlG <- readLines("O:/Psychiatrie/Predictie verantwoordonderzoek/testdata/10021470.xml")
theFile <- xmlG
theFile <- gsub( "\t", "", theFile)
theFile <- c("<TEI>", theFile[c(6:length(xmlG))])





###
# convert words to numbers
##
word2num <- function(word){
  wsplit <- strsplit(tolower(word)," ")[[1]]
  one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,
                     six=6, seven=7, eight=8, nine=9)
  teens <- list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,
                sixteen=16, seventeen=17, eighteen=18, nineteen=19)
  ten_digits <- list(ten=10, twenty=20, thirty=30, forty=40, fifty=50,
                     sixty=60, seventy=70, eighty=80, ninety=90)
  doubles <- c(teens,ten_digits)
  out <- 0
  i <- 1
  while(i <= length(wsplit)){
    j <- 1
    temp <- 0
    if(i==1 && wsplit[i]=="hundred")
      temp <- 100
    else if(i==1 && wsplit[i]=="thousand")
      temp <- 1000
    else if(wsplit[i] %in% names(one_digits))
      temp <- as.numeric(one_digits[wsplit[i]])
    else if(wsplit[i] %in% names(teens))
      temp <- as.numeric(teens[wsplit[i]])
    else if(wsplit[i] %in% names(ten_digits))
      temp <- (as.numeric(ten_digits[wsplit[i]]))
    if(i < length(wsplit) && wsplit[i+1]=="hundred"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 100*temp
      else
        out <- 100*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1]=="thousand"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 1000*temp
      else
        out <- 1000*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
      temp <- temp*100
      out <- out + temp
    }
    else{
      out <- out + temp
    }
    i <- i + j
  }
  #return(list(word,out))
  return(out)
}

text <- "thousand and one nights"
text <- "seven, three and four, respectively. Seven were for"
text <- "one hundred twenty-one"

###
# replace numbers written as words by actual numbers
##
replaceNumbers <- function(text){
  
  a <- stringr::str_extract_all(tolower(text), "(one|two|three|four|five|six|seven|eight|nine|zero|ten|eleven|twelve|thirteen|fourteen|forteen|fifteen|sixteen|seventeen|eighteen|nineteen|twenty|thirty|fourty|forty|fifty|sixty|seventy|eighty|ninety|hundred|thousand)((\\s|-)(and|one|two|three|four|five|six|seven|eight|nine|zero|ten|eleven|twelve|thirteen|fourteen|forteen|fifteen|sixteen|seventeen|eighteen|nineteen|twenty|thirty|fourty|forty|fifty|sixty|seventy|eighty|ninety|hundred|thousand)+){0,10}")[[1]]
  a2 <- gsub( "-", " ", a )
  
  a2 <- lapply(a2, 
               function(x) {
                 
                 if( !grepl("\\s", x) ) {
                   s <- x
                 } else if( !grepl("and", x) ) {
                   s <- x
                 } else { 
                   s <- str_split( x, " " )[[1]]
                   if( s[grep("and", s) - 1] %in% c("hundred", "thousand")){
                     s <- paste(s, collapse = " ")
                   } else {
                     s <- s[-grep("and",s)]
                   }
                }
              }
            )
  a2 <- unlist(a2)
  
  b <- sapply(a2, word2num)
  
  if( length(b) == 0 ){
    out <- text
  } else if( length(b) == 1 ) {
    out <- sub( a, b, text )
  } else {
    ifelse(length(b)==length(a), c <- a, c <- a2 )
    out <- tolower(text)
    for( i in 1:length(b) ) {
      out <- sub( c[i], b[i], out )
    }
  }
  return(out)
}


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
      body <- as.character(xpathSApply(newData,"//text/body", xmlValue))
      # example 1: string takes 3 words before and after 'volunteers'
      # stringr::str_extract_all(tolower(body), "([^\\s]+\\s){3}volunteers(\\s[^\\s]+){3}")
      # example 2: string takes the full sentence from full stop to full stop around the word "were enrolled"
      # stringr::str_extract_all(tolower(body), "\\.([^\\s]+\\s){1,}were enrolled(\\s[^\\s]+){0,}\\.")[[1]]
      
      #volunteers <- 
      text <- stringr::str_extract_all(tolower(body), "\\.([^\\s]+\\s){0,30}were enrolled(\\s[^\\s]+)")[[1]]
      
      ###################
      #################
      ######determine possible terms related to sample size###############
      ######extract the number of participants###############
      ##################
      ##############
      
      #methodSection
      
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
      
      
      au1 <- xpathSApply(newData,"//ce:surname", xmlValue)
      #au2
      aff1 <- xpathSApply(newData,"//ce:affiliation[id='aff1']", xmlValue)
      #aff2
      
      
      #country
      #finSupport
      
      # replace empty values by NA
      if( length(title) == 0 ) title <- NA
      if( length(acknowledgement) == 0 ) acknowledgement <- NA
      if( length(coi) == 0 ) coi <- NA
      
      
      theDF <- data.frame( 
        title = title,
        acknowledgement = acknowledgement,
        year = year,
        #coi = coi,
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


######################################
# END FUNCTIONS
######################################

# read input data
'
xmlG <- readLines("O:/Psychiatrie/Predictie verantwoordonderzoek/testdata/29212736.xml")
theFile <- xmlG
theFile <- gsub( "\t", "", theFile)
theFile <- c("<TEI>", theFile[c(6:length(xmlG))])
xmlS <- readLines("O:/Psychiatrie/Predictie verantwoordonderzoek/testdatascopus/12583944.xml")
theFile <- xmlS

'

filezG <- list.files("O:/Psychiatrie/Predictie verantwoordonderzoek/testdata")
filezS <- list.files("O:/Psychiatrie/Predictie verantwoordonderzoek/testdatascopus")
pmidsG <- gsub( ".xml", "", filezG)
pmidsS <- gsub( ".xml", "", filezS)

# acknowledgement from GROBID xml
system.time(Gout <- lapply(filezG, FUN = function(x){
  print(x)
  xml <- readLines(paste0("O:/Psychiatrie/Predictie verantwoordonderzoek/testdata/", x))
  xml <- gsub( "\t", "", xml)
  
  if( length( xml ) > 1 ) {
    xml <- c("<TEI>", xml[c(6:length(xml))])
    extract_grobidXML(xml)
  } else {
    NA
  }
} ))

# acknowledgement from scopus xml
system.time(Sout <- lapply(filezS, FUN = function(x){
  print(x)
  xml <- readLines(paste0("O:/Psychiatrie/Predictie verantwoordonderzoek/testdatascopus/", x))
  extract_scopusXML(xml)
} ))

# to dataframe
outG <- ldply( Gout, data.frame )
outS <- ldply( Sout, data.frame )

outG$pmid <- pmidsG
outS$pmid <- pmidsS

outG$source <- "grobid"
outS$source <- "scopus"

# combine
out <- rbind(outG,outS)

# sort on type of acknowledgement
out$fund <- out$auCont <- out$coi <- NA
out[grepl("author contributions",out$acknowledgement, ignore.case = T),]$auCont <- T
out[grepl("funding",out$acknowledgement, ignore.case = T),]$fund <- T
out[grepl("conflict[s]? of interest|disclosure",out$acknowledgement, ignore.case = T),]$coi <- T




'
Voeg toe aan zoektermen:
inclusie
thank
grateful
gratitude
acknowledg[e]?


exclusie
??

subgroepen
patient/participant
died/deceased etc
statistical
specific names? extractie van namen.....
colleagues
editor/reviewer --> slijmbal?

'


'
voor analyse:
geslacht eerste (en laatste) auteur
land van eerste auteur
jaar van pub
medical discipline
IF
(sample size)
'



table(out$fund)
table(out$auCont)
table(out$coi)

# write output
write.csv2(out, "testdata.csv", row.names = F)


# quit R session
#q( save = "no" )