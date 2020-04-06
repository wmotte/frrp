#
# contact: h.j.lamberink@umcutrecht.nl
# date: 2017-12-21
# original source of function: https://github.com/christopherBelter/pubmedXML
#
# aim: convert XML data downloaded from NCBI/pubmed to csv
#################################################################


.libPaths( c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) ) 

library( rentrez )
library( XML )


######################################
# FUNCTIONS
######################################


extract_xmldata <- function( theFile ) 
{
  caughtTry <- tryCatch(
    {
      # parse into nodes and records
      newData <- xmlParse(theFile)
      records <- getNodeSet(newData, "//PubmedArticle")
      
      # for every item extract data
      pmid <- xpathSApply(newData,"//MedlineCitation/PMID", xmlValue)
      doi <- lapply(records, xpathSApply, ".//ELocationID[@EIdType = \"doi\"]", xmlValue)
      doi[sapply(doi, is.list)] <- NA
      doi <- unlist(doi)
      if( is.na( doi ))
      {
        doi <- lapply(records, xpathSApply, "//PubmedData/ArticleIdList/ArticleId[@IdType='doi']", xmlValue)
        doi[sapply(doi, is.list)] <- NA
        doi <- unlist(doi)
      }
      authLast <- lapply(records, xpathSApply, ".//Author/LastName", xmlValue)
      authLast[sapply(authLast, is.list)] <- NA
      authInit <- lapply(records, xpathSApply, ".//Author/Initials", xmlValue)
      authInit[sapply(authInit, is.list)] <- NA
      authors <- mapply(paste, authLast, authInit, collapse = "|")
      firstnames <- lapply(records, xpathSApply, ".//Author/ForeName", xmlValue)
      firstnames[sapply(firstnames, is.list)] <- NA
      firstnames <- sapply(firstnames, paste, collapse = "|")
      affiliations <- lapply(records, xpathSApply, ".//Author/AffiliationInfo/Affiliation", xmlValue)
      affiliations[sapply(affiliations, is.list)] <- NA
      affiliations <- sapply(affiliations, paste, collapse = "|")
      affiliations <- sapply(strsplit(affiliations, "|", fixed = TRUE), unique)
      affiliations <- sapply( affiliations, gsub, pattern = "; ", replacement = "|" )
      affiliations <- paste( affiliations, collapse = "|" )
      yearpub <- lapply(records, xpathSApply, ".//PubDate/Year", xmlValue) 
      yearpub[sapply(yearpub, is.list)] <- NA
      yearpub <- unlist(yearpub)
      articletitle <- lapply(records, xpathSApply, ".//ArticleTitle", xmlValue) 
      articletitle[sapply(articletitle, is.list)] <- NA
      articletitle <- unlist(articletitle)
      journal <- lapply(records, xpathSApply, ".//ISOAbbreviation", xmlValue) 
      journal[sapply(journal, is.list)] <- NA
      journal <- unlist(journal)
      jcountry <- lapply(records, xpathSApply, ".//MedlineJournalInfo/Country", xmlValue)
      jcountry[sapply(jcountry, is.list)] <- NA
      jcountry <- unlist(jcountry)
      volume <- lapply(records, xpathSApply, ".//JournalIssue/Volume", xmlValue)
      volume[sapply(volume, is.list)] <- NA
      volume <- unlist(volume)
      issue <- lapply(records, xpathSApply, ".//JournalIssue/Issue", xmlValue)
      issue[sapply(issue, is.list)] <- NA
      issue <- unlist(issue)
      pages <- lapply(records, xpathSApply, ".//MedlinePgn", xmlValue)
      pages[sapply(pages, is.list)] <- NA
      pages <- unlist(pages)
      abstract <- lapply(records, xpathSApply, ".//Abstract/AbstractText", xmlValue)
      abstract[sapply(abstract, is.list)] <- NA
      abstract <- sapply(abstract, paste, collapse = "|")
      absBackgr <- lapply(records, xpathSApply, ".//AbstractText[@NlmCategory = \"BACKGROUND\"]", xmlValue)
      absBackgr[sapply(absBackgr, is.list)] <- NA
      absBackgr <- sapply(absBackgr, paste, collapse = "|")
      absObject <- lapply(records, xpathSApply, ".//AbstractText[@NlmCategory = \"OBJECTIVE\"]", xmlValue)  
      absObject[sapply(absObject, is.list)] <- NA
      absObject <- sapply(absObject, paste, collapse = "|")
      absMethod <- lapply(records, xpathSApply, ".//AbstractText[@NlmCategory = \"METHODS\"]", xmlValue)
      absMethod[sapply(absMethod, is.list)] <- NA
      absMethod <- sapply(absMethod, paste, collapse = "|")
      absResult <- lapply(records, xpathSApply, ".//AbstractText[@NlmCategory = \"RESULTS\"]", xmlValue)
      absResult[sapply(absResult, is.list)] <- NA
      absResult <- sapply(absResult, paste, collapse = "|")
      absConclu <- lapply(records, xpathSApply, ".//AbstractText[@NlmCategory = \"CONCLUSIONS\"]", xmlValue)
      absConclu[sapply(absConclu, is.list)] <- NA
      absConclu <- sapply(absConclu, paste, collapse = "|")
      meshHeadings <- lapply(records, xpathSApply, ".//DescriptorName", xmlValue)
      meshHeadings[sapply(meshHeadings, is.list)] <- NA
      meshHeadings <- sapply(meshHeadings, paste, collapse = "|")
      grantAgency <- lapply(records, xpathSApply, ".//Grant/Agency", xmlValue)
      grantAgency[sapply(grantAgency, is.list)] <- NA
      grantAgency <- sapply(grantAgency, paste, collapse = "|")
      grantAgency <- sapply(strsplit(grantAgency, "|", fixed = TRUE), unique)
      grantAgency <- paste( grantAgency, collapse = "|")
      grantNumber <- lapply(records, xpathSApply, ".//Grant/GrantID", xmlValue)
      grantNumber[sapply(grantNumber, is.list)] <- NA
      grantNumber <- sapply(grantNumber, paste, collapse = "|")
      grantCountry <- lapply(records, xpathSApply, ".//Grant/Country", xmlValue)
      grantCountry[sapply(grantCountry, is.list)] <- NA
      grantCountry <- sapply(grantCountry, paste, collapse = "|")
      grantCountry <- sapply(strsplit(grantCountry, "|", fixed = TRUE), unique)
      grantCountry <- paste( grantCountry, collapse = "|")
      ptype <- lapply(records, xpathSApply, ".//PublicationType", xmlValue)
      ptype[sapply(ptype, is.list)] <- NA
      ptype <- sapply(ptype, paste, collapse = "|")
      lang <- lapply(records, xpathSApply, ".//Language", xmlValue)
      lang[sapply(lang, is.list)] <- NA
      lang <- sapply(lang, paste, collapse = "|")
      collective <- lapply(records, xpathSApply, ".//CollectiveName", xmlValue)
      collective[sapply(collective, is.list)] <- NA
      collective <- sapply(collective, paste, collapse = "|")
      funding <- lapply(records, xpathSApply, ".//AbstractText[@Label = \"FUNDING\"]", xmlValue)
      funding[sapply(funding, is.list)] <- NA
      funding <- sapply(funding, paste, collapse = "|")
      databankname <- lapply(records, xpathSApply, ".//DataBankName", xmlValue)
      databankname[sapply(databankname, is.list)] <- NA
      databankname <- sapply(databankname, paste, collapse = "|")
      databanknumber <- lapply(records, xpathSApply, ".//AccessionNumber", xmlValue)
      databanknumber[sapply(databanknumber, is.list)] <- NA
      databanknumber <- sapply(databanknumber, paste, collapse = "|")
      keyword <- lapply(records, xpathSApply, ".//Keyword", xmlValue)
      keyword[sapply(keyword, is.list)] <- NA
      keyword <- sapply(keyword, paste, collapse = "|")
      
      theDF <- data.frame(pmid, doi, authors, firstnames, yearpub, articletitle, journal, 
                          jcountry, volume, issue, pages, abstract, absBackgr, absObject, 
                          absMethod, absResult, absConclu, lang, meshHeadings, keyword,
                          grantAgency, grantNumber, grantCountry, ptype, affiliations, 
                          collective, funding, databankname, databanknumber,
                          stringsAsFactors = FALSE)
      
      # replace every instance of ';' by ':'
      theDF <- as.data.frame(t(as.data.frame( sapply( theDF, gsub, pattern = ";", replacement = ":" ), stringsAsFactors = F )))
      
      return(theDF)
    },
    
    error=function(err) {
      #message(paste("URL does not seem to exist:", url))
      #message("Here's the original error message:")
      message(err)
      # Choose a return value in case of error
      return( paste( err, pmid ) )
    },
    
    warning=function(war) {
      #message(paste("URL caused a warning:", url))
      #message("Here's the original warning message: ")
      message(paste( war, "\n" ) )
      # Choose a return value in case of warning
      #return( paste( pmid, "warning, test if downloaded" ) )
    }
    #finally={
    # NOTE:
    # Here goes everything that should be executed at the end,
    # regardless of success or error.
    # If you want more than one expression to be executed, then you 
    # need to wrap them in curly brackets ({...}); otherwise you could
    # just have written 'finally=<expression>' 
    #message(paste("Processed URL:", url))
    #message("Some other message at the end")
    #}
  )
} 


######################################
# END FUNCTIONS
######################################

# read input data
pmidlist <- read.csv2( "new_rcts_pmidsonly_2017-03-19.csv", stringsAsFactors = FALSE )


# set i, e and f (start and end point) (this will be the 23rd dataset)
i <- 23
e <- 1
f <- length( pmidlist[,1] )
print( paste("i:", i, "... loading pmids", e, "until", f) )

# colnames
small.file <- as.data.frame( t( as.data.frame( c( "pmid", "doi", "yearpub", "journal", "lang" ) ) ) )
tiab.file <- as.data.frame( t( as.data.frame( c( "pmid", "articletitle", "abstract", "journal" ) ) ) )
noabs.file <- as.data.frame( t( as.data.frame( c( "pmid", "doi", "authors", "firstnames", "yearpub", "articletitle",  
                                                  "journal", "jcountry", "volume", "issue", "pages", "lang",
                                                  "meshHeadings", "keyword", "grantAgency", "grantNumber", "grantCountry", "ptype",
                                                  "affiliations", "collective", "funding", "databankname", "databanknumber" ) ) ) )

# set starting number (start where previous system ended)
startnr <- e



# adjust startnumber in case the file already exists
if( file.exists( paste0( "csvoutNEW/pubmeddata_limited", i, ".csv" ) ) )
{
  a <- read.csv2( paste0( "csvoutNEWNEW/pubmeddata_limited", i, ".csv" ) )
  startnr <- e + length( a[,1] )
  
  # length of first file is different, therefore, add 1 to startnr
  if( i == 1 ){ startnr <- e + length( a[,1] ) + 1 }
}

# print status of file
print(paste("e =", e, ";  f =", f, "; startnr =", startnr))


# set progress bar
progbar <- txtProgressBar( min = e, max = f, style = 3 )



# prepare files
if( !file.exists( paste0( "csvoutNEW/pubmeddata", i, ".csv" ))) 
{
  write.table( noabs.file, paste0( "csvoutNEW/pubmeddata", i, ".csv" ), 
               sep = ";", dec = ",", row.names = FALSE, col.names = FALSE, append = FALSE )
}
if( !file.exists( paste0( "csvoutNEW/pubmeddata_limited", i, ".csv" ))) 
{
  write.table( small.file, paste0( "csvoutNEW/pubmeddata_limited", i, ".csv" ), 
               sep = ";", dec = ",", row.names = FALSE, col.names = FALSE, append = FALSE )
}
if( !file.exists( paste0( "csvoutNEW/pubmeddata_tiab", i, ".csv" ))) 
{
  write.table( tiab.file, paste0( "csvoutNEW/pubmeddata_tiab", i, ".csv" ), 
               sep = ";", dec = ",", row.names = FALSE, col.names = FALSE, append = FALSE )
}


# loop over pmids and save per 20000
for( b in startnr:10)#f )
{
  # after every 50 retrievals, take a break of (on average) 60 seconds
  if( b %in% seq(e,f,50) ) Sys.sleep( rnorm(1, mean=60, sd=10 ) ) 
  
  # update progressbar
  setTxtProgressBar( progbar, b )
  
  # get xml data from pubmed database
  pmid <- pmidlist$pmid[ b ]
  xmldata <- entrez_fetch( db = "pubmed", id = pmid, rettype = "xml")
  
  # convert data to dataframe
  theData <- extract_xmldata( xmldata )
  
  # select limited number of columns for smaller basic file
  small.file <- c( "pmid", "doi", "yearpub", "journal", "lang" )
  tiab.file <- c( "pmid", "articletitle", "abstract", "journal" )
  noabs.file <- names( theData[ !names(theData) %in% c( "abstract", "absBackgr", "absObject", "absMethod", "absResult", "absConclu" )] )
  
  # write data
  write.table( theData[ , noabs.file ], paste0( "csvoutNEW/pubmeddata", i, ".csv" ), 
               sep = ";", dec = ",", row.names = FALSE, col.names = FALSE, append = TRUE )
  write.table( theData[ , small.file ], paste0( "csvoutNEW/pubmeddata_limited", i, ".csv" ), 
               sep = ";", dec = ",", row.names = FALSE, col.names = FALSE, append = TRUE )
  write.table( theData[ , tiab.file ], paste0( "csvoutNEW/pubmeddata_tiab", i, ".csv" ), 
               sep = ";", dec = ",", row.names = FALSE, col.names = FALSE, append = TRUE )
}
    

# quit R session
q( save = "no" )