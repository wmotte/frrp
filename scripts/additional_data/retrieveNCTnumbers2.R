################################################################################
# aim: retrieve NCT numbers from csv file and xml text data
#
# contact: h.j.lamberink@umcutrecht.nl
# date: 4-4-2018
################################################################################

.libPaths( c( .libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) )

# load libraries
library( stringr )
library( plyr )


#########################
# FUNCTIONS
#########################

###
# extract nct number from large xml file
##
extract_nct_xml <- function( pmidlist )#, filelist1, filelist2 )
{
#  filez1 <- as.numeric( gsub( ".xml", "", filelist1 ) )
#  filez2 <- as.numeric( gsub( ".xml", "", filelist2 ) )
  
  # define NCT structure
  nct <- "NCT[0-9]{8}"
  
  # prepare vehicle
  fromXML <- NULL
  
  
  for( i in 1:length(pmidlist[,1] ) )
  {
    pmid <- pmidlist[ i, "pmid" ]
    

    if( i %in% seq( 0, 330000, 1000 ) ) print( paste( i, "/ 323402 xml files finished at", Sys.time() ) )
    

    
    if( pmidlist[ i, "fulltext" ] == "fulltext" )
    {
      if( grepl( "pdf/pdfNEW", pmidlist[i, "source" ] ) )
      {
        dir <- "/mnt/data/live02/stress/hlamberink/grobid/xmloutNEW/"
      } else if( pmidlist[i, "source" ] == "pdf/pdf" ) {
        dir <- "/mnt/data/live02/stress/hlamberink/grobid/xmlout/"
      } else if( pmidlist[i, "source" ] == "scopus/pdf" ) {
        dir <- "/mnt/data/live02/stress/hlamberink/scopus/xmlGrobid/"
      }
      
      path <- paste0( dir, pmid, ".xml" )
      
      txt <- readLines( path )
      number <- str_extract_all( txt, nct )

      
      fromX <- NULL
      for( i in 1:length(number) )
      {
        if( length( number[[i]] ) > 0 )
        {
          fromX <- c( fromX, number[[i]] )
        }
      }
      fromX <- gsub( "\\s", "", fromX )
      fromX <- unique( fromX )
      
      if( length(fromX) == 0 ) fromX <- NA
      
      if( length(fromX) > 1 ) {

        # in case of multiple NCT numbers in full-text, use the one that matches Pubmed and Tiab number 
        if( !is.na( nctnumbers[nctnumbers$pmid == pmid, "fromPubmed" ] ) |
            !is.na( nctnumbers[nctnumbers$pmid == pmid, "fromTiab" ] ) )
        {
          fromPubmed <- nctnumbers[nctnumbers$pmid == pmid, "fromPubmed" ]
          fromTiab <- nctnumbers[nctnumbers$pmid == pmid, "fromTiab" ]
          previous <- unique( fromPubmed, fromTiab )
          
          if( !is.na( previous ) )
          {
            its.a.match <- fromX[ fromX %in% previous ]
            
            fromX <- its.a.match
          }
          
        }
        
        if( length( fromX ) > 1 ) fromX <- paste( "multiple: c(", paste( fromX, collapse = ", " ), ")" )
        if( length( fromX ) == 0 ) fromX <- NA
        
      }

    } else {
      
      fromX <- NA
      
    }
    
    # bind files
    fromXML <- rbind( fromXML, fromX )
  }
  return( fromXML )
}



###
# extract nct number from large text file
##
extract_nct_txt <- function( pmidlist )#, textfilelist )
{
#  textfiles <- as.numeric( gsub( ".txt", "", textfilelist ) )
  
  # define NCT structure
  nct <- "NCT[0-9]{8}"
  
  # prepare vehicle
  fromTxt <- NULL
  
  
  for( i in 1:length(pmidlist[,1]))
  {
    pmid <- pmidlist[ i, "pmid" ]
    
    if( i %in% seq( 0, 330000, 1000 ) ) print( paste( i, "/ 323402 txt files finished at", Sys.time() ) )
    
    
    if( pmidlist[ i, "fulltext" ] == "fulltext" )
    {
      if( grepl( "pdf/pdf", pmidlist[i, "source" ] ) ) {
        dir <- "/mnt/data/live02/stress/hlamberink/pdf/text/"
        
        path <- paste0( dir, pmid, ".txt" )
        
        txt <- readLines( path )
        number <- str_extract_all( txt, nct )
        
        fromT <- NULL
        for( i in 1:length( number ) )
        {
          if( length( number[[i]] ) > 0 )
          {
            fromT <- c( fromT, number[[i]] )
          }
        }
        fromT <- gsub( "\\s", "", fromT )
        fromT <- unique( fromT )
        
        if( length( fromT ) == 0 ) fromT <- NA
        
        if( length( fromT ) > 1 )
        {
          # in case of multiple NCT numbers in full-text, use the one that matches Pubmed and Tiab number 
          if( !is.na( nctnumbers[nctnumbers$pmid == pmid, "fromPubmed" ] ) |
              !is.na( nctnumbers[nctnumbers$pmid == pmid, "fromTiab" ] ) )
          {
            fromPubmed <- nctnumbers[nctnumbers$pmid == pmid, "fromPubmed" ]
            fromTiab <- nctnumbers[nctnumbers$pmid == pmid, "fromTiab" ]
            previous <- unique( fromPubmed, fromTiab )
            
            if( !is.na( previous ) )
            {
              its.a.match <- fromT[ fromT %in% previous ]
              
              fromT <- its.a.match
            }
            
          }
          
          if( length( fromT ) > 1 ) fromT <- paste( "multiple: c(", paste( fromT, collapse = ", " ), ")" )
          if( length( fromT ) == 0 ) fromT <- NA
          
        }
        
      } else if( pmidlist[i, "source" ] == "scopus/pdf" ) {
        dir <- "/mnt/data/live02/stress/hlamberink/scopus/text/"
        
        path <- paste0( dir, pmid, ".txt" )
        
        txt <- readLines( path )
        number <- str_extract_all( txt, nct )
        
        fromT <- NULL
        for( i in 1:length( number ) )
        {
          if( length( number[[i]] ) > 0 )
          {
            fromT <- c( fromT, number[[i]] )
          }
        }
        fromT <- gsub( "\\s", "", fromT )
        fromT <- unique( fromT )
        
        if( length( fromT ) == 0 ) fromT <- NA
        
        if( length( fromT ) > 1 )
        {
          # in case of multiple NCT numbers in full-text, use the one that matches Pubmed and Tiab number 
          if( !is.na( nctnumbers[nctnumbers$pmid == pmid, "fromPubmed" ] ) |
              !is.na( nctnumbers[nctnumbers$pmid == pmid, "fromTiab" ] ) )
          {
            fromPubmed <- nctnumbers[nctnumbers$pmid == pmid, "fromPubmed" ]
            fromTiab <- nctnumbers[nctnumbers$pmid == pmid, "fromTiab" ]
            previous <- unique( fromPubmed, fromTiab )
            
            if( !is.na( previous ) )
            {
              its.a.match <- fromT[ fromT %in% previous ]
              
              fromT <- its.a.match
            }
            
          }
          
          if( length( fromT ) > 1 ) fromT <- paste( "multiple: c(", paste( fromT, collapse = ", " ), ")" )
          if( length( fromT ) == 0 ) fromT <- NA
          
        }
        
      } else {
        
        fromT <- NA
        
      }

      
    } else {

      fromT <- NA
      
    }
    
    # bind files
    fromTxt <- rbind( fromTxt, fromT )
    
  }
  return( fromTxt )
}



#########################
# END FUNCTIONS
#########################


# define NCT structure
nct <- "NCT[0-9]{8}"



# read full datafiles of included studies
datatotal <- read.csv( "/mnt/data/live02/stress/hlamberink/csv/data/datatotal.csv", stringsAsFactors = F )
tiabtotal <- read.csv( "/mnt/data/live02/stress/hlamberink/csv/data/tiabtotal.csv", stringsAsFactors = F )

# prepare dataframe
nctnumbers <- data.frame( pmid = datatotal$pmid,
                          yearpub = datatotal$yearpub,
                          fulltext = datatotal$fulltext,
                          source = datatotal$source,
                          stringsAsFactors = F )

# retrieve NCT numbers in datatotal
nctnumbers$fromPubmed <- str_extract( datatotal$databanknumber, nct )

# retrieve NCT numbers in tiabtotal
nctnumbers$fromTiab <- str_extract( tiabtotal$abstract, nct )

# retrieve NCT numbers in xml files
to.use <- nctnumbers[ , c("pmid", "fulltext", "source")]
nctnumbers$fromXml <- extract_nct_xml( to.use )#, filez1, filez2 )


# retrieve NCT numbers in text files
nctnumbers$fromTxt <- extract_nct_txt( to.use )#, textfiles )
head(nctnumbers, 30)

# write new nctnumbers file
write.csv( nctnumbers, "NCT/nctnumbers.csv", row.names = F )


# for now, set multiple to NA
nctnumbers$fromXmlOrig <- nctnumbers$fromXml
nctnumbers$fromTxtOrig <- nctnumbers$fromTxt
nctnumbers[ grepl( "multiple: ", nctnumbers$fromXml ), "fromXml" ] <- NA
nctnumbers[ grepl( "multiple: ", nctnumbers$fromTxt ), "fromTxt" ] <- NA

# summarize in how many NCT number is present
nctnumbers$present <-  !is.na( nctnumbers$fromPubmed ) | !is.na( nctnumbers$fromTiab ) | !is.na( nctnumbers$fromXml ) | !is.na( nctnumbers$fromTxt ) 
nctnumbers$presentPlusMultiple <-  !is.na( nctnumbers$fromPubmed ) | !is.na( nctnumbers$fromTiab ) | !is.na( nctnumbers$fromXmlOrig ) | !is.na( nctnumbers$fromTxtOrig ) 

table(nctnumbers$present)
table(nctnumbers$presentPlusMultiple)

# summarize nct numbers present
df <- ddply( nctnumbers[nctnumbers$fulltext == "fulltext",], .(yearpub), summarize,
             numberNCT = table( present )[2],
             numbernoNCT = table( present )[1] )
df[ is.na( df$numberNCT ), "numberNCT" ] <- 0
df$inclusions <- df$numberNCT + df$numbernoNCT
totalstudies <- ddply( nctnumbers, .(yearpub), summarize,
                       TOTALRCTs = length( fulltext ) )
df <- merge( df, totalstudies, by = "yearpub", all = TRUE )
df[ is.na(df) ] <- 0
columnTotals <- data.frame( yearpub = "total",
                            numberNCT = sum( df$numberNCT ),
                            numbernoNCT = sum( df$numbernoNCT ),
                            inclusions = sum( df$inclusions ),
                            TOTALRCTs = sum( df$TOTALRCTs) )
df <- rbind( df, columnTotals )
df$propNCT <- round( 100 * df$numberNCT / df$inclusions, 1 )

outPerYear <- df[ , c( "yearpub", "TOTALRCTs", "inclusions", "numberNCT", "propNCT")]
print( outPerYear )



# combine columns
nctnumbers$NCT <- NA
for( i in 1:length(nctnumbers[,1]))
{
  df <- nctnumbers
  numbers <- c( df[ i, "fromPubmed" ], df[ i, "fromTiab" ], df[ i, "fromXml" ], df[ i, "fromTxt" ] )
  numbers <- numbers[ !is.na(numbers) ]
  numbers <- unique( numbers )
  
  if( length( numbers ) != 0 ) 
  {
    if( length( numbers ) == 1 )
    {
      nctnumbers[ i, "NCT" ] <- numbers
    } else {
      nctnumbers[ i, "NCT" ] <- "multiple"
    }
    
  }
  
  if( grepl( "multiple: ", nctnumbers[ i, "fromXmlOrig" ] ) ) nctnumbers[ i, "NCT" ] <- "multiple"
  if( grepl( "multiple: ", nctnumbers[ i, "fromTxtOrig" ] ) ) nctnumbers[ i, "NCT" ] <- "multiple"
  
}

head(nctnumbers,30)

# how many studies have NCT number available?
sum( !is.na(nctnumbers$NCT ) )

# how many of those have multiple NCT numbers?
sum( grepl( "multiple", nctnumbers$NCT ) )



write.csv( nctnumbers, "NCT/nctnumbersOverall.csv", row.names = F )
write.csv2( outPerYear, "NCT/nctnumbersYearSummary.csv", row.names = F )


# quit R session
q( save = "no" )