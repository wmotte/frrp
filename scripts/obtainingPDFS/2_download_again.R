
setwd( "/mnt/data/live02/stress/hlamberink/pdf" ) 
.libPaths( c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) )     


library( stringr )


################################################################
# custom functions
################################################################

get.pdflink <- function( doc )
{
  v <- tryCatch(
    {
      y <- readLines( doc )
      z <- y[ grep( "pdf", y ) ]
      
      # only continue if any line actually contains 'pdf'
      if( length( z ) > 0 )
      {
        a <- strsplit(z[1], ' ' )[[1]]
        b <- a[ grep( "pdf", a ) ]
        c <- b[ grep( "http", b) ]
        if( length( b ) > 0 )
        {
          d <- strsplit(b, "=")[[1]][2]
          e <- gsub( "\"", "", d )
          
          
          out <- data.frame( pmid = doc, 
                             pdflink = c,
                             stringsAsFactors = FALSE )
        } else { out <- data.frame( pmid = doc, pdflink = "missing", stringsAsFactors = FALSE ) }
      } else {
        out <- data.frame( pmid = doc,
                           pdflink = "not available",
                           stringsAsFactors = FALSE )
      }
      
      return( out )
    },
    warning = function( war ){
      #warz <- message( war )
    },
    error = function( err ){
      #errorz <- message( err )
    }
  )
}


download.newlinks <- function( indata, outdr = outdir )
{
  v <- tryCatch(
    {
      outpdf <- indata$outpdf
      outfile <- paste0( outdr, "/", outpdf )
      
      download.file( url = indata$pdflink, destfile = outfile,
                     mode = "wb", quiet = TRUE )
    },
    error = function( err ) {
      #
    }
    
  )
}

################################################################
# end custom functions
################################################################

outdir <- "pdfs_replace"


# create list of all pdf files
#filez <- list.files( "pdf/pdfs" )

# vehicle
#df <- NULL

#max <- length( filez )

# loop over files
#for(i in 1:max )
#{
#  if( i %in% seq( 0, 200000, 1000) ) print( paste( i, "of", max ) )
#  
#  # pdf nr
#  pdfnr <- filez[i]
#  
#  # read document
#  doc <- paste0( "pdf/pdfs/", pdfnr )
#  
#  # read first line
#  x <- readLines( doc, n=1)
#
#  if( length( x ) > 0 ){
#    if( grepl( "PDF", x ) == FALSE )
#    {
#      # read new data and bind to df
#      newrow <- get.pdflink( doc )
#      df <- rbind( df, newrow )
#    } 
#  }
#}

#df$pmid <- as.character(df$pmid)
#df$pdflink <- as.character(df$pdflink)

# summarize df
#missings <- df[ df$pdflink == "missing" | is.na( df$pdflink ), ]
#notAvailable <- df[ df$pdflink == "not available", ]
#links <- df[ !is.na( df$pdflink ) & df$pdflink != "missing" & df$pdflink != "not available", ]

#write.csv2( links, "additional_toDownload.csv", row.names=F)
links <- read.csv2( "additional_toDownload.csv", stringsAsFactors = F)

# prepare links for downloading
links$pdflink <- sapply( links$pdflink, gsub, pattern = "/>", replacement = "" )
links$outpdf <- sapply( links$pmid, gsub, pattern = "U:/data/pdf/pdfs/", replacement = "" )

# remove links that do not function
links <- links[ !grepl( "birpublications", links$pdflink ), ]
links <- links[ !grepl( "news", links$pdflink ), ]
links <- links[ !grepl( "press-release", links$pdflink ), ]
links <- links[ !grepl( "0;URL", links$pdflink ), ]
links <- links[ !grepl( "thieme-connect.de", links$pdflink), ]
links <- links[ !grepl( "pdf.png", links$pdflink ), ]

print( length( links[ ,1 ] ) )

# download new links again
for( rownr in 1:length( links[ ,1 ] ) )
{
  dd <- links[ rownr, ]
  print(dd)
  download.newlinks( dd )
  
}

warnings()

