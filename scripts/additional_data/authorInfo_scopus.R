################################################################################
# aim: determine h-index based on Scopus (web of science) data
#
# contact: h.j.lamberink@umcutrecht.nl
# date: march 27, 2018
################################################################################

.libPaths( c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) ) 


library( 'rscopus' )
library( 'httr' )

######################################
# FUNCTIONS
######################################

source( "author_functions.R" )

######################################
# END FUNCTIONS
######################################

## API key
set_api_key("6a3419d8f660b249f72c16ab565cf993")
api_key <- "6a3419d8f660b249f72c16ab565cf993"

# read data of title and abstract
tiab <- read.csv( "/mnt/data/live02/stress/hlamberink/csv/data/tiabtotal.csv",
                  stringsAsFactors = FALSE )



# extract first and last name of author from author file
if( file.exists( "authors_names_split.csv" ) ) {
  
  df <- read.csv( "authors_names_split.csv", stringsAsFactors = FALSE )
  
} else {
  
  # read author information
  df <- read.csv( "/mnt/data/live02/stress/hlamberink/csv/data/authors.csv",
                  stringsAsFactors = FALSE )
  
  # add publication year
  years <- read.csv( "/mnt/data/live02/stress/hlamberink/csv/data/journals.csv",
                     stringsAsFactors = FALSE )
  years <- years[ , c( "pmid", "yearpub" ) ]
  df <- merge( df, years, 
               by = "pmid", all = TRUE ) 
  
  for( i in 1:length(df[,1])) {
    # gather first and last author
    s <- strsplit( df[i,]$authors, split = "\\|" )[[1]]
    df[ i, "authorOne" ] <- replace_non_ascii2( s[1] )
    df[ i, "authorLast" ] <- replace_non_ascii2( s[ length(s) ] )
    
    # gather last and first names of author 
    sFname <- strsplit( df[i,]$firstnames, split = "\\|" )[[1]]
    df[ i, "authorOne_fn" ] <- replace_non_ascii2( sFname[1] )
    df[ i, "authorLast_fn" ] <- replace_non_ascii2( sFname[ length(sFname) ] )
    
    sOne <- strsplit( df[i,]$authorOne, split = " " )[[1]]
    sLast <- strsplit( df[i,]$authorLast, split = " " )[[1]]
    
    if( length( sOne ) == 2 ) {
      df[ i, "authorOne_ln" ] <- sOne[1]
    } else {
      df[ i, "authorOne_ln" ] <- paste( sOne[ c(1:(length(sOne) - 1 ) ) ], collapse = " " )
    }
    
    if( length( sLast ) == 2 ) {
      df[ i, "authorLast_ln" ] <- sLast[1]
    } else {
      df[ i, "authorLast_ln" ] <- paste( sLast[ c(1:(length(sLast) - 1 ) ) ], collapse = " " )
    }
    
    # if first names are only initials, use sOne and sLast data
    a <- strsplit( sFname, split = " " )
    
    if( all(!is.na(a)) ) {
      if( nchar( a[[1]][1] ) == 1 & 
          nchar( a[[1]][1]) == nchar( a[[length(a)]][1] ) ) {
        
        
        # get initials
        so <- strsplit( sOne[length(sOne)], "|" )
        sl <- strsplit( sLast[length(sLast)], "|" )
        
        # add dots behind initials
        df[ i, "authorOne_fn" ] <- paste0( so[[1]], ".", collapse = "" )
        df[ i, "authorLast_fn" ] <- paste0( sl[[1]], ".", collapse = "" )
      }
    }
  }
  write.csv( df, "authors_names_split.csv", row.names = F )
}




# set progress bar
if( file.exists("startnr.csv" ) ) {
  startnr <- read.csv("startnr.csv")
  startnr <- startnr[1,1]
} else { startnr <- 1 }
maximum <- length(df[,1])
progbar <- txtProgressBar( min = 0, max = maximum, style = 3 )



# add h-index and number of collaborators
for( i in startnr:maximum ) {
  d <- tryCatch(
    {

      # write startnr.csv (in case of error, next time start here)
      write.csv( i, "startnr.csv", row.names = F )
      
      # set progress bar
      setTxtProgressBar( progbar, i )
      
      # number of authors in RCT
      authors <- strsplit( df[ i, "authors" ], "\\|" )[[1]] 
      df[ i, "nAuthors" ] <- length( authors )
      
      # prepare names of first author and yearpub
      lastname1 <- df[ i, "authorOne_ln" ]
      firstname1 <- df[ i, "authorOne_fn" ]
      yearpub <- df[ i, "yearpub" ]
      title <- tiab[ tiab$pmid == df[ i, "pmid" ], "articletitle" ]
      
      
      
      ########### FIRST AUTHOR INFORMATION ########################################  
      firstINFO <- find.author( last_name = lastname1, 
                                first_name = firstname1, 
                                title = title,
                                verbose = F )
      ##firstINFO[[1]] == entries, DF, auth_name
      ##firstINFO[[2]] == affiliation_scopus
      ##firstINFO[[3]] == affiliation_ID_scopus
      if( !is.null( firstINFO[[2]] ) ) {
        frame.first <- firstINFO[[1]][[2]]
        affil_sc.first <- firstINFO[[2]]
        affil_scID.first <- firstINFO[[3]]
        
      } else {
        affil_sc.first <- NA
        affil_scID.first <- NA
        frame.first <- NULL
      }
      
      
      # from info of first author, extract info other authors and citation count RCT
      if( !is.null(frame.first) ) {
        au_id.last <- NULL
        pub <- frame.first[ agrepl( title, frame.first$title, ignore.case = TRUE ), ]
        if( length( pub[,1] )  == 1 ) {
          df[ i, "ncitations" ] <- as.numeric( pub$citations )
          df[ i, "fullnames" ] <- as.character( pub$name )
          df[ i, "scopusIDs" ] <- as.character( pub$au_id )
          split.authors <- strsplit( as.character( pub$au_id[1] ), ";" )
          au_id.last <- split.authors[[1]][ length(split.authors[[1]]) ]
          au_id.first <- split.authors[[1]][1]
          df[ i, "scopusID.first" ] <- au_id.first
          df[ i, "scopusID.last" ] <- au_id.last
        }
      } else {
        pub <- data.frame(  )
        au_id.last <- NULL
      }
      
      
      # get yearpub if this was missing
      if( is.na( yearpub ) & NROW(pub) == 1 ) {  
        date <- as.character( pub$cover_date )
        yearpub <- as.numeric( substr( date, start = 1, stop = 4 ) )
        if( length(yearpub) == 1 ) df[ i, "yearpub" ] <- yearpub
      }
      
      
      if( all(is.null(frame.first) ) ) frame.first <- NA 
      if( !is.data.frame( frame.first ) ) frame.first <- NA
      if( !is.na(frame.first[1]) ) {
        
        # get affiliation for first author
        df[ i, "affil_sc.first" ] <- affil_sc.first
        df[ i, "affil_scID.first" ] <- affil_scID.first
        
        # get h-index for first author
        h1 <- h_fetcher( frame.first, yearpub ) 
        h2 <- h_fetcher( frame.first, yearpub = 2019 ) 
        df[ i, "h.first" ] <- h1[[1]]
        df[ i, "h.firstCurrent" ] <- h2[[1]]
        df[ i, "nPubs.first" ] <- h1[[2]]
        df[ i, "nPubs.firstCurrent" ] <- h2[[2]]
        
        # get nr of collaborators
        df[ i, "nCollabor.first" ] <- collaborate( frame.first, yearpub )
        
        # get given first names for gender analysis
        names.first <- as.character( tolower( frame.first[1,2] ) )
        nfirst <- strsplit( names.first, split = ";" )[[1]]
        nfirst <- replace_non_ascii2( iconv( nfirst, to = "UTF-8" ) )
        nfirst <- nfirst[ grepl( tolower( lastname1 ), nfirst ) ]
        if( length( nfirst ) > 1 ) {
          initial <- tolower( substr( df[ i, "authorOne_fn" ], start = 1, stop = 1 ) )
          nfirst <- nfirst[ grepl( paste0( "^", initial ), nfirst ) ]
        }
        if( length( nfirst ) > 1 ) nfirst <- nfirst[1]
        nfirst <- gsub( paste0( " ", tolower( lastname1 ) ), "", nfirst )
        if( length( nfirst ) == 0 ) nfirst <- "a."
        if( grepl( "\\s", nfirst ) ) {
          nfirst <- strsplit(nfirst, split = "\\s" )[[1]]
          nfirst <- nfirst[ !grepl( "\\.", nfirst) ]
          if( length( nfirst ) > 1 ) nfirst <- nfirst[1]
          if( length( nfirst ) == 0 ) nfirst <- NA
        }
        if( grepl( "^[a-z]\\.", nfirst ) ) {
          names.first <- frame.first[2]
          names.first[,1] <- as.character( names.first[,1] )
          ROWS <- seq( 1:length(names.first[,1]))
          rmove <- grep( "\\.", names.first$name )
          which.row <- ROWS[ !(ROWS %in% rmove ) ]
          if( length( which.row ) == 0 ) {
            nfirst <- NA
          } else {
            names.first <- as.character( tolower( names.first[which.row[1],1] ) )
            nfirst <- strsplit( names.first, split = ";" )[[1]]
            nfirst <- replace_non_ascii2( iconv( nfirst, to = "UTF-8" ) )
            nfirst <- nfirst[ grepl( tolower( lastname1 ), nfirst ) ]
            if( length( nfirst ) > 1 )
            {
              initial <- tolower( substr( df[ i, "authorOne_fn" ], start = 1, stop = 1 ) )
              nfirst <- nfirst[ grepl( paste0( "^", initial ), nfirst ) ]
            }
            if( length( nfirst ) > 1 ) nfirst <- nfirst[1]
            nfirst <- gsub( paste0( " ", tolower( lastname1 ) ), "", nfirst )
            if( length( nfirst ) == 0 ) nfirst <- NA
            if( grepl( "\\s", nfirst ) )
            {
              nfirst <- strsplit(nfirst, split = "\\s" )[[1]]
              nfirst <- nfirst[ !grepl( "\\.", nfirst) ]
              if( length( nfirst ) > 1 ) nfirst <- nfirst[1]
              if( length( nfirst ) == 0 ) nfirst <- NA
            }
          }
          
        }
        if( length(nfirst) == 1 ) { df[ i, "name_first" ] <- nfirst
        } else { df[ i, "name_first" ] <- NA }
        
        
        # determine academic age and uninterrupted presence first author
        frame.first$year <- as.numeric( substr( frame.first$cover_date, start = 1, stop = 4 ) )
        frame.first <- frame.first[ frame.first$year < yearpub, ]
        if( length( frame.first$year ) > 0 )
        {
          acadAge <- yearpub - min( frame.first$year )
          if( is.na(acadAge) ) acadAge <- 0
          df[ i, "acadAge_first" ] <- acadAge
          d <- 1
          for( j in 1:1000 )
          {
            # if publications exist in previous year, add 1 year to 'd' and continue cycle
            if( sum( grepl( (yearpub - d), frame.first$year ) ) > 0 ) 
            {
              d <- d + 1
              next
              
              # else, stop for loop
            } else { break }
          }
          df[ i, "acadPresence_first" ] <- d - 1 
          
        } else {
          
          df[ i, "acadAge_last" ] <- 0
          df[ i, "acadPresence_first" ] <- 0
        }
      }
      
      #  if( is.na(frame.first[1]) ) {
      #    fnames <- strsplit( firstname1, split = " " )
      #    df[ i, "name_first" ] <- tolower( fnames[[1]][1] )
      #  }
      
      ########### LAST AUTHOR INFORMATION (when 1 author, this is same as first author) ##############  
      lastname2 <- df[ i, "authorLast_ln" ]
      firstname2 <- df[ i, "authorLast_fn" ]
      lastINFO <- NULL
      
      if( is.null(au_id.last) ) {
        lastINFO <- find.author( last_name = lastname2, 
                                 first_name = firstname2, 
                                 title = title, 
                                 verbose = F )
      } else {
        lastINFO <- find.author( au_id.input = au_id.last,
                                 last_name = lastname2,
                                 first_name = firstname2,
                                 title = title, 
                                 verbose = F )
      }
      
      
      if( !is.null( lastINFO[[2]] ) ) {
        frame.last <- lastINFO[[1]][[2]]
        affil_sc.last <- lastINFO[[2]]
        affil_scID.last <- lastINFO[[3]]
      } else {
        affil_sc.last <- NA
        affil_scID.last <- NA
        frame.last <- NULL
      }
      
      
      # get yearpub if this was still missing
      if( is.na( yearpub ) & !is.null(frame.last)  ) {
        title <- tiab[ tiab$pmid == df[ i, "pmid" ], "articletitle" ]
        date <- as.character( frame.last[ agrepl( title, frame.last$title, ignore.case = TRUE ), "cover_date" ] )
        yearpub <- as.numeric( substr( date, start = 1, stop = 4 ) )
        if( length(yearpub) == 1 ) df[ i, "yearpub" ] <- yearpub
      }
      
      
      if( all( is.null( frame.last ) ) ) frame.last <- NA
      if( !is.data.frame( frame.last ) ) frame.last <- NA
      if( !is.na( frame.last[1] ) ) {
        
        # get affiliation for last author
        df[ i, "affil_sc.last" ] <- affil_sc.last
        df[ i, "affil_scID.last" ] <- affil_scID.last
        
        # get h-index last author
        h3 <- h_fetcher( frame.last, yearpub )
        h4 <- h_fetcher( frame.last, yearpub = 2019 )
        df[ i, "h.last" ] <- h3[[1]]
        df[ i, "h.lastCurrent" ] <- h4[[1]]
        df[ i, "nPubs.last" ] <- h3[[2]]
        df[ i, "nPubs.lastCurrent" ] <- h4[[2]]
        
        # get n of collaborators last author
        df[ i, "nCollabor.last" ] <- collaborate( frame.last, yearpub ) 
        
        # get given name last author
        names.last <- as.character( tolower( frame.last[1,2] ) )
        nlast <- strsplit( names.last, split = ";" )[[1]]
        nlast <- replace_non_ascii2( iconv( nlast, to = "UTF-8" ) )
        nlast <- nlast[ grepl( tolower(lastname2), nlast ) ]
        if( length( nlast ) > 1 ) {
          initial <- tolower( substr( df[ i, "authorLast_fn" ], start = 1, stop = 1 ) )
          nlast <- nlast[ grepl( paste0( "^", initial ), nlast ) ]
        }
        if( length( nlast ) > 1 ) nlast <- nlast[1]
        nlast <- gsub( paste0( " ", tolower( lastname2 ) ), "", nlast )
        if( length( nlast ) == 0 ) nlast <- "a."
        if( grepl( "\\s", nlast ) ) {
          nlast <- strsplit(nlast, split = "\\s" )[[1]]
          nlast <- nlast[ !grepl( "\\.", nlast) ]
          if( length( nlast ) > 1 ) nlast <- nlast[1]
          if( length( nlast ) == 0 ) nlast <- NA
        }
        if( grepl( "^[a-z]\\.", nlast ) | is.na( nlast ) ) {
          names.last <- frame.last[2]
          names.last[,1] <- as.character( names.last[,1] )
          ROWS <- seq( 1:length(names.last[,1] ) )
          rmove <- grep( "\\.", names.last$name )
          which.row <- ROWS[ !(ROWS %in% rmove ) ]
          if( length( which.row ) == 0 ) {
            nlast <- NA
          } else {
            names.last <- as.character( tolower( names.last[which.row[1],1] ) )
            
            nlast <- strsplit( names.last, split = ";" )[[1]]
            nlast <- replace_non_ascii2( iconv( nlast, to = "UTF-8" ) )
            nlast <- nlast[ grepl( tolower(lastname2), nlast ) ]
            if( length( nlast ) == 0 ) nlast <- NA
            if( length( nlast ) > 1 )
            {
              initial <- tolower( substr( df[ i, "authorLast_fn" ], start = 1, stop = 1 ) )
              nlast <- nlast[ grepl( paste0( "^", initial ), nlast ) ]
            }
            nlast <- gsub( paste0( " ", tolower( lastname2 ) ), "", nlast )
            if( length( nlast ) > 1 ) nlast <- nlast[1]
            if( grepl( "\\s", nlast ) )
            {
              nlast <- strsplit(nlast, split = "\\s" )[[1]]
              nlast <- nlast[ !grepl( "\\.", nlast) ]
              if( length( nlast ) > 1 ) nlast <- nlast[1]
              if( length( nlast ) == 0 ) nlast <- NA
            }
          }
        }
        
        if( length(nlast) == 1 ) { df[ i, "name_last" ] <- nlast 
        } else { df[ i, "name_last" ] <- NA }
        
        # determine academic age and uninterrupted presence
        frame.last$year <- as.numeric( substr( frame.last$cover_date, start = 1, stop = 4 ) )
        frame.last2 <- frame.last[ frame.last$year < yearpub, ]
        if( length( frame.last2$year ) > 0 ) {
          acadAge <- yearpub - min( frame.last2$year )
          if( is.na(acadAge) ) acadAge <- 0
          df[ i, "acadAge_last" ] <- acadAge
          d <- 1
          for( j in 1:1000 )
          {
            
            if( sum( grepl( (yearpub - d), frame.last2$year ) ) > 0 )
            {
              d <- d + 1
              next
              
              # else, stop for loop
            } else { break }
          }
          df[ i, "acadPresence_last" ] <- d - 1 
          
        } else {
          
          df[ i, "acadAge_last" ] <- 0
          df[ i, "acadPresence_last" ] <- 0
        }
      }
      
      
      
      
      #    if( is.na(frame.last[1]) ) {
      #      fnames <- strsplit( firstname2, split = " " )
      #      df[ i, "name_last" ] <- tolower( fnames[[1]][1] )
      #    }
      
      ########### if first author is missing but last author not, try first author again #############
      ########### based on last. #####################################################################
      if( length( names(df) ) == 43 ) {
        if( is.na(df[ i, "fullnames" ]) & is.na(df[ i, "scopusID.first" ]) & !is.na(df[ i, "acadAge_last"]) ) {
          
          # from info of last author, extract info other authors and citation count RCT
          if( !is.null(frame.last) ) {
            au_id.first <- NULL
            pub <- frame.last[ agrepl( title, frame.last$title, ignore.case = TRUE ), ]
            if( length( pub[,1] ) > 1 ) pub <- pub[1,]
            if( length( pub[,1] )  == 1 ) {
              df[ i, "ncitations" ] <- as.numeric( pub$citations )
              df[ i, "fullnames" ] <- as.character( pub$name )
              df[ i, "scopusIDs" ] <- as.character( pub$au_id )
              split.authors <- strsplit( as.character( pub$au_id[1] ), ";" )
              au_id.last <- split.authors[[1]][ length(split.authors[[1]]) ]
              au_id.first <- split.authors[[1]][1]
              df[ i, "scopusID.first" ] <- au_id.first
              df[ i, "scopusID.last" ] <- au_id.last
            }
          }
          
          
          
          # get yearpub if this was missing
          if( is.na( yearpub ) & length(pub[,1]) == 1 ) {  
            date <- as.character( pub$cover_date )
            yearpub <- as.numeric( substr( date, start = 1, stop = 4 ) )
            if( length(yearpub) == 1 ) df[ i, "yearpub" ] <- yearpub
          }
          
          
          if( !is.null(au_id.first) ) {
            firstINFO <- find.author( au_id.input = au_id.first,
                                      last_name = lastname1,
                                      first_name = firstname1,
                                      title = title, 
                                      verbose = F )
          }
          
          
          if( !is.na( firstINFO[[1]] ) ) {
            frame.first <- firstINFO[[1]][[2]]
            affil_sc.first <- firstINFO[[2]]
            affil_scID.first <- firstINFO[[3]]
          } else {
            affil_sc.first <- NA
            affil_scID.first <- NA
            frame.first <- NULL
          }
          
          if( all( is.null( frame.first ) ) ) frame.first <- NA
          if( !is.data.frame( frame.first ) ) frame.first <- NA
          if( !is.na( frame.first[1] ) )
          {
            # get affiliation for first author
            df[ i, "affil_sc.first" ] <- affil_sc.first
            df[ i, "affil_scID.first" ] <- affil_scID.first
            
            # get h-index first author
            h1 <- h_fetcher( frame.first, yearpub )
            h2 <- h_fetcher( frame.first, yearpub = 2019 )
            df[ i, "h.first" ] <- h1[[1]]
            df[ i, "h.firstCurrent" ] <- h2[[1]]
            df[ i, "nPubs.first" ] <- h1[[2]]
            df[ i, "nPubs.firstCurrent" ] <- h2[[2]]
            
            # get n of collaborators first author
            df[ i, "nCollabor.first" ] <- collaborate( frame.first, yearpub ) 
            
            # get given name first author
            names.first <- as.character( tolower( frame.first[1,2] ) )
            nfirst <- strsplit( names.first, split = ";" )[[1]]
            nfirst <- replace_non_ascii2( iconv( nfirst, to = "UTF-8" ) )
            nfirst <- nfirst[ grepl( tolower(lastname1), nfirst ) ]
            if( length( nfirst ) > 1 )
            {
              initial <- tolower( substr( df[ i, "authorOne_fn" ], start = 1, stop = 1 ) )
              nfirst <- nfirst[ grepl( paste0( "^", initial ), nfirst ) ]
            }
            if( length( nfirst ) > 1 ) nfirst <- nfirst[1]
            nfirst <- gsub( tolower( lastname1 ), "", nfirst )
            nfirst <- gsub( paste0( " ", tolower( firstname1 ) ), "", nfirst )
            if( length( nfirst ) == 0 ) nfirst <- "a."
            if( grepl( "\\s", nfirst ) )
            {
              nfirst <- strsplit(nfirst, split = "\\s" )[[1]]
              nfirst <- nfirst[ !grepl( "\\.", nfirst) ]
              if( length( nfirst ) > 1 ) nfirst <- nfirst[1]
              if( length( nfirst ) == 0 ) nfirst <- NA
            }
            if( grepl( "^[a-z]\\.", nfirst ) | is.na( nfirst ) ) 
            {
              names.first <- frame.first[2]
              names.first[,1] <- as.character( names.first[,1] )
              ROWS <- seq( 1:length(names.first[,1] ) )
              rmove <- grep( "\\.", names.first$name )
              which.row <- ROWS[ !(ROWS %in% rmove ) ]
              if( length( which.row ) == 0 ) {
                nfirst <- NA
              } else {
                names.first <- as.character( tolower( names.first[which.row[1],1] ) )
                
                nfirst <- strsplit( names.first, split = ";" )[[1]]
                nfirst <- replace_non_ascii2( iconv( nfirst, to = "UTF-8" ) )
                nfirst <- nfirst[ grepl( tolower(firstname1), nfirst ) ]
                if( length( nfirst ) == 0 ) nfirst <- NA
                if( length( nfirst ) > 1 )
                {
                  initial <- tolower( substr( df[ i, "authorfirst_fn" ], start = 1, stop = 1 ) )
                  nfirst <- nfirst[ grepl( paste0( "^", initial ), nfirst ) ]
                }
                nfirst <- gsub( paste0( " ", tolower( firstname1 ) ), "", nfirst )
                if( length( nfirst ) > 1 ) nfirst <- nfirst[1]
                if( grepl( "\\s", nfirst ) )
                {
                  nfirst <- strsplit(nfirst, split = "\\s" )[[1]]
                  nfirst <- nfirst[ !grepl( "\\.", nfirst) ]
                  if( length( nfirst ) > 1 ) nfirst <- nfirst[1]
                  if( length( nfirst ) == 0 ) nfirst <- NA
                }
              }
            }
            
            if( length(nfirst) == 1 ) { df[ i, "name_first" ] <- nfirst 
            } else { df[ i, "name_first" ] <- NA }
            
            # determine academic age and uninterrupted presence
            frame.first$year <- as.numeric( substr( frame.first$cover_date, start = 1, stop = 4 ) )
            frame.first <- frame.first[ frame.first$year < yearpub, ]
            if( length( frame.first$year ) > 0 )
            {
              acadAge <- yearpub - min( frame.first$year )
              if( is.na(acadAge) ) acadAge <- 0
              df[ i, "acadAge_first" ] <- acadAge
              d <- 1
              for( j in 1:1000 )
              {
                
                if( sum( grepl( (yearpub - d), frame.first$year ) ) > 0 )
                {
                  d <- d + 1
                  next
                  
                  # else, stop for loop
                } else { break }
              }
              df[ i, "acadPresence_first" ] <- d - 1 
              
            } else {
              
              df[ i, "acadAge_first" ] <- 0
              df[ i, "acadPresence_first" ] <- 0
            }
          }
          
          # get yearpub if this was still missing
          if( is.na( yearpub ) & length(frame.first) > 0 )
          {
            title <- tiab[ tiab$pmid == df[ i, "pmid" ], "articletitle" ]
            date <- as.character( frame.first[ agrepl( title, frame.first$title, ignore.case ), "cover_date" ] )
            yearpub <- as.numeric( substr( date, start = 1, stop = 4 ) )
            if( length(yearpub) == 1 ) df[ i, "yearpub" ] <- yearpub
          }
          
          
          #      if( is.na( df[ i, "name_first" ] ) ) {
          #        fnames <- strsplit( firstname2, split = " " )
          #        df[ i, "name_first" ] <- tolower( fnames[[1]][1] )
          #      }
        }
      }
      
      
      
      ################ ADDITIONAL INFORMATION ###################################################
      
      # affiliation of first and last author
      affiliations <- strsplit( df[ i, "affiliations" ], "\\|" )[[1]] 
      df[ i, "affFirst" ] <- affiliations[1]
      if( length( affiliations ) == length( authors ) ) {
        df[ i, "affLast" ] <- affiliations[ length(affiliations) ]
        df[ i, "affNumber" ] <- "computablePubmed"
      } else { 
        df[ i, "affLast" ] <- NA 
        df[ i, "affNumber" ] <- "notComputablePubmed"
      }
      
      
      suppressWarnings( rm(au_id.last) )
      suppressWarnings( rm(au_id) )
      
      
      # remove ';' from fullnames, scopusIDs
      if( length( names(df) ) == 43 ) {
        df[i,]$fullnames <- gsub( ";", "\\|", df[i,]$fullnames)
        df[i,]$scopusIDs <- gsub( ";", "\\|", df[i,]$scopusIDs)
      }
      
      # because first item does not create all variables, add these to file
      if( i == 1 ) {
        df$acadPresence_last <- df$acadAge_last <- df$nCollabor.last <- 
          df$nPubs.lastCurrent <- df$nPubs.last <- df$h.lastCurrent <- df$h.last <-
          df$affil_scID.last <- df$affil_sc.last <- 
          df$acadPresence_first <- df$acadAge_first <- df$nCollabor.first <- 
          df$nPubs.firstCurrent <- df$nPubs.first <- df$h.firstCurrent <- df$h.first <-
          df$affil_scID.first <- df$affil_sc.first <- df$scopusID.last <- 
          df$scopusID.first <- df$scopusIDs <- df$fullnames <- df$ncitations <- NA
      }
      
      
      # add line to file
      if( i == 1 ) {
        do.append <- F
        use.headers <- T
      } else {
        do.append <- T
        use.headers <- F
      }
      write.table( df[ i, ], "authors_with_details_part.csv", 
                   row.names = F, sep = ",", dec = ".", 
                   append = do.append, col.names = use.headers )
      
      # do not overload Scopus server
      Sys.sleep(0.5)
    },
    
    error=function(err) {
      #message(paste("URL does not seem to exist:", url))
      #message("Here's the original error message:")
      message( err )
      # Choose a return value in case of error
      return(NA)
    }
  )
}

      
      

###TODO### inspect and remove duplicate rows, if present

# write file with h-index
write.csv2( df, "/mnt/data/live02/stress/hlamberink/csv/data/authors_with_detailsFront.csv", row.names = F )

# quit R session
q( save = "no" )