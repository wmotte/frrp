# aim: determine gender of first and last author
# date: 2018-07-18
# contact: h.j.lamberink@umcutrecht.nl
#
###############################################################

.libPaths( c( .libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) )


library( 'httr' )
library( 'rvest' )
library( 'pbapply' )
library( 'plyr' )

####################################
# FUNCTIONS
####################################

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


###
# for names like 'Bob B G', extract 'Bob'
##
extract.first.names <- function(namez) { 
  
  namez <- gsub( "-", " ", namez )
  namesSplit <- strsplit(namez, split = "|", fixed = TRUE)

  newnames <- lapply( namesSplit, function(x) { 
    
    for( i in 1:length(x)) {
      if( !is.na(x[i]) & nchar( x[i] ) == 1 ) {
        x <- NA
        next()
      } 
      
      if( grepl( " ", x[i] ) & x[i] != " " ) {
        
        fnSplit <- strsplit(x[i], split = " ")[[1]]
        fnSplit[fnSplit == ""] <- NA
        fnSplit <- fnSplit[!is.na(fnSplit)]
        
        # 1: if only initials, set to NA
        if( sum(nchar(fnSplit)) == length(fnSplit) ) {
          x <- NA
          next()
        } 
        
        # 2: if name + initials, remove initials
        fnSplit <- fnSplit[ nchar(fnSplit) > 1 ]
        
        # 3: if multiple names, choose first one
        if( length(fnSplit) > 1 ) {
          x[i] <- fnSplit[1]
        } else {
          x[i] <- fnSplit
        }
      }
    }
    return(x)
  })
  
  nnames <- sapply(newnames, function(x) {
    paste0( x, collapse = "|" )
  })
  
  nnames[nnames == "NA"] <- NA
  
  return(nnames)
}


###
# get results from genderize.io API using country abbreviations
##
get.genders.country <- function(dfrow) {
  # loop over publications in calling this function
  # loop over authors within publications
  fnames <- dfrow['fnames']
  country <- dfrow['country_abbrev']
  pmid <- dfrow['pmid']
  
  if( !is.na(fnames) ) {
    
    # split names
    nameSplit <- strsplit( fnames, split = "\\|" )[[1]]
    
    # if country is missing, set to NULL
    if(is.na(country)) {
      country <- NULL
    }
    
    
    # use get.genderize.result
    gendz <- lapply(nameSplit, get.genderize.result, country = country) 
    genderz <- t(matrix(unlist(gendz), nrow = 5, ncol = length(nameSplit))) 
    
    namez <- paste0( genderz[,1], collapse = "|" )
    gender <- paste0( genderz[,3], collapse = "|" )
    genderprop <- paste0( genderz[,4], collapse = "|" )
    country <- paste0( genderz[,2], collapse = "|" )
    count <- paste0( genderz[,5], collapse = "|" )
  } else {
    count <- country <- genderprop <- gender <- namez <- NA
  }
  
  
  gdf <- data.frame( names = namez,
                      gender = gender,
                      genderprop = genderprop,
                      count = count,
                     pmid = pmid,
                      stringsAsFactors = F)
  
  write.table(gdf, "partial_output_country.csv", sep = ",", 
              col.names = F, row.names = F, append = T)
  
  return(gdf)

}


###
# get results from genderize.io API not using country abbreviations
##
get.genders.nocountry <- function(dfrow) {
  # loop over publications in calling this function
  # loop over authors within publication
  fnames <- dfrow['fnames']
  pmid <- dfrow['pmid']
  
  if( !is.na(fnames) ) {
    
    if(class(fnames) == "list") fnames <- fnames[[1]]
    if(class(pmid) == "list") pmid <- pmid[[1]]
    
    # split names
    nameSplit <- strsplit( fnames, split = "\\|" )[[1]]
    #nameSplit[nameSplit == "NA"] <- NA

    
    
    # use get.genderize.result
    gendz <- lapply(nameSplit, get.genderize.result, country = NULL) 
    genderz <- t(matrix(unlist(gendz), nrow = 5, ncol = length(nameSplit))) 
    
    namez <- paste0( genderz[,1], collapse = "|" )
    gender <- paste0( genderz[,3], collapse = "|" )
    genderprop <- paste0( genderz[,4], collapse = "|" )
    count <- paste0( genderz[,5], collapse = "|" )
  } else {
    count <- genderprop <- gender <- namez <- NA
  }
  
  
  gdf <- data.frame( names = namez,
                     gender = gender,
                     genderprop = genderprop,
                     count = count,
                     pmid = pmid,
                     stringsAsFactors = F)
  

  return(gdf)
  
}



###
# get results from genderize.io API
##
get.genderize.result <- function( name, country = NULL, 
                                  api_key = "ba40855e3144b669299ad5eb099bbc56",
                                  http = "https://api.genderize.io/" ) {
  
  if( name == "NA" ) {
    name <- NA
    country <- NA
    gender <- NA
    probability <- NA
    count <- NA
  } else {
    # create query with name, country & api key
    query <- paste0( "?name=", name )
    if( !is.null(country) ) query <- paste0( query, "&country_id=", country )
    query <- paste0( query, "&apikey=", api_key )
    
    # combine all to api URL
    link <- paste0( http, query )
    
    # get output for name/country combination
    out <- GET( link )
    if( out$status_code == 429 ) stop("...error: quota exeeded.\nWait until next month...")
    if( out$status_code == 401 ) stop("...error: wrong API key...")
    if( out$status_code != 200 ) {
      for(j in 1:5) {
        out <- GET( link )
        
        if(out$status_code == 200) break()
        if(j == 5) stop(paste("...error: HTTP status", out$status_code, "..."))
      }
    }
    
    result <- content( out )
    if( length( result ) == 4 ) {
      gender <- result$gender
      probability <- result$probability
      count <- result$count
    } else {
      gender <- NA
      probability <- NA
      count <- 0
    }
    if(is.null(country)) country <- NA
  }
  
  
  return(list(name,country,gender,probability,count))
}




####################################
# END FUNCTIONS
####################################




# if loop needs rebooting after error, start where previously ended
if(file.exists("startnr.csv")) {
  
  startnr <- read.csv("startnr.csv")
  startnr <- startnr$startnr
  
  df <- read.csv("partial_output_afterScopus.csv", stringsAsFactors = F)
  
  print(paste("starting at startnr", startnr))
  
} else if(file.exists("genders_dataframe_beforeScopus.csv")){
  
  df <- read.csv("genders_dataframe_beforeScopus.csv", stringsAsFactors = F)
  startnr <- 1
  
  print(paste("starting at prepared 'empty' dataframe"))
  
} else {

  startnr <- 1
  
  print(paste("starting at start, assembling original dataframes"))
  
  # get data of genders before scopus
  dfSmall <- read.csv("genders_full_data.csv", stringsAsFactors = F)
  dfLarge <- read.csv("author_country_gender.csv", stringsAsFactors = F)
  
  # same order for small and large
  dfSmall <- plyr::arrange(dfSmall, pmid)
  head(dfSmall$pmid);head(dfLarge$pmid)
  tail(dfSmall$pmid);tail(dfLarge$pmid)
  
  # get scopus data with additional first names
  scopus <- read.csv("../authors/author_scopus_data_clean.csv", stringsAsFactors = F)
  
  # create smaller dataset: original firstnames (names), scopus first names, Genderize info, number of authors
  namegen <- merge(dfSmall[,c("pmid", "names", "genders", "genderprop", "count")],
                   scopus[,c("pmid", "firstnames_scopus", "firstname_first", "firstname_last", "nAuthors")],
                   all.x = T)
  names(namegen)[4] <- "genderprob" #not proportion but probability
  
  # nAuthors is missing for those in which Scopus failed. Add this info through dfLarge$authors
  namegen[is.na(namegen$nAuthors),]$nAuthors <- sapply(dfLarge[is.na(namegen$nAuthors),]$authors,
                                                       function(x) {
                                                         length(strsplit(x, split = "\\|")[[1]])
                                                       })
  
  # remove non-names from firstnames_scopus, like they have been removed from .._first and .._last
  rm.list <- read.csv("../authors/author_scopus_data_removed_firstnames.csv", stringsAsFactors = F)
  ## further sanitize first names. look at all first names with only two letters
  #View(namegen[!is.na(namegen$firstname_first) & nchar(namegen$firstname_first) == 2, ])
  ## conclusion 1: if first name has two characters and second name has more, it is fine
  ## conclusion 2: this will also apply the other way around
  ## conclusion 3: this is best tested in the column firstnames_scopus, because some first or last names have been erronously removed in the other two columns
  ## plan of attack: (0) add missing firstnames to firstnames_scopus.(1) split firstnames_scopus. (2) nchar on all elements. (3) if all =< 2, NA
  
  sum(is.na(namegen$firstnames_scopus))
  
  #(0) add missing firstnames_first and .._last to firstnames_scopus
  # with only one author (first add name from last author, then from first author)
  namegen[namegen$nAuthors == 1 & is.na(namegen$firstnames_scopus) & !is.na(namegen$firstname_last),]$firstnames_scopus <-
    namegen[namegen$nAuthors == 1 & is.na(namegen$firstnames_scopus) & !is.na(namegen$firstname_last),]$firstname_last
  namegen[namegen$nAuthors == 1 & is.na(namegen$firstnames_scopus) & !is.na(namegen$firstname_first),]$firstnames_scopus <-
    namegen[namegen$nAuthors == 1 & is.na(namegen$firstnames_scopus) & !is.na(namegen$firstname_first),]$firstname_first
  # with two authors
  namegen[namegen$nAuthors == 2 & is.na(namegen$firstnames_scopus),]$firstnames_scopus <- 
    paste0(namegen[namegen$nAuthors == 2 & is.na(namegen$firstnames_scopus),]$firstname_first, "|", namegen[namegen$nAuthors == 2 & is.na(namegen$firstnames_scopus),]$firstname_last)
  # with more than two authors
  for(i in 1:length(namegen[,1])) {
    if( i %in% seq(1,350000, 10000)) print(paste("i =", i))
    
    if( is.na(namegen[i,]$firstnames_scopus) & 
        (!is.na(namegen[i,]$firstname_first) | !is.na(namegen[i,]$firstname_last)) ) {
      
      namegen[i,]$firstnames_scopus <- paste0(namegen[i,]$firstname_first, "|", paste0(rep("NA|", namegen[i,]$nAuthors-2), collapse = ""), namegen[i,]$firstname_last)
      
    }
  }
  sum(is.na(namegen$firstnames_scopus))
  ##(4) last step, remove all "NA|NA" entries
  namegen[!is.na(namegen$firstnames_scopus) & namegen$firstnames_scopus == "NA|NA",]$firstnames_scopus <- NA
  
  
  #(1) split firstnames_scopus. #(2) nchar on all elements. #(3) if all =< 2, mark for removal
  testdf <- strsplit(namegen$firstnames_scopus, split = "\\|")
  namegen$rmnames <- unlist(lapply(testdf, function(x){
    y <- nchar(x)
    if(length(y) == 0) { 
      z <- "remove"
      next()
    }
    y <- y[!is.na(y)]
    if(sum( y > 2 ) > 0) {
      z <- "keep"
    } else {
      z <- "remove"
    }
    return(z)
  }))
  
  namegen[namegen$rmnames == "remove", c("firstnames_scopus", "firstname_first", "firstname_last") ] <- NA
  
  ## _first and _last are now completely sanitized of wrong names. 
  ## If these are missing, remove firstnames_scopus
  namegen[!is.na(namegen$firstnames_scopus) & is.na(namegen$firstname_first) &
            is.na(namegen$firstname_last),]$firstnames_scopus <- NA
  
  #remove indicator column
  namegen$rmnames <- NULL

  
  # rename df for clarity
  df <- namegen #name/gender
  rm(namegen)
  
  # create new variable with all first names
  df$fnames <- tolower(df$names)
  df[is.na(df$fnames),]$fnames <- df[is.na(df$fnames),]$firstnames_scopus
  
  write.csv(df, "genders_dataframe_beforeScopus.csv", row.names = F)
}




# loop over all rows. If genders are missing but fnames not, connect to Genderize
for(i in startnr:length(df[,1])) {
  
  # periodically write output, in case of error
  if(i %in% seq(1, 330000, 1000)) {
    print(paste("i =", i))
    
    write.csv(data.frame(startnr = i+1), "startnr.csv", row.names = F)
    
    write.csv(df, "partial_output_afterScopus.csv", row.names = F)
  }
  
  
  
  if(is.na(df[i,]$genders) & !is.na(df[i,]$fnames)) {
    
    genderize_out <- get.genders.nocountry(dfrow = as.list(df[i,]))
    
    df[i,]$genders <- genderize_out$gender
    df[i,]$genderprob <- genderize_out$genderprop
    df[i,]$count <- genderize_out$count
      
    if(i %in% c(2:30)) print(paste("i =", i, "; genderize_out ==", genderize_out))
  }
  
  
  
}



#### finally #####
# insert four variables: gender_first, gender_last, gender_propFem, gender_ratioF/M
#### finally #####
df$gender_ratioFM <- df$gender_propFem <- df$gender_last <- df$gender_first <- NA

gsplit <- strsplit(df$genders, split = "\\|")
psplit <- strsplit(df$genderprob, split = "\\|")

for(i in 1:length(gsplit)) {
  if(i %in% seq(1,330000,10000)) print(paste("Final step! Creating gender_last, gender_first, propFem and ratio. Item nr", i))
  
  if(is.na(gsplit[[i]][1])) next()
  
  genders <- gsplit[[i]]
  genders[genders == "NA"] <- NA
  probabilities <- suppressWarnings(as.numeric(psplit[[i]]))
  
  if(length(genders) == length(probabilities)) {
    
    # if probability for gender is below 0.7, set as missing
    genders[probabilities < 0.7] <- NA
    
    if( sum(is.na(genders)) / length(genders) > 0.5 ) {
      ratio <- NA
      propFem <- NA
    } else {
      nfemales <- sum(!is.na(genders) & genders == "female")
      nmales <- sum(!is.na(genders) & genders == "male")
      
      ratio <- nfemales / nmales
      propFem <- nfemales / (nfemales + nmales)
    }
    
    df[i,]$gender_first <- genders[1]
    df[i,]$gender_last <- genders[length(genders)]
    df[i,]$gender_ratioFM <- ratio
    df[i,]$gender_propFem <- propFem
  }
}


write.csv(df, "author_country_gender_afterScopus.csv", row.names = F)




# quit R session
q("no")
