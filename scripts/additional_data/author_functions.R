###
# find correct author
##
find.author <- function( au_id.input = NULL, last_name = NULL, first_name = NULL, title = NULL, verbose = T )
{
  Z <- tryCatch(
    {
      au_data <- author_data2( last_name = last_name, 
                    first_name = first_name, 
                    au_id.input = au_id.input,
                    all_author_info = T, verbose = verbose )

    },
    
    error=function(err) {
      message( err )
    },
    
    warning=function(war) {
      message( war )
    }
  )
  affil_sc <- au_data[[3]][1,"affil_name"]
  affil_scID <- au_data[[3]][1,"affid"]
  
  
  
  # check whether title is present in this author's record (only if au_id is null). 
  # otherwise, loop over alternative authors (starting at author with least entries)
  if( length(au_data) == 3 & is.null( au_id.input ) ) {
    frame.author <- au_data[[2]]
    alternative_auths <- au_data[[3]]
    maxK <- length( alternative_auths[,1] )
    if( sum( agrepl( title, frame.author$title, ignore.case = T ) ) == 0 ) {
      affil_sc <- NULL
      affil_scID <- NULL
      if( maxK > 1 ) {
        print( paste( "...checking for correct author with name", first_name, last_name, "at", Sys.time() ) )
        for( k in 2:maxK ) {
          Sys.sleep( 0.5 )
          au_data <- tryCatch(
            {
              author_data3( last_name = last_name, 
                            first_name = first_name, 
                            all_author_info = T, verbose = F,
                            auth_name = alternative_auths,
                            k = k)
            },
            
            error=function(err) {
              message( err )
              return( NA )
            },
            
            warning=function(war) {
            }
          )
          if(is.null(au_data) & k < maxK ) {
            next()
          } 
          if(is.null(au_data) & k == maxK & verbose ) {
            print(paste( "no author found with name", first_name, last_name ))
            au_data <- NULL
          }
          
          frame.author <- au_data[[2]]
          if( sum( agrepl( title, frame.author$title, ignore.case = T ) ) > 0 ) {
            print(paste("author found! at", Sys.time() ))
            
            affil_sc <- au_data[[3]][k,"affil_name"]
            affil_scID <- au_data[[3]][k,"affid"]
            break()
          } else { au_data <- NULL }
        }
      } else {
        if(verbose) message( paste( "no author found with name", first_name, last_name ) )
        au_data <- NULL
      }
    }
  }
  return(list(au_data,affil_sc,affil_scID))
}



###
# adjusted version of rscopus::author_data2
# added: process_author_name2
# added: k = 1 (choose the first author from scopus, was already default)
##
author_data2 <- function (au_id.input = NULL, last_name = NULL, first_name = NULL, api_key = NULL, verbose = TRUE, 
                          all_author_info = TRUE, k = 1, ...) 
{
  api_key = get_api_key(api_key)
  
  if( is.null( au_id.input ) ) {
    L = process_author_name2(au_id.input = au_id.input, first_name = first_name,        
                             last_name = last_name, api_key = api_key, 
                             verbose = verbose, k = k )
    
    first_name = L$first_name
    last_name = L$last_name
    au_id = L$au_id
    auth_name = L$auth_name
  } 
  if( !is.null( au_id.input ) ) {
    L = process_author_name2(au_id.input = au_id.input, first_name = first_name,
                             last_name = last_name, api_key = api_key, 
                             verbose = verbose, k = k)
    L$first_name = L$auth_name$auth_name[1]
    L$last_name = L$auth_name$auth_name[1]
    auth_name = L$auth_name
    au_id = au_id.input
  }
  if( !is.null(au_id) ) {
    entries = author_search2(au_id = au_id, api_key = api_key, 
                             verbose = verbose, ...)$entries
  } else {
    entries <- NULL
  }
  if (all_author_info) {
    DF = tryCatch( 
      {
        entries_to_df(entries = entries, au_id = NULL, verbose = verbose)
      },
      error=function(err) {
        message( err )
        return( c(NA, NA, NA) )
      },
      warning=function(war) {
      }
    )
  } else {
    DF = tryCatch( 
      {
        entries_to_df(entries = entries, au_id = au_id, verbose = verbose)
      },
      error=function(err) {
        message( err )
        return( c(NA, NA, NA) )
      },
      warning=function(war) {
      }
    )
  }
  DF$first_name = L$first_name
  DF$last_name = L$last_name
  DF$au_id_interest = L$au_id
  DF$au_affil = L$auth_name$affil_name[k]
  DF$au_affid = L$auth_name$affid[k]
  L = list(entries = entries, DF = DF, auth_name = auth_name)
  return(L)
}



###
# adjusted version of rscopus::process_author_name
# added: get_author_info2
# added: select names more selectively with agrepl function
# added: k = k (possibility to select next author in list)
##
process_author_name2 <- function (au_id.input = NULL, last_name = NULL, first_name = NULL, api_key = NULL, 
                                  aut_name = NULL, verbose = FALSE, k = k) 
{
  #if ((!missing(last_name) | !missing(first_name)) & ( !is.null(au_id) ) ) {
  #  message("AU-ID overriding first/last name combination")
  #}
  if (is.null(au_id.input)) {
    last_name = replace_non_ascii2(last_name)
    first_name = replace_non_ascii2(first_name)
    splitname <- strsplit( first_name, split = " " )[[1]]
    auth_name1 <- NULL
    auth_name1$auth_name <- NA
    if( length( splitname ) > 1 ) {
      for( s in 1:length(splitname) ) {
        if( nchar(splitname[s]) == 1 ) splitname[s] <- paste0( splitname[s], "." )
      }
      splitname[1] <- paste0( splitname[1], " " )
      first_name2 <- paste0( splitname[1:length(splitname)], collapse = "" )
      auth_name1 = get_author_info2(last_name = last_name, first_name = first_name2,   
                                    api_key = api_key, verbose = verbose)
      auth_name1 <- auth_name1[ !is.na(auth_name1$auth_name), ]
    }
    if ( is.na(auth_name1$auth_name[1]) ) {
      auth_name1 <- NULL
      auth_name1 = get_author_info2(last_name = last_name, first_name = first_name,   
                                    api_key = api_key, verbose = verbose)
      auth_name1 <- auth_name1[ !is.na(auth_name1$auth_name), ]
    }
    auth_name <- auth_name1[ agrepl( pattern = paste( first_name, last_name ), 
                                     auth_name1$auth_name, ignore.case = TRUE,
                                     max.distance = 0.1), ]
    if (NROW(auth_name) == 0) {
      auth_name <- auth_name1[ agrepl( pattern = paste( first_name, last_name ), 
                                       auth_name1$auth_name, ignore.case = TRUE,
                                       max.distance = 0.2), ]
    }
    if (NROW(auth_name) > 50 ) {
      auth_name <- auth_name1[ tolower(auth_name1$auth_name) == 
                                 tolower( paste( first_name, last_name ) ), ]
    }
    if (NROW(auth_name) == 0) {
      auth_name <- auth_name1
    }
    if (NROW(auth_name) == 0) {
      message(paste("No author name found for", first_name, last_name, "\\n"))
    } else if (all(is.na(auth_name$au_id))) {
      message(paste("No author name found for", first_name, last_name, "\\n"))
    }
    au_id = auth_name$au_id[k]
    #au_affil = auth_name$affil_name[k]
    #au_affid = auth_name$affid[k]
  } 
  if( !is.null(au_id.input) ) {
    auth_name <- NULL
    auth_name = get_author_info2(au_id = au_id.input, first_name = first_name, last_name = last_name, 
                                  api_key = api_key, verbose = verbose)
    last_name = last_name
    first_name = first_name
    au_id = au_id.input
  }
  if (missing(last_name)) {
    last_name = NULL
  }
  if (missing(first_name)) {
    first_name = NULL
  }
  au_id = as.character(au_id)
  if( is.na(au_id) ) au_id <- NULL
  L = list(first_name = first_name, last_name = last_name, 
           au_id = au_id, auth_name = auth_name )
  return(L)
}



###
# adjusted version of rscopus::get_author_info
##
get_author_info2 <- function (...) 
{
  cr = get_complete_author_info2(...)$content                                
  #cr = get_complete_author_info2(...)
  cr2 = cr$`search-results`$entry
  if( is.null(cr2) ) cr2 <- cr
  auth_get_info = function(cr2) {
    auth_names = cr2$`preferred-name`
    auth_names = paste(auth_names$`given-name`, auth_names$surname)
    auth_id = cr2$`dc:identifier`
    auth_id = gsub("AUTHOR_ID:", "", auth_id, fixed = TRUE)
    affil = cr2$`affiliation-current`
    affil_name = affil$`affiliation-name`
    affid = affil$`affiliation-id`
    nonull2 = function(x) {
      ifelse(is.null(x), "", x)
    }
    c(auth_name = nonull2(auth_names), au_id = nonull2(auth_id), 
      affid = nonull2(affid), affil_name = nonull2(affil_name))
  }
  info = t(sapply(cr2, auth_get_info))
  info = as.data.frame(info, stringsAsFactors = FALSE)
  return(info)
}


###
# adjusted version of rscopus::get_complete_author_info
# added: get maximum of 200 entries instead of 25
##
get_complete_author_info2 <- function (au_id = NULL, last_name = NULL, first_name = NULL, api_key = NULL, 
                                       http = "http://api.elsevier.com/content/search/author", 
                                       query = NULL, verbose = FALSE, ...) 
{
  api_key = get_api_key(api_key)
  reg_query = ""
  if (!is.null(au_id)) {
    reg_query = paste0(reg_query, "AU-ID(", au_id, ")")
  } else {
    if (!is.null(first_name)) {
      reg_query = paste0("AUTHFIRST(", first_name, ")+AND+")
    }
    reg_query = paste0(reg_query, "AUTHLAST(", last_name, ")")
  }

  if (!is.null(query)) {
    reg_query = paste0(paste0(reg_query, collapse = "+AND+"), 
                       "+AND+", query)
  }
  reg_query = utils::URLencode(reg_query)
  url = paste0(http, "?query=", reg_query, "&APIKey=", api_key)
  if (verbose) {
    message(paste0("HTTP specified is:", url))
  }
  r = GET(url, add_headers(`X-ELS-ResourceVersion` = "allexpand"), 
          ...)
  cr = content(r)
  if (!is.null(cr$`service-error`)) {
    stop("Service Error\\n")
  }
  if (as.numeric(cr$`search-results`$`opensearch:totalResults`)>25) {   
    n <- as.numeric( cr$`search-results`$`opensearch:totalResults` )
    times <- ceiling( n/200 )
    cr <- NULL
    for( z in 1:times ) {
      if( z == times ) {
        start <- 200 * ( z - 1 )
        count <- n - ( 200 * ( z - 1 ) )
        url <- paste0( http, "?query=", reg_query, "&start=", start,
                       "&count=", count, "&APIKey=", api_key )
        if (verbose) {
          message(paste0("HTTP specified is:", url))
        }
        r <- GET(url, add_headers(`X-ELS-ResourceVersion` = "allexpand"), 
                 ...)
        crT <- content(r)
        if (!is.null(crT$`service-error`)) {
          stop("Service Error\\n")
        }
        if( times == 1 ) {
          cr <- crT
        } else {
          crT <- crT$`search-results`$entry
          cr <- append( cr, crT )
        }
      } else {
        start <- 200 * ( z - 1 )
        count <- 200
        url <- paste0( http, "?query=", reg_query, "&start=", start,
                       "&count=", count, "&APIKey=", api_key )
        if (verbose) {
          message(paste0("HTTP specified is:", url))
        }
        r <- GET(url, add_headers(`X-ELS-ResourceVersion` = "allexpand"), 
                 ...)
        crT <- content(r)
        if (!is.null(crT$`service-error`)) {
          stop("Service Error\\n")
        }
        crT <- crT$`search-results`$entry
        cr <- append( cr, crT )
      }
    }
  }
  return(list(r, content = cr))
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


###
# changed version of rscopus::author_search
# change: removed progress bar
##
author_search2 <- function (au_id, api_key = NULL, http = "http://api.elsevier.com/content/search/scopus", 
          count = 25, verbose = TRUE, facets = "subjarea(sort=fd)", 
          searcher = "AU-ID", max_count = Inf, ...) 
{
  api_key = get_api_key(api_key)
  get_results = function(au_id, start = 0, count = count, verbose = TRUE, 
                         ...) {
    q = list(query = paste0(searcher, "(", au_id, ")"), APIKey = api_key, 
             count = count, start = start, view = "COMPLETE", 
             ...)
    print_q = q
    print_q$APIKey = NULL
    if (verbose) {
      message("The query list is: ")
      print(dput(print_q))
    }
    r = GET(http, query = q, add_headers(`X-ELS-ResourceVersion` = "allexpand"))
    stop_for_status(r)
    cr = content(r)$`search-results`
    return(cr)
  }
  au_id = as.character(au_id)
  cr = get_results(au_id, start = 0, count = count, facets = facets, 
                   verbose = verbose, ...)
  all_facets = cr$facet
  total_results = as.numeric(cr$`opensearch:totalResults`)
  if (verbose) {
    message(paste0("Total Entries are ", total_results))
  }
  if (total_results > max_count) {
    total_results = max_count
    if (verbose) {
      message(paste0("Maximum Count is ", total_results))
    }
  }
  all_entries = cr$entry
  n_runs = ceiling(total_results/count)
  if (n_runs > 1) {
    if (verbose) {
      message(paste0(n_runs, " runs need to be ", "sent with current count"))
      #pb = txtProgressBar(min = ifelse(n_runs == 2, 0, 
      #                                 1), max = n_runs - 1, initial = 1, style = 3)
    }
    for (irun in seq(n_runs - 1)) {
      start = irun * count
      cr = get_results(au_id, start = start, count = count, 
                       facets = facets, verbose = FALSE, ...)
      all_entries = c(all_entries, cr$entry)
      all_facets = c(all_facets, cr$facet)
      #if (verbose) {
      #  setTxtProgressBar(pb, value = irun)
      #}
    }
    #if (verbose) {
    #  close(pb)
    #}
  }
  if (verbose) {
    message(paste0("Number of Output Entries are ", length(all_entries)))
  }
  if (total_results != length(all_entries)) {
    warning("May not have received all entries")
  }
  return(list(entries = all_entries, facets = all_facets))
}



###
# adjusted version of rscopus::author_data2
# skip several steps. This script makes it easier to find the correct author
##
author_data3 <- function (last_name, first_name, api_key = NULL, verbose = TRUE, 
                          all_author_info = FALSE, k = k, auth_name = auth_name, ...) 
{
  api_key = get_api_key(api_key)
  
  au_id <- auth_name$au_id[k]
  au_affil <- auth_name$affil_name[k]
  au_affid <- auth_name$affid[k]
  entries = author_search2(au_id = au_id, api_key = api_key, 
                          verbose = verbose, ...)$entries
  if (all_author_info) {
    DF = entries_to_df(entries = entries, au_id = NULL, verbose = verbose)
  }
  else {
    DF = entries_to_df(entries = entries, au_id = au_id, 
                       verbose = verbose)
  }
  DF$first_name = first_name
  DF$last_name = last_name
  DF$au_id_interest = au_id
  DF$au_affil = au_affil
  DF$au_affid = au_affid
  L = list(entries = entries, DF = DF, auth_name = auth_name)
  return(L)
}


###
# get h-index of author
##
h_fetcher <- function( scopusPubs, yearpub )
{
  t <- tryCatch(
    {
      dd <- scopusPubs
      
      # number of publications 
      pubs <- length( dd[,1] ) 
      
      # per publication, the citation count
      dftotal <- NULL
      for( i in 1:pubs ) {
        citeCount <- dd$citations[i]
        date <- as.character( dd$cover_date[i] )
        if( is.null(date) ) date <- as.character( dd$cover_display_date[i] ) 
        year <- suppressWarnings( as.numeric( substr( date, start = 1, stop = 4 ) ) )
        
        # for all missing values, impute NA
        if( length( citeCount ) == 0 ) citeCount <- NA
        if( length( year ) == 0 ) citeCount <- NA
        
        # combine data to frame
        ds <- data.frame( pub = i,
                          cited_count = citeCount,
                          pubyear = year,
                          stringsAsFactors = F )
        
        dftotal <- rbind( dftotal, ds )
      }
      
      # remove all publications after the RCT publication
      dftotal <- dftotal[ dftotal$pubyear < yearpub, ]
      
      if( length( dftotal[,1] ) > 0  )
      {
        # order by citation count and give ranks
        dfordered <- dplyr::arrange( dftotal, -cited_count )
        dfordered$rank <- as.numeric( row.names( dfordered ) )
        
        
        # select only rows with actual citation count (exclude missings and store how many are actually missing)
        dfordered <- dfordered[ !is.na(dfordered$cited_count), ]
        
        # new variable comparing rank and citationcount
        dfordered$higher <- 1
        dfordered[ dfordered$rank < dfordered$cited_count, "higher" ] <- 0
        
        # select the rows where rank is higher than citation count, 
        # then select the rank of the first row. This rank minus 1 is the H-index 
        if( sum( grepl( 1, dfordered$higher ) ) > 0 ) 
        {
          h <- dfordered[ dfordered$higher == 1, ][ 1, "rank" ] - 1
        } else { h <- length( dfordered[,1] ) }
        
        npubs <- length( dfordered[,1] )
      } else {
        
        h <- 0
        npubs <- 0
      }
      
      
      
      return( list( h, npubs ) )
      
    },
    
    error=function(err) {
      #message(paste("URL does not seem to exist:", url))
      #message("Here's the original error message:")
      message( err )
      # Choose a return value in case of error
      return( NA )
    },
    
    warning=function(war) {
      #message(paste("URL caused a warning:", url))
      #message("Here's the original warning message: ")
      message( war )
      # Choose a return value in case of warning
      return( NA )
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

###
# get number of collaborations
##
collaborate <- function( scopusPubs, yearpub )
{
  # extract dataframe of all publications by author
  dd <- scopusPubs
  
  if( !is.null( dd$cover_date[1] ) ) {
    # subselect all publications prior to the yearpub
    dd$year <- as.numeric( substr( as.character( dd$cover_date ), start = 1, stop = 4 ) )
    dd <- dd[ dd$year < yearpub, ]
    
    if( length( dd[,1] ) > 0 ) {
      # get total number of unique co-authors
      colIDs <- as.character( dd$au_id )
      colIDs <- strsplit(colIDs, split = ";")
      ids <- NULL
      for( i in 1:length(colIDs) ) {
        cc <- colIDs[[i]]
        ID <- paste( cc, collapse = ", " )
        ids <- paste( ids, ", ", ID, collapse = ", " )
      }
      
      ids <- strsplit( ids, split = ", " )[[1]]
      ids <- gsub( "\\s", "", ids )
      ids <- ids[ ids != "" ]
      uniqueIDs <- unique(ids)
      
      n_collaborators <- length( uniqueIDs ) - 1
      
    } else {
      
      n_collaborators <- 0
    }
    
    # else, set n_collaborators to missing  
  } else {
    n_collaborators <- 0
  }

  return( n_collaborators )
}

