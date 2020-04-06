

# load data on journals and categories
scie <- read.csv2( "O:/psychiatrie/predictie verantwoordonderzoek/10.predictors/journalIF/JCR_SCIE_2016.csv" )
ssci <- read.csv2( "O:/psychiatrie/predictie verantwoordonderzoek/10.predictors/journalIF/JCR_SSCI_2016.csv" )
all <- rbind( scie, ssci )

# sort and select relevant columns
alljs <- dplyr::arrange( all, TITLE )
alljs <- dplyr::select( alljs, Title20, ISO_ABBREV, TITLE, CATEGORY_DESCRIPTION, categories )


# collapse categories
alljs$categories <- gsub( "NI", "AQ", alljs$categories ) # allergy --> immunology
alljs$categories <- gsub( "WH", "AQ", alljs$categories ) # rheumatology --> immunology
alljs$categories <- gsub( "ZD", "DQ", alljs$categories ) # peripheral vascular disease --> cardiac and cardiovascular systems
alljs$categories <- gsub( "QU", "NN", alljs$categories ) # microbiology --> infectious
alljs$categories <- gsub( "RQ", "NN", alljs$categories ) # mycology --> infectious
alljs$categories <- gsub( "TI", "NN", alljs$categories ) # parasitology --> infectious
alljs$categories <- gsub( "ZE", "NN", alljs$categories ) # virology --> infectious
alljs$categories <- gsub( "QA", "PY", alljs$categories ) # medicine, research and experimental --> medicine, general and internal
alljs$categories <- gsub( "TU", "PY", alljs$categories ) # pharmacology and pharmacy --> medicine, general and internal
alljs$categories <- gsub( "WF", "SD", alljs$categories ) # reproductive biology --> obstetrics and gynecology



# make list of above categories
cats <- c( "AQ", "BA", "DQ", "RT", "FY", "GA", "IA", "KI", "KM", 
           "LI", "MA", "NN", "OI", "PY", "SD",
           "DM", "SU", "TC", "TD", "TQ", "VE", "WC",
           "WE", "YA", "ZA")


# for all double categories, only keep necessary ones and change all others to "OT"
{
  # vehicle
  dat <- NULL
  
  # loop over rows
  for( i in 1:nrow( alljs ) )
  {
    # select row
    df <- alljs[ i, ]
    
    # if row has only one category, continue
    if( !grepl( " ", df$categories ) ){
      
      # replace all non-relevant category values by "OT" (other)
      if( !( df$categories %in% cats ) ) df$categories <- "OT"
      
    # else, slpit categories and continue
    } else {
      
      # split
      sp <- strsplit( as.character(df$categories), split = " " )
      sp <- sp[[1]]
      
      # replace all non-relevant category values by "OT" (other)
      for( d in 1:length(sp) )
      {
        e <- sp[d]
        if( !( e %in% cats ) ) sp[d] <- "OT"
      }
      
      
      # if all are 'other', create one 'other' category
      # also, collapse categories which have the same value (e.g. "QU QU" <- "QU")
      # else, only keep the non-other categories
      
      if( length( sp ) == 2 ){
        
        # all OT
        if( "OT" == sp[1] && "OT" == sp[2] ) { 
          sp <- "OT" 
          
          # all some other value
          } else if( sp[1] == sp[2] ) {
            
            sp <- sp[1]
            
          } else {
            sp <- paste( sp, collapse = " " )
            sp <- gsub( "OT", " ", sp )
            sp <- gsub( "  ", "", sp )
          }
      } 
      
      if( length( sp ) == 3 ){
        
        # all OT
        if( "OT" == sp[1] && sp[1] == sp[2] && sp[2] == sp[3] ){
          sp <- "OT"
          
        # all some other value
        } else if( sp[1] == sp[2] && sp[1] == sp[3] ) {
          sp <- sp[1]
        
        # two some other value  
        } else if( "OT" != sp[1] && sp[1] == sp[2] ) {
          sp <- c( sp[1], sp[3] )
          sp <- paste( sp, collapse = " " )
          
        } else if( "OT" != sp[1] && sp[1] == sp[3] ) {
          sp <- c( sp[1], sp[2] )
          sp <- paste( sp, collapse = " " )
          
        } else if( "OT" != sp[2] && sp[2] == sp[3] ) {
          sp <- c( sp[1], sp[2] )
          sp <- paste( sp, collapse = " " )
          
        } else { sp <- paste( sp, collapse = " " ) }
        
        if( grepl( " ", sp ) ) {
          
          sp <- gsub( "OT", " ", sp )
          sp <- gsub( "    ", "", sp ) #in case two were removed next to each other
          sp <- gsub( "  ", "", sp ) #in case one or two unconnected were removed
        }
      } 
      
      if( length( sp ) == 4 ){
        
        if( "OT" == sp[1] && sp[2] == sp[3] && "OT" == sp[4] ){
          sp <- "OT" 
          
        # two some other value  
        } else if( "OT" != sp[1] && sp[1] == sp[2] ) {
          sp <- c( sp[1], sp[3], sp[4] )
          sp <- paste( sp, collapse = " " )
          
          # two some other value  
        } else if( "OT" != sp[2] && sp[2] == sp[4] ) {
          sp <- c( sp[1], sp[2], sp[3] )
          sp <- paste( sp, collapse = " " )
          
        # two some other value  
        } else if( "OT" != sp[3] && sp[3] == sp[4] ) {
          sp <- c( sp[1], sp[2], sp[3] )
          sp <- paste( sp, collapse = " " )
          
        }  else { sp <- paste( sp, collapse = " " ) }
        
        
        if( grepl( " ", sp ) ) {
          
          sp <- gsub( "OT", " ", sp )
          sp <- gsub( "      ", "", sp ) #in case three were removed next to each other
          sp <- gsub( "    ", "", sp ) #in case two were removed next to each other
          sp <- gsub( "  ", "", sp ) #in case one or two unconnected were removed
          
        }
      } 
      
      if( length( sp ) == 5 ){
        
        if( "OT" == sp[1] && sp[2] == sp[3] && sp[4] == sp[5] ){
          sp <- "OT"
        } else{
          sp <- paste( sp, collapse = " " )
          sp <- gsub( "OT", " ", sp )
          sp <- gsub( "        ", "", sp ) #in case four were removed next to each other
          sp <- gsub( "      ", "", sp ) #in case three were removed next to each other
          sp <- gsub( "    ", "", sp ) #in case two were removed next to each other
          sp <- gsub( "  ", "", sp ) #in case one or two unconnected were removed
        }
      }
      
      if( length( sp ) == 6 ){
        
        if( sp[6] == sp[1] && sp[2] == sp[3] && sp[4] == sp[5] ){
          sp <- "OT"
        } else{
          sp <- paste( sp, collapse = " " )
          sp <- gsub( "OT", " ", sp )
          sp <- gsub( "          ", "", sp ) #in case five were removed next to each other
          sp <- gsub( "        ", "", sp ) #in case four were removed next to each other
          sp <- gsub( "      ", "", sp ) #in case three were removed next to each other
          sp <- gsub( "    ", "", sp ) #in case two were removed next to each other
          sp <- gsub( "  ", "", sp ) #in case one or two unconnected were removed
        }
      }
      
      df$categories <- sp
    }
    
    dat <- rbind(dat,df)
    
  }
}






# for journals in multiple categories, choose one category
{
  # vehicle
  dat2 <- NULL
  
  # loop over rows
  for( i in 1:nrow( dat ) )
  {
    # select row
    df <- dat[ i, ]
    
    # split
    sp <- strsplit( as.character(df$categories), split = " " )
    sp <- sp[[1]]
    

    # select which category prevails over others
    if( length( sp ) == 2 ){
      
      # if one is general and one is specific. choose specific
      if( "PY" == sp[1] && "PY" != sp[2] ) { 
        sp <- sp[2] 
      } else if( "PY" != sp[1] && "PY" == sp[2] ) { 
        sp <- sp[1]
        
      # specifics (neurology, gastroenterology, hematology, dermatology, 
      # endocrinology, obstetrics, respiratory, urology, oralsurgery, integrative) prevail over oncology     
      } else if( sp[1] %in% c( "RT", "KI", "MA", "GA", "IA", "SD", "WE", "ZA", "FY", "OI" ) && sp[2] == "DM"  ) {
        sp <- sp[1]
      } else if( sp[2] %in% c( "RT", "KI", "MA", "GA", "IA", "SD", "WE", "ZA", "FY", "OI" ) && sp[1] == "DM"  ) {
        sp <- sp[2]
      
      # oncology prevails over genetics, surgery, geriatrics and immunology
      } else if( sp[1] %in% c( "AQ", "YA", "LI", "KM" ) && sp[2] == "DM"  ) {
        sp <- sp[2]
      } else if( sp[2] %in% c( "AQ", "YA", "LI", "KM" ) && sp[1] == "DM"  ) {
        sp <- sp[1]
      
      # infection prevails over immunology and dermatology
      } else if( sp[1] == "NN" && sp[2] %in% c( "AQ", "GA" ) ) {
        sp <- sp[1]
      } else if( sp[2] == "NN" && sp[1] %in% c( "AQ", "GA" ) ) {
        sp <- sp[2]
        
      # neurology and psychiatry prevail over surgery
      } else if( sp[1] %in% c( "RT", "VE" ) && sp[2] == "YA" ) {
        sp <- sp[1]
      } else if( sp[2] %in% c( "RT", "VE" ) && sp[1] == "YA" ) {
        sp <- sp[2]
        
      # geriatrics prevails over psychiatry
      } else if( sp[1] == "VE" && sp[2] == "LI" ) {
        sp <- sp[2]
      } else if( sp[2] == "VE" && sp[1] == "LI" ) {
        sp <- sp[1]
        

      # heart and vascular prevails over hematology
      } else if( sp[1] == "DQ" && sp[2] == "MA" ) {
        sp <- sp[1]
      }  else if( sp[2] == "DQ" && sp[1] == "MA" ) {
          sp <- sp[2]
        
      
      
          
      # dermatology and respiratory prevail over immunology AQ 
      } else if( sp[1] %in% c( "GA", "WE" ) && sp[2] == "AQ" ) {
        sp <- sp[1]
      } else if( sp[2] %in% c( "GA", "WE" ) && sp[1] == "AQ" ) {
        sp <- sp[2]
        
      # immunology AQ prevails over reproduction, pediatrics, surgery
      } else if( sp[1] == "AQ" && sp[2] %in% c( "SD", "TQ", "YA" ) ) {
        sp <- sp[1]
      } else if( sp[2] == "AQ" && sp[1] %in% c( "SD", "TQ", "YA" ) ) {
        sp <- sp[2]
        
      # anesthesiology prevails over respiratory, heartVasc, neurology, pediatrics, obstetrics
      } else if( sp[1] == "BA" && sp[2] %in% c( "WE", "DQ", "RT", "TQ", "SD" ) ) {
        sp <- sp[1]
      } else if( sp[2] == "BA" && sp[1] %in% c( "WE", "DQ", "RT", "TQ", "SD" ) ) {
        sp <- sp[2]
        
        
      # cardiovascular DQ prevails over endocrine, geriatrics, hematology, pediatrics, respiratory, urology
      } else if( sp[1] == "DQ" && sp[2] %in% c( "IA", "LI", "MA", "TQ", "WE", "ZA" ) ) {
        sp <- sp[1]
      } else if( sp[2] == "DQ" && sp[1] %in% c( "IA", "LI", "MA", "TQ", "WE", "ZA" ) ) {
        sp <- sp[2]
        
        
      # genetics prevails heart and vascular 
      } else if( sp[1] == "DQ" && sp[2] == "KM" ) {
        sp <- sp[2]
      }  else if( sp[2] == "DQ" && sp[1] == "KM" ) {
        sp <- sp[1]
      
      # oral prevails all others
      } else if( sp[1] == "FY" && sp[2] != "FY" ) {
        sp <- sp[1]
      }  else if( sp[2] == "FY" && sp[1] != "FY" ) {
        sp <- sp[2]
        
      # pediatrics and geriatrics do not prevail over others
      } else if( sp[1] %in% c( "TQ", "LI" ) && !(sp[2] %in% c( "TQ", "LI" ) ) ) {
        sp <- sp[2]
      }  else if( sp[2] %in% c( "TQ", "LI" ) && !(sp[1] %in% c( "TQ", "LI" ) ) ) {
        sp <- sp[1]
        
      # rehabilitation prevails over general
      } else if( sp[1] == "TQ" && sp[2] != "TQ" ) {
        sp <- sp[2]
      }  else if( sp[2] == "TQ" && sp[1] != "TQ" ) {
        sp <- sp[1]
        
      # orthopedics previals over surgery
      } else if( sp[1] == "TC" && sp[2] == "YA" ) {
        sp <- sp[1]
      } else if( sp[2] == "TC" && sp[1] == "YA" ) {
        sp <- sp[2]
        
      # surgery prevails over other
      } else if( sp[1] == "YA" && sp[2] != "YA" ) {
        sp <- sp[1]
      } else if( sp[2] == "YA" && sp[1] != "YA" ) {
        sp <- sp[2]
        
      # genetics prevails over other
      } else if( sp[1] == "KM" && sp[2] != "KM" ) {
        sp <- sp[1]
      } else if( sp[2] == "KM" && sp[1] != "KM" ) {
        sp <- sp[2]
        
      # hematology prevails over other
      } else if( sp[1] == "MA" && sp[2] != "MA" ) {
        sp <- sp[1]
      } else if( sp[2] == "MA" && sp[1] != "MA" ) {
        sp <- sp[2]
        
      # neurology prevails over other
      } else if( sp[1] == "RT" && sp[2] != "RT" ) {
        sp <- sp[1]
      } else if( sp[2] == "RT" && sp[1] != "RT" ) {
        sp <- sp[2]
        
      # psychiatry prevails over other
      } else if( sp[1] == "VE" && sp[2] != "VE" ) {
        sp <- sp[1]
      } else if( sp[2] == "VE" && sp[1] != "VE" ) {
        sp <- sp[2]
        
      # pediatrics prevails over obstetrics
      } else if( sp[1] == "TQ" && sp[2] == "SD" ) {
        sp <- sp[1]
      } else if( sp[2] == "TQ" && sp[1] == "SD" ) {
        sp <- sp[2]
      
      # rehabilitation WC prevails over orthopedics TC
      } else if( sp[1] == "WC" && sp[2] == "TC" ) {
        sp <- sp[1]
      } else if( sp[2] == "WC" && sp[1] == "TC" ) {
        sp <- sp[2]
        
      
        
      # paste sp together again  
      } else { sp <- paste( sp, collapse = " " ) }
    
    } else { sp <- paste( sp, collapse = " " ) }
    
    df$categories <- sp  
    dat2 <- rbind( dat2, df )
    
  }
}



# create own category variable
dat2$catNew <- "other"
dat2[ dat2$categories == "AQ", "catNew" ] <- "immunology"
dat2[ dat2$categories == "BA", "catNew" ] <- "anesthesiology"
dat2[ dat2$categories == "DQ", "catNew" ] <- "heart_vascular"
dat2[ dat2$categories == "RT", "catNew" ] <- "neurology"
dat2[ dat2$categories == "FY", "catNew" ] <- "dentistry_oralSurgery"
dat2[ dat2$categories == "GA", "catNew" ] <- "dermatology"
dat2[ dat2$categories == "IA", "catNew" ] <- "endocrinology_metabolism"
dat2[ dat2$categories == "KI", "catNew" ] <- "gastroenterology_hepatology"
dat2[ dat2$categories == "KM", "catNew" ] <- "genetics_heredity"
dat2[ dat2$categories == "LI", "catNew" ] <- "geriatrics_gerontology"
dat2[ dat2$categories == "PY", "catNew" ] <- "general"
dat2[ dat2$categories == "MA", "catNew" ] <- "hematology"
dat2[ dat2$categories == "NN", "catNew" ] <- "infectious"
dat2[ dat2$categories == "OI", "catNew" ] <- "integrative_complementary"
dat2[ dat2$categories == "PY", "catNew" ] <- "general"
dat2[ dat2$categories == "SD", "catNew" ] <- "gynacology_reproduction"
dat2[ dat2$categories == "DM", "catNew" ] <- "oncology"
dat2[ dat2$categories == "SU", "catNew" ] <- "ophthalmology"
dat2[ dat2$categories == "TC", "catNew" ] <- "orthopedics"
dat2[ dat2$categories == "TD", "catNew" ] <- "otorhinolaryngology"
dat2[ dat2$categories == "TQ", "catNew" ] <- "pediatrics"
dat2[ dat2$categories == "VE", "catNew" ] <- "psychiatry"
dat2[ dat2$categories == "WC", "catNew" ] <- "rehabilitation"
dat2[ dat2$categories == "WE", "catNew" ] <- "respiratory_system"
dat2[ dat2$categories == "YA", "catNew" ] <- "surgery"
dat2[ dat2$categories == "ZA", "catNew" ] <- "urology_nephrology"
    

# remove duplicates
dat2 <- dat2[ !duplicated(dat2$TITLE), c("Title20", "ISO_ABBREV", "TITLE", "catNew") ]

# write to file
write.csv2( dat2, "journal_categories.csv", row.names = TRUE )
    
    