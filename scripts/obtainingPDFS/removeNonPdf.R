filez1 <- list.files( "pdfNEW/pdfs1" )
head(filez1)

for( i in filez1[c(1:50)] )
{
  path <- paste0( "pdfNEW/pdfs1/", i )
  l <- readLines( path, n = 1 )

  
  
  if( !grepl( "PDF", l ) )
  {
    cmnd <- paste0( "rm ", path )
    
    system( cmnd, intern = FALSE )
    
    
  }

}

filez2 <- list.files( "pdfNEW/pdfs2" )
head(filez2)

for( i in filez2[c(1:50)] )
{
  path <- paste0( "pdfNEW/pdfs2/", i )
  l <- readLines( path, n = 1 )

  
  if( length(l) != 0  ) {
    if( !grepl( "PDF", l ) )
    {
      cmnd <- paste0( "rm ", path )
      
      system( cmnd, intern = FALSE )
      
    }
    
  }
  
}



# quit R session
q(save = 'no')