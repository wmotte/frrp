################################################################################
# Aim: Download full text pdfs, given PMID and url
# 
# Contact: Herm Lamberink, h.j.lamberink@umcutrecht.nl
# Date: 2018-03-19
#############################


.libPaths( c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) ) 

library( 'xml2' ) # used by rvest package
library( 'rvest' ) # web scraping package
library( "curl" )
library( "XML" )
library( "pbapply" ) # power bar during sapply
library( 'plyr' ); library( 'dplyr' )
library( 'tidyr' )

###################################
# FUNCTIONS
###################################



###
# Get pdf from given pmid
##
get.pdf <- function( pmid, url, outdr = outdir  )
{
  # prevent the function from shutting down due to an error
  v <- tryCatch(
    {
      # output pdf
      outpdf <- paste0( outdr, '/', pmid, '.pdf' )
      
      if( ! file.exists( outpdf ) )
      {
        
        # set empty pdflink
        pdflink <- NA
        
        #######################
        # pdflink per publisher
        #######################
        # url is from arvojournals
        if( grepl( "arvojournals", url ) )
        {
          # url to pdf
          pdflink <- get.pdflink.arvojournals( url )
        }
        
        # url is from JAMA
        if( grepl( "jamanetwork.com", url ) )
        {
          # url to pdf
          pdflink <- get.pdflink.jama( url )
        }
        
        # url is from PLOS
        if( grepl( "dx.plos", url ) )
        {
          # url to pdf
          pdflink <- get.pdflink.plos( url )
        }
        
        # url is from EHP
        if( grepl( "/EHP", url ) )
        {
          pdflink <- get.pdflink.ehp( url )
        }
        
        # url is from doi/bjs
        if( grepl( "/bjs", url ) )
        {
          pdflink <- get.pdflink.doibjs( url )
        }
        
        # url is from Wiley, via doi.org
        #if( grepl( "dx.doi.org", url ) )
        #{
        #   pdflink <- get.pdflink.doiwiley( url )
        #}
        
        # url is from wiley
        if( grepl( "wiley.com", url ) )
        {
          pdflink <- get.pdflink.wileyreal( url )
        }
        
        # url is from bmj
        if( grepl( "bmj.com", url ) )
        {
          pdflink <- get.pdflink.bmj( url )
        }
        
        # url is from cmaj
        if( grepl( "cmaj.ca", url ) )
        {
          pdflink <- get.pdflink.cmaj( url )
        }
        
        # url is from nejm
        if( grepl( "nejm.org", url ) )
        {
          pdflink <- get.pdflink.nejm( url )
        }
        
        # url is from scielo
        if( grepl( "scielo.br", url ) )
        {
          pdflink <- get.pdflink.scielo( url )
        }
        
        # url is from academic.oup
        if( grepl( "academic.oup", url ) )
        {
          pdflink <- get.pdflink.acoup( url )
        }
        
        # url is from annals
        if( grepl( "annals", url ) )
        {
          pdflink <- get.pdflink.annals( url )
        }
        
        # url is from cambridge
        if( grepl( "cambridge.org", url ) )
        {
          pdflink <- get.pdflink.cambridge( url )
        }
        
        # url is from OVID
        if( grepl( "Insights.ovid", url ) )
        {
          # url to pdf
          pdflink <- get.pdflink.ovid1( url )
          
          if( length(pdflink) == 0 ) pdflink <- get.pdflink.ovid2( url )
        }
        
        # url is from iiar
        if( grepl( "iiar", url ) )
        {
          pdflink <- get.pdflink.iiar( url )
        }
        
        # url is from ahajournals
        if( grepl( "ahajournals", url ) )
        {
          pdflink <- get.pdflink.ahaj( url )
        }
        
        # url is from sciencedirect
        if( grepl( "sciencedirect.com", url ) )
        {
          pdflink <- get.pdflink.sciencedirect( url )
        }
        
        # url is from asm
        if( grepl( "asm", url ) )
        {
          pdflink <- get.pdflink.asm( url )
        }
        
        # url is from ajp
        if( grepl( "ajp", url ) )
        {
          pdflink <- get.pdflink.ajp
        }
        
        # url is from apsjournals
        if( grepl( "apsjournals", url ) )
        {
          pdflink <- get.pdflink.apsjournals( url )
        }
        
        # url is from arjournals
        if( grepl( "arjournals", url ) )
        {
          pdflink <- get.pdflink.arjournals( url )
        }
        
        # url is from ascopubs
        if( grepl( "ascopubs", url ) )
        {
          pdflink <- get.pdflink.ascopubs( url )
        }
        
        # url is from avmajournals
        if( grepl( "avmajournals", url ) )
        {
          pdflink <- get.pdflink.avma( url )
        }
        
        # url is from bjgp
        if( grepl( "bjgp", url ) )
        {
          pdflink <- get.pdflink.bjgp( url )
        }
        
        # url is from boneandjoint
        if( grepl( "boneandjoint", url ) )
        {
          pdflink <- get.pdflink.boneandjoint( url )
        }
        
        # url is from aacrjournals
        if( grepl( "aacrjournals", url ) )
        {
          pdflink <- get.pdflink.aacrjournals( url )
        }
        
        # url is from diabetesjournals
        if( grepl( "diabetesjournals", url ) )
        {
          pdflink <- get.pdflink.diabetesjournals( url )
        }
        
        # url is from asnjournals
        if( grepl( "asnjournals", url ) )
        {
          pdflink <- get.pdflink.asnjournals( url )
        }
        
        # url is from ersjournals
        if( grepl( "ersjournals", url ) )
        {
          pdflink <- get.pdflink.ersjournals( url )
        }
        
        # url is from gacetamedicade
        if( grepl( "gacetamedicade", url ) )
        {
          pdflink <- get.pdflink.gacetamedicade( url )
        }
        
        # url is from tums.ac.ir
        if( grepl( "tums.ac.ir", url ) )
        {
          pdflink <- get.pdflink.tums( url )
        }
        
        # url is from nutrition.org
        if( grepl( "nutrition.org", url ) )
        {
          pdflink <- get.pdflink.nutrition( url )
        }
        
        # url is from aota.org
        if( grepl( "aota.org", url ) )
        {
          pdflink <- get.pdflink.aota( url )
        }
        
        # url is from physiology.org
        if( grepl( "physiology.org", url ) )
        {
          pdflink <- get.pdflink.physiology( url )
        }
        
        # url is from asahq.org
        if( grepl( "asahq.org", url ) )
        {
          pdflink <- get.pdflink.asahq( url )
        }
        
        # url is from upol.cz
        if(  grepl( "upol.cz", url ) )
        {
          pdflink <- get.pdflink.upol.cz( url )
        }
        
        # url is from rcpsych
        if( grepl( "rcpsych.org", url ) )
        {
          pdflink <- get.pdflink.rcpsych( url )
        }
        
        # url is from sabinet.co.za
        if( grepl( "sabinet.co.za", url ) )
        {
          pdflink <- get.pdflink.sabinet( url )
        }
        
        # url is from quintessenz
        if( grepl( "quintessenz", url ) )
        {
          pdflink <- get.pdflink.quintessenz( url )
        }
        
        # url is from clinicalandtranslationalinvestigation
        if( grepl( "clinicalandtranslationalinvestigation", url ) )
        {
          pdflink <- get.pdflink.clinicalandtranslationalinvestigation( url )
        }
        
        # url is from jaoa.org
        if( grepl( "jaoa.org", url ) )
        {
          pdflink <- get.pdflink.jaoa( url )
        }
        
        # url is from snmjournals
        if( grepl( "snmjournals", url ) )
        {
          pdflink <- get.pdflink.snmjournals( url )
        }
        
        # url is from umsha.ac.ir
        if( grepl( "umsha" , url ) )
        {
          pdflink <- get.pdflink.umsha( url )
        }
        
        # url is from tokai
        if( grepl( "tokai" , url ) )
        {
          pdflink <- get.pdflink.tokai( url )
        }
        
        # url is from pamw.pl
        if( grepl( "pamw.pl", url ) )
        {
          pdflink <- get.pdflink.pamw( url )
        }
        
        # url is from aappublications
        if( grepl( "aappublications", url ) )
        {
          pdflink <- get.pdflink.aappublications( url )
        }
        
        # url is from publisherspanel
        if( grepl( "publisherspanel", url ) )
        {
          pdflink <- get.pdflink.publisherspanel( url )
        }
        
        # url is from rcseng
        if( grepl( "rcseng", url ) )
        {
          pdflink <- get.pdflink.rcseng( url )
        }
        
        # url is from rsna
        if( grepl( "rsna", url ) )
        {
          pdflink <- get.pdflink.rsna( url )
        }
        
        # url is from rcjournal
        if( grepl( "rcjournal", url ) )
        {
          pdflink <- get.pdflink.rcjournal( url )
        }
        
        # url is from revistachirurgia
        if( grepl( "revistachirurgia", url ) )
        {
          pdflink <- get.pdflink.revistachirurgia( url )
        }
        
        # url is from thejns
        if( grepl( "thejns", url ) )
        {
          pdflink <- get.pdflink.thejns( url )
        }
        
        # url is from alphamedpress
        if( grepl( "alphamedpress", url ) )
        {
          pdflink <- get.pdflink.alphamedpress( url )
        }
        
        # url is from aepress
        if( grepl( "aepress", url ) )
        {
          pdflink <- get.pdflink.aepress( url )
        }
        
        # url is from ajronline
        if( grepl( "ajronline", url ) )
        {
          pdflink <- get.pdflink.ajronline( url )
        }
        
        # url is from ajcn
        if( grepl( "ajcn", url ) )
        {
          pdflink <- get.pdflink.ajcn( url )
        }
        
        # url is from ams.ac.ir
        if( grepl( "ams.ac.ir", url ) )
        {
          pdflink <- get.pdflink.ams.ac.ir( url )
        }
        
        # url is from annfammed
        if( grepl( "annfammed", url ) )
        {
          pdflink <- get.pdflink.annfammed( url )
        }
        
        # url is from annsaudimed
        if( grepl( "annsaudimed", url ) )
        {
          pdflink <- get.pdflink.annsaudimed( url )
        }
        
        # url is from atsjournals
        if( grepl( "atsjournals", url ) )
        {
          pdflink <- get.pdflink.atsjournals( url )
        }
        
        # url is from birpublications
        if( grepl( "birpublications", url ) )
        {
          pdflink <- get.pdflink.birpublications( url )
        }
        
        # url is from bloodjournal
        if( grepl( "bloodjournal", url ) )
        {
          pdflink <- get.pdflink.bloodjournal( url )
        }
        
        # url is from cfp
        if( grepl( "cfp.org", url ) )
        {
          pdflink <- get.pdflink.cfp( url )
        }
        
        # url is from cmj.hr
        if( grepl( "cmj.hr", url ) )
        {
          pdflink <- get.pdflink.cmj.hr( url )
        }
        
        # url is from cmj.org
        if( grepl( "cmj.org", url ) )
        {
          pdflink <- get.pdflink.cmj.org( url )
        }
        
        # url is from danmedj
        if( grepl( "danmedj", url ) )
        {
          pdflink <- get.pdflink.danmedj( url )
        }
        
        # url is from dirjournal
        if( grepl( "dirjournal", url ) )
        {
          pdflink <- get.pdflink.dirjournal( url )
        }
        
        # url is from e-cmh
        if( grepl( "e-cmh", url ) )
        {
          pdflink <- get.pdflink.ecmh( url )
        }
        
        # url is from ectrx
        if( grepl( "ectrx", url ) )
        {
          pdflink <- get.pdflink.ectrx( url )
        }
        
        # url is from educationforhealth
        if( grepl( "educationforhealth", url ) )
        {
          pdflink <- get.pdflink.educationforhealth( url )
        }
        
        # url is from eje-online
        if( grepl( "eje-online", url ) )
        {
          pdflink <- get.pdflink.ejeonline( url )
        }
        
        # url is from europeanreview
        if( grepl( "europeanreview", url ) )
        {
          pdflink <- get.pdflink.europeanreview( url )
        }
        
        # url is from haematologica
        if( grepl( "haematologica", url ) )
        {
          pdflink <- get.pdflink.haematologica( url )
        }
        
        # url is from hdbp
        if( grepl( "hdbp", url ) )
        {
          pdflink <- get.pdflink.hdbp( url )
        }
        
        # url is from healio
        if( grepl( "healio", url ) )
        {
          pdflink <- get.pdflink.healio( url )
        }
        
        # url is from ijkd
        if( grepl( "ijkd", url ) )
        {
          pdflink <- get.pdflink.ijkd( url )
        }
        
        # url is from ijo.in
        if( grepl( "ijo.in", url ) )
        {
          pdflink <- get.pdflink.ijo.in( url )
        }
        
        # url is from impactjournals
        if( grepl( "impactjournals", url ) )
        {
          pdflink <- get.pdflink.impactjournals( url )
        }
        
        # url is from inaactamedica
        if( grepl( "inaactamedica", url ) )
        {
          pdflink <- get.pdflink.inaactamedica( url )
        }
        
        # url is from indianjcancer
        if( grepl( "indianjcancer", url ) )
        {
          pdflink <- get.pdflink.indianjcancer( url )
        }
        
        # url is from intbrazjurol
        if( grepl( "intbrazjurol", url ) )
        {
          pdflink <- url
        }
        
        # url is from jiaci
        if( grepl( "jiaci", url ) )
        {
          pdflink <- get.pdflink.jiaci( url )
        }
        
        # url is from jmir
        if( grepl( "jmir", url ) )
        {
          pdflink <- get.pdflink.jmir( url )
        }
        
        # url is from jneurosci
        if( grepl( "jneurosci", url ) )
        {
          pdflink <- get.pdflink.jneurosci( url )
        }
        
        # url is from jospt
        if( grepl( "jospt", url ) )
        {
          pdflink <- get.pdflink.jospt( url )
        }
        
        # url is from mdpi.com
        if( grepl( "mdpi.com", url ) )
        {
          pdflink <- get.pdflink.mdpi.com( url )
        }
        
        # url is from painphysicianjournal
        if( grepl( "painphysicianjournal", url ) )
        {
          pdflink <- get.pdflink.painphysicianjournal( url )
        }
        
        # url is from sjweh
        if( grepl( "sjweh", url ) )
        {
          pdflink <- get.pdflink.sjweh( url )
        }
        
        # url is from tandfonline
        if( grepl( "tandfonline", url ) )
        {
          pdflink <- get.pdflink.tandfonline( url )
        }
        
        # url is from thieme-connect
        if( grepl( "thieme-connect", url ) )
        {
          pdflink <- get.pdflink.thieme( url )
        }
        
        # url is from wjgnet
        if( grepl( "wjgnet", url ) )
        {
          pdflink <- get.pdflink.wjgnet( url )
        }
        
        # url is from degruyter
        if( grepl( "degruyter", url ) )
        {
          pdflink <- get.pdflink.degruyter( url )
        }
        
        # url is from biomedcentral
        if( grepl( "biomedcentral", url ) )
        {
          pdflink <- get.pdflink.biomedcentral( url )
        }
        
        # url is from karger
        if( grepl( "karger", url ) )
        {
          pdflink <- get.pdflink.karger( url )
        }
        
        # url is from jkan.or.kr
        if( grepl( "jkan.or.kr", url ) )
        {
          pdflink <- get.pdflink.jkan.or.kr( url )
        }
        
        # url is from medicaljournals.se
        if( grepl( "medicaljournals.se", url ) )
        {
          pdflink <- get.pdflink.medicaljournals.se( url )
        }
        
        
        # url is from anesthesiology
        if( grepl( "anesthesiology", url ) )
        {
          pdflink <- get.pdflink.anesthesiology( url )
        }
        
        # url is from linkinghub
        if( grepl( "linkinghub", url ) )
        {
          pdflink <- get.pdflink.linkinghub( url )
        }
        
        # url contains 10.1038 (nature publishers)
        if( grepl( "doi.org/10.1038", url ) )
        {
          pdflink <- get.pdflink.nature( url )
        }
        
        # url conains 10.1089 (acm journal)
        if( grepl( "doi.org/10.1089", url ) )
        {
          pdflink <- get.pdflink.acm( url )
        }
        
        # url conains 10.1111 (acm journal)
        if( grepl( "doi.org/10.1111", url ) )
        {
          pdflink <- get.pdflink.wiley( url )
        }
        
        # url conains 10.1002 (acm journal)
        if( grepl( "doi.org/10.1002", url ) )
        {
          pdflink <- get.pdflink.wiley( url )
        }
        
        # url contains 10.1038 (springerlink)
        if( grepl( "doi.org/10.1007", url ) )
        {
          pdflink <- get.pdflink.springerlink( url )
        }
        
        
        
        # psychiatryonline
        if( grepl( "psychiatryonline", url ) )
        {
          pdflink <- get.pdflink.psychiatryonline( url )
        }
        
        
        #######################
        # downoad pdf
        #######################
        
        # write pdf to output if link is available
        if( ! is.na( pdflink ) )
        {
          # download pdf (only if output is yet downloaded)
          download.file( url = pdflink, destfile = outpdf, 
                         mode = "wb", quiet = TRUE )
        }
      }
      return( NA )
    },
    
    error=function(err) {
      #message(paste("URL does not seem to exist:", url))
      #message("Here's the original error message:")
      message(paste( pmid, err, "\n" ) )
      # Choose a return value in case of error
      return( paste( pmid, "URL does not seem to exist" ) )
    },
    
    warning=function(war) {
      #message(paste("URL caused a warning:", url))
      #message("Here's the original warning message: ")
      message(paste( pmid, war, "\n" ) )
      # Choose a return value in case of warning
      return( paste( pmid, "warning, test if downloaded" ) )
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
# Get full text pdf link from psychiatryonline.org full text website.
##
get.pdflink.psychiatryonline <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- ".show-pdf"
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css )  %>% html_attr( "href" )
  
  return( pdflink )
}

###
# Get full text pdf link from springerlink full text website.
##
get.pdflink.springerlink <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
}


###
# Get full text pdf link from nature full text website.
##
get.pdflink.nature <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf
  css <- 'meta[name="citation_pdf_url"]'
  
  # save pdflink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  if( identical( pdflink, character(0) ) )
  {
    css <- 'a[class="inline-block block-link pa10 pl0"]'
    intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
    if( !identical( intermed1, character(0)))
    {
      pdflink <- paste0( "https://www.nature.com", intermed1[1] )
      return( pdflink )
    }
  }
}  


###
# Get full text pdf link from acm full text website.
##
get.pdflink.acm <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf
  css <- '.pdfprint a'
  
  # save pdflink
  intermed <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  if( !identical( intermed, character(0) ) ) 
  {
    pdflink <- paste0( "http://online.liebertpub.com", intermed )
    return( pdflink ) 
  }
}  

###
# Get full text pdf link from wiley full text website.
##
get.pdflink.wiley <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf
  css <- 'meta[name="citation_pdf_url"]'
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink ) 
}  

###
# Get full text pdf link from wiley full text website.
##
get.pdflink.wileyreal <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf
  css <- 'meta[name="citation_pdf_url"]'
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink ) 
}  



get.pdflink.sciencedirect <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  css <- 'input[name="redirectURL"]'
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "value" )
  intermed2 <- URLdecode(intermed1)
  
  page <- xml2::read_html( curl( intermed2, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # xpath of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css = 'meta[name="citation_pdf_url"]'
  
  # get href to pdfLink
  intermed3 <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  pdflink <- paste0( "https://www.sciencedirect.com", intermed3 )
  
  return( pdflink )
}




###
# Get full text pdf link from springerlink full text website.
##
get.pdflink.springerlink <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
}



###
# Get full text pdf link from medicaljournals.se full text website.
##
get.pdflink.medicaljournals.se <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'li:nth-child(2) .btn-success2' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "https://www.medicaljournals.se", intermed1 )
  
  return( pdflink )
}



###
# Get full text pdf link from jkan.or.kr full text website.
##
get.pdflink.jkan.or.kr <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '#portlet_content_Format li:nth-child(4) a' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "https://www.jkan.or.kr", intermed1 )
  
  return( pdflink )
}



###
# Get full text pdf link from karger full text website.
##
get.pdflink.karger <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.btn-karger' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "https://www.karger.com", intermed1 )
  
  return( pdflink )
}



###
# Get full text pdf link from degruyter full text website.
##
get.pdflink.degruyter <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.pdf-link' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "https://www.degruyter.com", intermed1 )
  
  return( pdflink )
}



###
# Get full text pdf link from biomedcentral full text website.
##
get.pdflink.biomedcentral <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
}


###
# Get full text pdf link from wjgnet full text website.
##
get.pdflink.wjgnet <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.left-articlenav li:nth-child(3) a' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  
  return( pdflink )
}



###
# Get full text pdf link from thieme-connect full text website.
##
get.pdflink.thieme <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '#articleTabs :nth-child(2) a' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  intermed2 <- paste0( "http://www.thieme-connect.com", intermed1 )
  page2 <- xml2::read_html( curl( intermed2, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  css2 <- '#pdfLink'
  intermed3 <- page2 %>% html_nodes( css = css2 ) %>% html_attr( "href" )
  pdflink <- paste0( "http://www.thieme-connect.com", intermed3 )
  
  return( pdflink )
}


###
# Get full text pdf link from tandfonline full text website.
##
get.pdflink.tandfonline <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.show-pdf' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "http://www.tandfonline.com", intermed1 )
  
  return( pdflink )
}


###
# Get full text pdf link from sjweh full text website.
##
get.pdflink.sjweh <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.pdf-download' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "http://www.sjweh.fi/", intermed1 )
  
  return( pdflink )
}



###
# Get full text pdf link from painphysicianjournal full text website.
##
get.pdflink.painphysicianjournal <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.row .float-right' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "http://www.painphysicianjournal.com", intermed1 )
  
  return( pdflink )
}



###
# Get full text pdf link from mdpi.com full text website.
##
get.pdflink.mdpi.com <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
}


###
# Get full text pdf link from jospt full text website.
##
get.pdflink.jospt <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'a[href^="/doi/pdf"]' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "http://www.jospt.org", intermed1[1] )
  
  return( pdflink )
}


###
# Get full text pdf link from jneurosci full text website.
##
get.pdflink.jneurosci <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
} 


###
# Get full text pdf link from jmir.org full text website.
##
get.pdflink.jmir.org <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_abstract_pdf_url"]' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  page2 <- xml2::read_html( curl( intermed1, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  css2 <- 'a[href^="http://www.jmir.org/article/download"]'
  pdflink <- page2 %>% html_nodes( css = css2 ) %>% html_attr( "href" )
  
  
  return( pdflink )
} 

###
# Get full text pdf link from jiaci full text website.
##
get.pdflink.jiaci <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'li:nth-child(1) a:nth-child(2)'
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "http://www.jiaci.org", intermed1 )
  
  return( pdflink )
} 



###
# Get full text pdf link from indianjcancer full text website.
##
get.pdflink.indianjcancer <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  page2 <- xml2::read_html( curl( intermed1, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  css2 <- 'a[href$=".pdf"]'
  intermed2 <- page2 %>% html_nodes( css = css2 ) %>% html_attr( "href" )
  pdflink <- paste0( "http://www.indianjcancer.com/", intermed2 )
  
  return( pdflink )
} 


###
# Get full text pdf link from inaactamedica full text website.
##
get.pdflink.inaactamedica <- function( url )
{ 
  # get href to pdfLink
  pdflink <- url
  
  return( pdflink )
} 


###
# Get full text pdf link from impactjournals full text website.
##
get.pdflink.impactjournals <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
} 



###
# Get full text pdf link from ijo.in full text website.
##
get.pdflink.ijo.in <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  page2 <- xml2::read_html( curl( intermed1[1], handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  css2 <- 'a[href$=".pdf"]'
  intermed2 <- page2 %>% html_nodes( css = css2 ) %>% html_attr( "href")
  pdflink <- paste0( "http://www.ijo.in/", intermed2 )
  
  return( pdflink )
} 



###
# Get full text pdf link from ijkd full text website.
##
get.pdflink.ijkd <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'frame' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "src" )
  page2 <- xml2::read_html( curl( intermed1[1], handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  css2 <- 'a[href^="http://www.ijkd"]'
  pdflink <- page2 %>% html_nodes( css = css2 ) %>% html_attr( "href")
  
  return( pdflink )
} 


###
# Get full text pdf link from healio full text website.
##
get.pdflink.healio <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  
  return( pdflink )
} 


###
# Get full text pdf link from hdbp full text website.
##
get.pdflink.hdbp <- function( url )
{ 
  # get href to pdfLink
  pdflink <- url
  
  
  return( pdflink )
} 


###
# Get full text pdf link from haematologica full text website.
##
get.pdflink.haematologica <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  
  return( pdflink )
} 



###
# Get full text pdf link from europeanreview full text website.
##
get.pdflink.europeanreview <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.right' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- sub( " http", "http", intermed1 )
  
  return( pdflink )
} 



###
# Get full text pdf link from eje-online full text website.
##
get.pdflink.ejeonline <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  
  return( pdflink )
} 

###
# Get full text pdf link from educationforhealth full text website.
##
get.pdflink.educationforhealth <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  page2 <- xml2::read_html( curl( intermed1, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  css2 <- 'a[href$=".pdf"]'
  intermed2 <- page2 %>% html_nodes( css = css2 ) %>% html_attr( "href" )
  pdflink <- paste0( "http://www.educationforhealth.net/", intermed2)
  
  return( pdflink )
} 


###
# Get full text pdf link from ectrx full text website.
##
get.pdflink.ectrx <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'b a' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "http://www.ectrx.org/forms/", intermed1)
  
  return( pdflink )
} 


###
# Get full text pdf link from e-cmh full text website.
##
get.pdflink.ecmh <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="fulltext_pdf"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
} 


###
# Get full text pdf link from dirjournal full text website.
##
get.pdflink.dirjournal <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'a[href$=".pdf"]' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "http://www.dirjournal.org", intermed1[2] )
  
  return( pdflink )
} 


###
# Get full text pdf link from danmedj full text website.
##
get.pdflink.danmedj <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'a[href$=".pdf"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  
  return( pdflink )
} 



###
# Get full text pdf link from cmj.org full text website.
##
get.pdflink.cmj.org <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  page2 <- xml2::read_html( curl( intermed1, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  css2 <- 'p a:nth-child(1)'
  intermed2 <- page2 %>% html_nodes( css = css2 ) %>% html_attr( "href" )
  pdflink <- paste0( "http://www.cmj.org/", intermed2 )
  
  return( pdflink )
} 


###
# Get full text pdf link from cmj.hr full text website.
##
get.pdflink.cmj.hr <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'frame[src^="http"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "src" )
  
  return( pdflink )
} 


###
# Get full text pdf link from cfp full text website.
##
get.pdflink.cfp <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
} 


###
# Get full text pdf link from canjsurg full text website.
##
get.pdflink.canjsurg <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'p:nth-child(2) a:nth-child(2)' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  
  return( pdflink )
} 


###
# Get full text pdf link from bloodjournal full text website.
##
get.pdflink.bloodjournal <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
} 


###
# Get full text pdf link from birpublications full text website.
##
get.pdflink.birpublications <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.show-pdf' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "http://www.birpublications.org", intermed1 )
  
  
  return( pdflink )
} 


###
# Get full text pdf link from atsjournals full text website.
##
get.pdflink.atsjournals <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.show-pdf' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "http://www.atsjournals.org", intermed1 )
  
  return( pdflink )
} 


###
# Get full text pdf link from annsaudimed full text website.
##
get.pdflink.annsaudimed <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.desc' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  
  return( pdflink )
} 


###
# Get full text pdf link from annfammed.org full text website.
##
get.pdflink.annfammed <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.full-text-pdf-view-link a' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  intermed2 <- sub( "\\+html", "", intermed1 )
  pdflink <- paste0( "http://www.annfammed.org", intermed2 )
  
  return( pdflink )
} 


###
# Get full text pdf link from ams.ac.ir full text website.
##
get.pdflink.ams.ac.ir <- function( url )
{ 
  pdflink <- url
  
  return( pdflink )
} 



###
# Get full text pdf link from ajronline full text website.
##
get.pdflink.ajronline <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '#refLinkList+ li .nowrap' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "http://www.ajronline.org", intermed1 )
  
  return( pdflink )
} 


###
# Get full text pdf link from ajcn full text website.
##
get.pdflink.ajcn <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.full-text-pdf-view-link a' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  intermed2 <- sub( "\\+html", "", intermed1 )
  pdflink <- paste0( "http://www.ajcn.org", intermed2 )
  
  return( pdflink )
} 


###
# Get full text pdf link from aepress.sk full text website.
##
get.pdflink.aepress <- function( url )
{ 
  pdflink <- url
  
  return( pdflink )
} 

###
# Get full text pdf link from alphamedpress full text website.
##
get.pdflink.alphamedpress <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
} 


###
# Get full text pdf link from thejns full text website.
##
get.pdflink.thejns <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.article-tools li:nth-child(2)' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "http://thejns.org", intermed1 )
  
  return( pdflink )
} 

###
# Get full text pdf link from revistachirurgia full text website.
##
get.pdflink.revistachirurgia <- function( url )
{ 
  pdflink <- url
  
  return( pdflink )
} 


###
# Get full text pdf link from rcjournal full text website.
##
get.pdflink.rcjournal <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
} 


###
# Get full text pdf link from rsna full text website.
##
get.pdflink.rsna <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.tab-nav li:nth-child(6) a' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "http://pubs.rsna.org", intermed1)
  
  return( pdflink )
} 


###
# Get full text pdf link from rcseng.ac.uk full text website.
##
get.pdflink.rcseng <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.tab-nav li:nth-child(4) a' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "http://publishing.rcseng.ac.uk", intermed1)
  
  return( pdflink )
} 

###
# Get full text pdf link from publisherspanel full text website.
##
get.pdflink.publisherspanel <- function( url )
{ 
  pdflink <- url
  
  return( pdflink )
} 


###
# Get full text pdf link from aappublications full text website.
##
get.pdflink.aappublications <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
} 


###
# Get full text pdf link from pamw.pl full text website.
##
get.pdflink.pamw <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'div[class="field-item even"] a' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- intermed1[1]
  
  return( pdflink )
} 


###
# Get full text pdf link from tokai.com full text website.
##
get.pdflink.tokai <- function( url )
{
  pdflink <- url
  
  return( pdflink )
}


###
# Get full text pdf link from umsha.ac.ir full text website.
##
get.pdflink.umsha <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
} 



###
# Get full text pdf link from aspet full text website.
##
get.pdflink.aspet <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
} 


###
# Get full text pdf link from waocp full text website.
##
get.pdflink.waocp <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.pdf' 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  intermed2 <- sub( "./", "", intermed1 )
  pdflink <- paste0( "http://journal.waocp.org/", intermed2 )
  
  return( pdflink )
} 


###
# Get full text pdf link from snmjournals full text website.
##
get.pdflink.snmjournals <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  
  return( pdflink )
} 


###
# Get full text pdf link from jaoa.org full text website.
##
get.pdflink.jaoa <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]' 
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  
  return( pdflink )
} 

###
# Get full text pdf link from clinicalandtranslationalinvestigation full text website.
##
get.pdflink.clinicalandtranslationalinvestigation <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'a[href^="files/"]'
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "http://clinicalandtranslationalinvestigation.com/", intermed1 ) 
  
  return( pdflink )
} 


###
# Get full text pdf link from quintessenz full text website.
##
get.pdflink.quintessenz <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'a[class="tocbut"]'
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  link1 <- strsplit( url, ".de" )
  pdflink <- paste0( link1[[1]][1], ".de/", intermed1 ) 
  
  return( pdflink )
} 






###
# Get full text pdf link from sabinet.co.za full text website.
##
get.pdflink.sabinet <- function( url )
{
  pdflink <- url
  
  return( pdflink )
}


###
# Get full text pdf link from rcpsych full text website.
##
get.pdflink.rcpsych <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'link[type="application/pdf"]'
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css )  %>% html_attr( "href" )
  link1 <- strsplit( url, ".org" )
  pdflink <- paste0( link1[[1]][1], ".org", intermed1 )
  
  return( pdflink )
}


###
# Get full text pdf link from upol.cz full text website.
##
get.pdflink.upol.cz <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]'
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css )  %>% html_attr( "content" )
  
  
  return( pdflink )
}


###
# Get full text pdf link from asahq.org full text website.
##
get.pdflink.asahq <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- "#pdfLink"
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css )  %>% html_attr( "data-article-url" )
  link1 <- strsplit( url, ".org" )
  pdflink <- paste0( link1[[1]][1], ".org", intermed1 )
  
  return( pdflink )
}



###
# Get full text pdf link from physiology full text website.
##
get.pdflink.physiology <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'link[type="application/pdf"]'
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  link1 <- strsplit( url, ".org" )
  pdflink <- paste0( link1[[1]][1], ".org", intermed1 )
  
  return( pdflink )
}

###
# Get full text pdf link from aota.org full text website.
##
get.pdflink.aota <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]'
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink )
}


###
# Get full text pdf link from nutrition.org full text website.
##
get.pdflink.nutrition <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- ".full-text-pdf-view-link a"
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  link1 <- strsplit( url, ".org" )
  intermed2 <- paste0( link1[[1]][1], ".org", intermed1 )
  pdflink <- sub( "\\+html", "", intermed2)
  
  return( pdflink )
}


###
# Get full text pdf link from tums.ac.ir full text website.
##
get.pdflink.tums <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- "#sidebarRTArticleTools .file"
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  
  return( pdflink )
}



###
# Get full text pdf link from arvojournals full text website.
##
get.pdflink.arvojournals <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- "#pdfLink"
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "data-article-url" )
  pdflink <- paste0( "http://iovs.arvojournals.org/", pdflink )
  
  return( pdflink )
}


###
# Get full text pdf link from JAMA full text website.
##
get.pdflink.jama <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- "#full-text-tab #pdf-link"
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "data-article-url" )
  link1 <- strsplit( url, ".com" )
  pdflink <- paste0( link1[[1]][1], ".com", intermed1 )
  
  return( pdflink )
}


###
# Get full text pdf link from plos full text website.
##
get.pdflink.plos <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- "#downloadPdf"
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  pdflink <- paste0( "http://journals.plos.org", pdflink )
  
  return( pdflink )
}





###
# Get full text pdf link from bmj full text website.
##
get.pdflink.bmj <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- "link[type='application/pdf']"
  
  # get href to pdfLink
  intermed <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  pdflink <- paste0( "http://www.bmj.com", intermed )
  
  
  return( pdflink )
}



###
# Get full text pdf link from nejm full text website.
##
get.pdflink.nejm <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- "li a[href^='/doi/pdf']"
  
  # get href to pdfLink
  intermed <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  pdflink <- paste0( "http://www.nejm.org", intermed )
  
  return( pdflink )
}



###
# Get full text pdf link from academic.oup full text website.
##
get.pdflink.acoup <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- ".al-link"
  
  # get href to pdfLink
  intermed <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  pdflink <- paste0( "https://academic.oup.com", intermed )
  
  return( pdflink )
}


###
# Get full text pdf link from annals full text website.
##
get.pdflink.annals <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- "#tagmasterPDF"
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>%  html_attr( "data-article-url" )
  pdflink <- paste0( "https://www.annals.org", pdflink )
  
  return( pdflink )
}



###
# Get full text pdf link from cambridge full text website.
##
get.pdflink.cambridge <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- ".download-types li:nth-child(1) a"
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  pdflink <- paste0( "https://www.cambridge.org", pdflink[1] )
  
  return( pdflink )
}



###
# Get full text pdf link from OVID full text website.
##
get.pdflink.ovid1 <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]'
  
  # get href to pdfLink
  #  p1 <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  #  p2 <- xml2::read_html( curl( p1, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  p3 <- page %>% html_nodes( css = "script[type='text/javascript']")
  if ( grepl( "ovidFullTextUrlForButtons = ", p3[2]) )
  {
    p4 <- p3[2]
    p5 <- gsub( ".*ovidFullTextUrlForButtons = \"|PubMed.*", "", p4 )
    p6 <- paste0( p5, "PubMed" )
    
  }
  
  page2 <- xml2::read_html( curl( p6, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ), options = "HUGE" )
  pdflink <- page2 %>% html_nodes( css = "iframe" ) %>% html_attr( "src" )
  #intermed2 <- paste0( "http://ovidsp.tx.ovid.com/", intermed1 )
  
  #page3 <- xml2::read_html( curl( intermed2, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  #pdflink <- page3 %>% html_nodes( css = "iframe") %>% html_attr( "src" )
  
  return( pdflink )
}


###
# Get full text pdf link from OVID full text website.
##
get.pdflink.ovid2 <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name="citation_pdf_url"]'
  
  # get href to pdfLink
  p1 <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  if(identical(p1, character(0))){
    p3 <- page %>% html_nodes( css = "script[type='text/javascript']")
    if ( grepl( "ovidFullTextUrlForButtons = ", p3[2]) )
    {
      p4 <- p3[2]
      p5 <- gsub( ".*ovidFullTextUrlForButtons = \"|PubMed.*", "", p4 )
      p6 <- paste0( p5, "PubMed" )
      
    }
    page2 <- xml2::read_html( curl( p6, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ), options = "HUGE" )
    pdflink <- page2 %>% html_nodes( css = "iframe" ) %>% html_attr( "src" )
    
  }else{
    p2 <- xml2::read_html( curl( p1, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
    p3 <- p2 %>% html_nodes( css = "script[type='text/javascript']")
    if ( grepl( "ovidFullTextUrlForButtons = ", p3[2]) )
    {
      p4 <- p3[2]
      p5 <- gsub( ".*ovidFullTextUrlForButtons = \"|PubMed.*", "", p4 )
      p6 <- paste0( p5, "PubMed" )
      
    }
    
    page3 <- xml2::read_html( curl( p6, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ), options = "HUGE" )
    intermed1 <- page3 %>% html_nodes( css = "#pdf" ) %>% html_attr( "href" )
    intermed2 <- paste0( "http://ovidsp.tx.ovid.com/", intermed1 )
    
    page4 <- xml2::read_html( curl( intermed2, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
    pdflink <- page4 %>% html_nodes( css = "iframe") %>% html_attr( "src" )
    
  }
  return( pdflink )
}



###
# Get full text pdf link from EHP full text website.
##
get.pdflink.ehp <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- '.pdf_icon'
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  pdflink <- paste0( "https://ehp.niehs.nih.gov", pdflink )
  
  return( pdflink )
}




###
# Get full text pdf link from Science Direct full text website.
##
get.pdflink.sciencedirect <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # xpath of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css = ".pdf-download-btn-link"
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  intermed2 <- paste0( "http://www.sciencedirect.com", intermed1 )
  page2 <- xml2::read_html( curl( intermed2, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  css2 = 'meta[content^="0;URL"]'
  
  intermed3 <- page2 %>% html_nodes( css = css2 ) %>% html_attr( "content" )
  pdflink <- strsplit(intermed3, "URL=")[[1]][2]
  
  return( pdflink )
}

# for springerlink, retrieve the correct url
get.pdflink.linkinghub <- function( url )
{
  # parse url further and get the specific node with the URL
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Mozilla/5.0" ) ) )
  parsedfull <- htmlParse( page )
  rootnode <- xmlRoot( parsedfull )
  o <- getNodeSet( rootnode, "//input[@name='redirectURL']" )[[1]]
  
  # convert to character
  o2 <- capture.output(o)
  
  # extract URL from character string
  o3 <- data.frame( col = strsplit( o2, split = " " )[[1]] )
  o4 <- separate( o3, col = "col", into = c("a", "b"), sep = "=", fill = "right" )
  http <- o4[ o4$a == "value", "b" ]
  http <- gsub( "\"", "", http )
  outurl <- URLdecode(http)
  
  
  # parse page
  page <- xml2::read_html( curl( outurl, handle = curl::new_handle( "useragent" = "Mozilla/5.0" ) ) )
  
  # xpath of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css = 'meta[name="citation_pdf_url"]'
  
  # get href to pdfLink
  intermed3 <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  pdflink1 <- sub( "amp;", "", intermed3 )
  
  page2 <- xml2::read_html( pdflink1 )
  css2 = 'div a'
  
  pdflink <- page2 %>% html_nodes( css = css2 ) %>% html_attr( "href" )
  
  return( pdflink )
}


###
# Get full text pdf link from scielo full text website.
##
get.pdflink.scielo <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- "li:nth-child(2) a:nth-child(1)"
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  pdflink <- paste0( "http://www.scielo.br", pdflink[1] )
  
  return( pdflink )
}



###
# Get full text pdf link from hyper.ahajournals full text website.
##
get.pdflink.ahaj <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- 'meta[name=citation_pdf_url]'
  ".aha-icon-download"
  
  # get href to following page, then repeat the above steps
  pdflink <- page %>% html_nodes( css = css ) %>%  html_attr( "content" )
  
  
  # page1 <- xml2::read_html( curl( intermed1, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  #  css <- ".input-text-url input"
  # intermed2 <- page1 %>% html_nodes( css = css ) %>% html_attr( "value" )
  #  pdflink <- paste0( intermed2, ".full.pdf" )
  
  return( pdflink )
}



###
# Get full text pdf link from cmaj full text website.
##
get.pdflink.cmaj <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- ".full-text-pdf-view-link a"
  
  # get href to pdfLink
  pdflink <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  pdflink <- paste0( "http://www.cmaj.ca", pdflink )
  pdflink <- sub( "+html", "", pdflink)
  
  return( pdflink )
}



###
# Get full text pdf link from doi.org (Wiley) full text website.
##
get.pdflink.doiwiley <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- 'meta[name="citation_pdf_url"]'
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>%  html_attr( "content" )
  page2 <- xml2::read_html( curl( intermed1, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  css2 <- "#pdfDocument"
  
  pdflink <- page2 %>% html_nodes( css = css2 ) %>% html_attr( "src" )
  
  return( pdflink )
}

###
# Get full text pdf link from doi.org (bjs) full text website.
##
get.pdflink.doibjs <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- ".js-infopane-epdf"
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  pdflink <- sub( "epdf", "pdf", intermed1)
  
  return( pdflink )
}


###
# Get full text pdf link from asm.org full text website.
##
get.pdflink.asm <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # get href to pdfLink
  pdflink <- sub( "long", "full.pdf", url)
  
  return( pdflink )
}


###
# Get full text pdf link from ajp... full text website.
##
get.pdflink.ajp <- function( url )
{
  pdflink <- url
  
  return( pdflink )
}


###
# Get full text pdf link from apsjournals full text website.
##
get.pdflink.apsjournals <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- "li:nth-child(2) .nowrap"
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  pdflink <- paste0( "http://apsjournals.apsnet.org", intermed1 )
  
  return( pdflink )
}


###
# Get full text pdf link from arjournals full text website.
##
get.pdflink.arjournals <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- "a[href^='/doi/pdf']"
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  pdflink <- paste0( "http://arjournals.annualreviews.org", intermed1 )
  
  return( pdflink )
}

###
# Get full text pdf link from ascopubs full text website.
##
get.pdflink.ascopubs <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- ".show-pdf"
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  intermed2 <- paste0( "http://ascopubs.org", intermed1 )
  pdflink <- sub( "/pdf", "/pdfdirect", intermed2 )
  
  return( pdflink )
}

###
# Get full text pdf link from avmajournals full text website.
##
get.pdflink.avma <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- ".article_link td:nth-child(2) .header4"
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  pdflink <- paste0( "http://avmajournals.avma.org", intermed1 )
  
  return( pdflink )
}

###
# Get full text pdf link from bjgp full text website.
##
get.pdflink.bjgp <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- "link[type='application/pdf']" 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  pdflink <- paste0( "http://bjgp.org", intermed1 )
  
  return( pdflink )
}

###
# Get full text pdf link from boneandjoint full text website.
##
get.pdflink.boneandjoint <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- "link[type='application/pdf']" 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  pdflink <- paste0( "http://bjj.boneandjoint.org.uk", intermed1 )
  
  return( pdflink )
}



###
# Get full text pdf link from aacrjournals full text website.
##
get.pdflink.aacrjournals <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- ".last .highwire-article-nav-jumplink" 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  link1 <- strsplit(url, ".org") 
  pdflink <- paste0( link1[[1]][1], ".org", intermed1 )
  
  return( pdflink )
}



###
# Get full text pdf link from diabetesjournals full text website.
##
get.pdflink.diabetesjournals <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- "link[type='application/pdf']" 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  link1 <- strsplit(url, ".org") 
  pdflink <- paste0( link1[[1]][1], ".org", intermed1 )
  
  return( pdflink )
}


###
# Get full text pdf link from asnjournals full text website.
##
get.pdflink.asnjournals <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- ".primary a" 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  intermed2 <- sub( ".pdf\\+html", ".pdf", intermed1 )
  link1 <- strsplit( url, ".org" )
  pdflink <- paste0( link1[[1]][1], ".org", intermed1 )
  
  return( pdflink )
}


###
# Get full text pdf link from ersjournals full text website.
##
get.pdflink.ersjournals <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- "link[type='application/pdf']" 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  link1 <- strsplit( url, ".com" )
  pdflink <- paste0( link1[[1]][1], ".com", intermed1 )
  
  return( pdflink )
}


###
# Get full text pdf link from gacetamedicade full text website.
##
get.pdflink.gacetamedicade <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  # only modification is "\" before the double quotes.
  css <- ".col-sm-2 li:nth-child(1) a" 
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css ) %>%  html_attr( "href" )
  pdflink <- paste0( "http://gacetamedicademexico.com/", intermed1 )
  
  return( pdflink )
}

###
# Get full text pdf link from iiar full text website.
##
get.pdflink.iiar <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- ".full-text-pdf-view-link a"
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css )  %>%  html_attr( "href" )
  link1 <- strsplit( url, ".org" )
  intermed2 <- paste0( link1[[1]][1], ".org", intermed1 )
  pdflink <- sub( "\\+html", "", intermed2)
  
  
  return( pdflink )
}


###
# Get full text pdf link from anesthesiology full text website.
##
get.pdflink.anesthesiology <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css <- "#pdfLink"
  
  # get href to pdfLink
  intermed1 <- page %>% html_nodes( css = css )  %>% html_attr( "data-article-url" )
  link1 <- strsplit( url, ".org" )
  pdflink <- paste0( link1[[1]][1], ".org", intermed1 )
  
  return( pdflink )
}



###################################
# END FUNCTIONS
###################################

# output directory to store full text pdf
outdir <- 'pdfNEW/pdfs2'

# read data of missing pdfs
missings <- read.csv2( "missingsWithURL.csv", stringsAsFactors = F )

head(missings)

names(missings) <- c( "pmid", "url" )

min <- 40000
max <- 50000

# set progress bar
progbar <- txtProgressBar( min = min, max = max, style = 3 )


# for every pmid, add url
for( i in min:max )
{
  setTxtProgressBar( progbar, i )
  
  # add url
  pp <- data.frame( pmid = missings$pmid[ i ],
                    url = missings$url[ i ],
                    stringsAsFactors = FALSE )
  get.pdf( pmid = pp$pmid, url = pp$url )
  
}


# quit R session
q( save = "no" )