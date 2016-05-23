#' flickrlist - Get metadata  mediofa items available on Flickr for the search string.
#' @import XML
#' @import RCurl
#' @param apikey - API key provided by Flickr.com website. (Refer http://www.flickr.com/services/api/misc.api_keys.html for more details.)
#' @param picture_id - Test to search: ="17966004672"
#' @examples \dontrun{
#'  get_exif(apikey,"17966004672")
#' }
#' @export
get_exif <- function (apikey=NA,picture_id=NA){
  if(is.na(apikey)){
    print("Need to supply API key for Flicker.com website. \n Get yours at http://www.flickr.com/services/api/misc.api_keys.html")
    return(NULL)
  }
  if(is.na(picture_id)){
    print("Need to supply a picture id.")
    return(NULL)
  }
  
  url<-paste("https://api.flickr.com/services/rest/?method=flickr.photos.getExif&api_key=",apikey,
             "&photo_id=",as.character(picture_id),
             "&format=json", 
             "&nojsoncallback=1", sep="")
             # "&api_sig=a29a2542460ecb9684f045acab7567e7", sep="") 
  
  #x <- getURL(url, ssl.verifypeer = FALSE)
  #data <- fromJSON(x, unexpected.escape="keep", method="R")
  tag<-vector()
  info<-data.frame()
  lookUp <- URLencode(url)
  rd <- readLines(lookUp, warn="F") 
  dat <- fromJSON(rd)
  for (i in 1: length(dat$photo$exif)){
    tag[i]<-dat$photo$exif[[i]]$tag
    info[1,i]<-dat$photo$exif[[i]]$raw$`_content`
  }  
  colnames(info)<-tag
  return(info)
}
