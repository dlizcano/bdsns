



# analyze EXIF data for interesting list
library(httr)
#updated to use version 0.4 devtools::install_github("renkun-ken/pipeR/0.4")
library(pipeR)  
library(jsonlite)


# load("C:/Users/Diego/Documents/CodigoR/blog/flickr/secret_key") #load apikey
flickr.app <- oauth_app("caracterizacion",
                        key = "675180849039428abab7eb5b65b8ee63", 
                        secret = "2f434bef67b2a18b")

flickr.endpoint <- oauth_endpoint(
  request = "https://www.flickr.com/services/oauth/request_token"
  , authorize = "https://www.flickr.com/services/oauth/authorize"
  , access = "https://www.flickr.com/services/oauth/access_token"
)
tok <- oauth1.0_token(
  flickr.endpoint
  , flickr.app
  , cache = T
)

# end of auhtorizing part

# GET collections 

mydata <- fromJSON(sprintf(
  "https://api.flickr.com/services/rest/?method=flickr.collections.getTree&api_key=%s&user_id=%s&format=json&nojsoncallback=1"
  , key = "d1a79560f32f4ceded2440c13652a7be"
  , user_id = "128565749@N04"
  # , format( Sys.Date() - i, "%Y-%m-%d")
  , tok$credentials$oauth_token
), flatten = TRUE)

mydata$collections$collection$set







collections<-GET(url=sprintf(
  "https://api.flickr.com/services/rest/?method=flickr.collections.getTree&api_key=%s&user_id=%s&format=json&nojsoncallback=1"
  , key = "d1a79560f32f4ceded2440c13652a7be"
  , user_id = "128565749@N04"
  # , format( Sys.Date() - i, "%Y-%m-%d")
  , tok$credentials$oauth_token
 ))



#get a list and then make it a data frame 
interesting <- lapply(1:daysAnalyze, function(i){
  GET(url=sprintf(
    "https://api.flickr.com/services/rest/?method=flickr.collections.getTree&api_key=%s&user_id=%s&nojsoncallback=1"
    , key = "d1a79560f32f4ceded2440c13652a7be"
    , user_id = "128565749@N04"
    # , format( Sys.Date() - i, "%Y-%m-%d")
    , tok$credentials$oauth_token
  )
  ) %>>%
    content( as = "text" ) %>>%
    jsonlite::fromJSON () %>>%
    ( .$photos$photo ) %>>%
    ( data.frame(
      date = format( Sys.Date() - i, "%Y-%m-%d")
      ,.
      ,stringsAsFactors=F
    )) %>>%
    return
}
) %>>%
  # combine all the days into a data frame
  ( do.call(rbind, .) )


#for each photo try to get the exif information
#Flickr allows users to block EXIF
#so use try to bypass error
#in case you are wondering, yes we could use dplyr here
exifData <- lapply(
  1:nrow(interesting)
  ,function(photo){  # now we will process each photo
    exif <-
      GET(url=sprintf(
        "https://api.flickr.com/services/rest/?method=flickr.photos.getExif&api_key=%s&photo_id=%s&secret=%s&format=json&nojsoncallback=1"
        , api_key
        , interesting[photo,"id"]
        , interesting[photo,"secret"]
      )
      ) %>>%
      content( as = "text" ) %>>%
      jsonlite::fromJSON ()
  }
)# %>>% could chain it, but want to keep exifData
#now that we have a list of EXIF for each photo
#use another lapply
#to extract the useful information
iso <- exifData %>>% 
  # some photos will not have exif if their owners disable it
  # and the api call will give us a stat "fail" instead of "ok"
  list.map(
    f(p,index) -> {
      ifelse (
        p$stat == "ok"
        , p$photo$exif %>>%
          (.[which(.[,"label"]=="ISO Speed"),"raw"])  %>>% 
          as.numeric 
        , NA
      ) %>>%
      {
        data.frame(
          interesting[ index, c( "date", "id" )]
          , "iso" = .
        )
      }
    }
  ) %>>%
  list.table( date, iso ) %>>%
  data.frame( stringsAsFactors = F)


