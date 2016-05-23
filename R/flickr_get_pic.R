

#-------------------------------------------------------------------
# Downloading all Flickr pictures for a set (album) 
# Diego J. Lizcano, 2016
# Modified from: Benedikt Koehler, 2013
# @furukama
#-------------------------------------------------------------------

library(RCurl)
library(rjson)
library(dplyr)

# Login credentials
apikey <- "05bdd790fdcd89f9344003e0f47d7c86" # API key for Flickr API goes here
oauth_token <- "72157668424594651-62abe087b5cd8b53"

# Search data: by folder = set

workdir <- "C:/Temp/"
set     <- "72157653248586872" # picture folders
user_id <- "128565749%40N04"# equivale a: "128565749@N04"



flickr_get_pic <- function (workdir=NA,set=NA,user_id=NA,apikey="5d6a54435c9da0c091e94cf2232e94eb"){
  if(is.na(apikey)){
    print("Need to supply API key for Flicker.com website. \n Get yours at http://www.flickr.com/services/api/misc.api_keys.html")
    return(NULL)
  }
  if(is.na(set)){
    print("Need to supply a folder (set).")
    return(NULL)
  }
  
# Search parameters
n <- 500 # pictures per page
p <- 10 # Number of pages
  
  
# Downloading the images
for (i in 1:p) {
#   if (set != "") { # as json got problems with slash
#     api <- paste("https://api.flickr.com/services/rest/?method=flickr.photosets.getPhotos&api_key=", api.key, 
#                  "&photoset_id=", set,
#                  "&user_id=", "128565749%40N04",# tok$credentials$user_nsid,
#                  "&extras=url_o", # get the original url # makes problem in JSON
#                  "&per_page=", n, 
#                  "&page=", i,
#                  "&format=rest",#  "&format=json",
#                  "&nojsoncallback=1",
#                  "&auth_token=", oauth_token, #tok$credentials$oauth_token,
#                  "&api_sig=772bff11e245f5fb7450da665114373b", # NPI what is this but required
#                  sep="")
#     
#   } else { 
  # as XML
    api <- paste("https://api.flickr.com/services/rest/?method=flickr.photosets.getPhotos",
                 "&api_key=5d6a54435c9da0c091e94cf2232e94eb",
                 "&photoset_id=", set,
                 "&user_id=128565749%40N04",
                 "&extras=url_o",
                 "&per_page=", n,
                 "&page=", i,
                 "&format=rest", sep="")
                 #"&api_sig=fb2e9863c887c11077eb666e2bc3826d", # NPI what is this, but is required !
                 # sep="")
    

  }
  
  raw_data <- getURL(api)
  test<-xmlToList(raw_data)
  fields<-names(test$photoset[[1]])
  album<-test$photoset$.attrs[10]
  data_table<- data.frame() # empty table
  url_link<- vector()
  for (j in 2:length(test$photoset)-1){
    for( z in 1: length(test$photoset$photo)){
      data_table [j,z] <- test$photoset[[j]][[z]]
    }
    url_link[j] <- paste("https://www.flickr.com/photos/",user_id,"/",data_table[j,1],"/in/album-",set,sep="")
  }
  
  colnames(data_table)<-fields
  data_table<-cbind(data_table,url_link) # Table to store info
  
  # data <- fromJSON(raw_data, unexpected.escape="keep", method="R")
  
  imgdir <- paste(workdir, data$photoset$title, sep="")
  dir.create(imgdir)
  
  exiflist<-list()
  for (u in 2:length(test$photoset)-1) {
    # id <- data$photos$photo[[u]]$id
    # farm <- data$photos$photo[[u]]$farm
    # server <- data$photos$photo[[u]]$server
    # secret <- data$photos$photo[[u]]$secret
    # url_donload<-paste("https://c2.staticflickr.com/",farm,"/",server,"/",id,"_",secret,"_o.jpg",sep="")
    url_download<- gsub("farm6.staticflickr.com/", paste("c2.staticflickr.com/", data_table$farm[u],"/", sep=""), data_table$url_o[u]) # trick to get full resol
    temp <- paste(imgdir, "/", data_table$title[u], ".jpg", sep="") 
    exiflist[[u]]<-get_exif(apikey = apikey, picture_id=data_table$id[u]) # get exif info  
    download.file(url_download, temp, method="internal", mode="wb")
  }
  a<-ldply(exiflist)
ouput<-cbind(data_table,a)
return(ouput)

}


