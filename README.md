# `bdsns`
Harvesting Biodiversity Records from Social Networking Sites

Google Summer of Code 2015
[Barve, V. (2014). Discovering and developing primary biodiversity data from social networking sites: A novel approach. Ecological Informatics, 24, 194–199. doi:10.1016/j.ecoinf.2014.08.008]

## Install

### Install the development version using `install_github` within Hadley's [devtools](https://github.com/hadley/devtools) package.

```R
install.packages("devtools")
require(devtools)

install_github("vijaybarve/bdsns")
require(bdsns)
```

Note: 

Windows users have to first install [Rtools](http://cran.r-project.org/bin/windows/Rtools/).


### Functions currently available

#### flickerlist

Basic function to query Flickr website using APIs 

```r
flickrlist(myapikey,"Danaus chrysippus")
```

#### flickrtodatabase

Function to download data form Flickr for multiple species and store it in sqlite database

```r
flickrtodatabase(myapikey,"test.txt","scname","testdb")
```