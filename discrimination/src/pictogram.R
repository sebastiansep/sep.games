# in case you don't alredy have RCurl
# install.packages("RCurl", dependencies = TRUE)
source_github <- function(u) {
  # load package
  require(RCurl)
  
  # read script lines from website and evaluate
  script <- getURL(u, ssl.verifypeer = FALSE)
  eval(parse(text = script),envir=.GlobalEnv)
}

source_github("https://raw.githubusercontent.com/robertgrant/pictogram/master/pictogram.R")


# install.packages("png", dependencies = TRUE)
require(png)

img <- man<-readPNG("man.png")
pictogram(icon = img, n = c( 12, 35, 7),
          grouplabels=c("12 R logos","35 R logos","7 R logos"))
