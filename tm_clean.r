tm_clean <- function(x) {
   x <- tm_map(x,removePunctuation)
   x <- tm_map(x, stripWhitespace)
   x <- tm_map(x, removeWords, c(stopwords("en"),"xyz","abc","Company"))
   return(x)
}

