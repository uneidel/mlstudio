library(tm);
a = c(1,5)
text = c("We looked everywhere but we couldn't find him.","They are coming by car so they should be here soon.")
dt <- data.frame(a,text)

cleanCorpus<-function(corpus){ 
  # apply stemming 
  corpus <-tm_map(corpus, stemDocument, lazy=TRUE) 
  # remove punctuation 
  corpus.tmp <- tm_map(corpus,removePunctuation) 
  # remove white spaces 
  corpus.tmp <- tm_map(corpus.tmp,stripWhitespace) 
  # remove stop words 
  corpus.tmp <-  tm_map(corpus.tmp,removeWords,stopwords("en")) # de etc.
  
  return(corpus.tmp) 
  
}



corpus <- Corpus(VectorSource(dt$b))

corpus <- cleanCorpus(corpus)
dt1<-data.frame(text=unlist(sapply(corpus, `[`)), stringsAsFactors=F)
dataframe<-data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)
dt$b <-dataframe$text