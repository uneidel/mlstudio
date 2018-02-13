library("tm");

preprocess.PrepareData <- function(inputdataframe)
{
  #expecting text_column named to be text
  corpus <- Corpus(VectorSource(inputdataframe$Text))
  # apply stemming 
  corpus <-tm_map(corpus, stemDocument, lazy=TRUE) 
  # remove punctuation 
  corpus <- tm_map(corpus,removePunctuation) 
  # remove white spaces 
  corpus <- tm_map(corpus,stripWhitespace) 
  # remove stop words 
  corpus <-  tm_map(corpus,removeWords,stopwords("en")) # de etc.
  odataframe <-data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)
  inputdataframe$text<-odataframe$text
  
  return (inputdataframe);
}

preprocess.CreateVocabulary <- function(inputdataframe,minWordLength,maxWordLength,minDF,maxDF)
{
  #expecting text_column named to be text
  corpus <- Corpus(VectorSource(inputdataframe$text))
  DTM <- DocumentTermMatrix(corpus, control=list(dictionary=NULL, 
                                                 weighting= weightBin, 
                                                 bounds = list(global=c(minDF, maxDF)), 
                                                 wordLengths=c(minWordLength,maxWordLength)))
  
  terms  <- Terms(DTM) # just the terms
  df <-  tm_term_score(DTM, terms, FUN = slam::col_sums) # terms and there occurence
  idf <- log(nDocs(DTM)/df) # prcessing the inverse document Frequency - this means max occurency Term is 0 and all others are a 0. value
  #Creating output
  outputdataframe <- data.frame(row.names = c("df", "idf"))
  outputdataframe <- rbind(outputdataframe, df)
  outputdataframe <- rbind(outputdataframe, idf)
  names(outputdataframe) <- terms
  outputdataframe <- cbind(data.frame(total.docs=length(inputdataframe$text)), outputdataframe)
  return (outputdataframe); # matrix with vocabulary, DF foreach, IDF foreach 
  
}
a = c(1,5)
text = c("We looked everywhere but we  they couldn't find him.","They are coming by car so they should we be here soon.")
dt <- data.frame(a,text)
outputFrame<-preprocess.CreateVocabulary(dt,1,12,1,1000)

calculate.IDF <- function(vocabulary, minDF, maxDF)
{
  total.docs <- input.voc[1,"total.docs"] 
  
  input.voc <- subset( input.voc, select = -c(total.docs) )
  terms <- names(input.voc) 
  dfs <- as.matrix(input.voc [1,])
  kept_ids <- which(dfs >= minDF & dfs <= maxDF)
  kept_dfs <- dfs[kept_ids] 
  
  idfs <- log(total.docs/kept_dfs)
  
  output.voc <- data.frame(word=terms[kept_ids], df=kept_dfs, idf=idfs)
  
  return(output.voc)
}



