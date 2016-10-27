#####################
#James McDonagh 2016 fucntions for text analysis using work from 
# https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html 
# as a base. 
# restart the R session to get all plots
#####################

RunTextAnalysis <- function(path, fin, infreq, clustk){
  # Load
  library(tm)
  library(wordcloud) 
  library(ggplot2)
  library(cluster)
  library(fpc) 
  
  # Files and paths
  fs <- .Platform$file.sep
  home <- getwd()
  setwd(path)
  
  # Backups of input data
  pathbkup <- path
  infreqbkup <- infreq
  finbkup <- fin
  clustkbkup <- clustk
  
  # Make a corpus in which each line of the input file is treated as a separate document
  datain <- paste(path, fin, sep =fs)
  print(datain)
  din <- read.table(datain, header = FALSE, sep = " ", fill=TRUE) # fill = TRUE as the lines will have different numbers of words
  raw <- Corpus(VectorSource(din)) 
  
  # Prepare the text so as to be a corpus the input file as one document
  #raw <- Corpus(DirSource(path))
  # Remove white spaces
  clean <- tm_map(raw, stripWhitespace)
  # Make all words lower case (R may separate capital and lower case words)
  clean <- tm_map(clean, tolower)
  # Remove common words from the language (Set to English here change for other languages)
  clean <- tm_map(clean, removeWords, stopwords("english"))
  # Stem take the word back to their root words
  clean <- tm_map(clean, stemDocument)
  # Get rid of the numbers
  clean <- tm_map(clean, removeNumbers)
  # Get rid of the punctuation
  clean <- tm_map(clean, removePunctuation)
  # Plain text
  final <- tm_map(clean, PlainTextDocument)
  
  # Make words into document term list 
  dtm <- DocumentTermMatrix(final)

  # Make words into term document matrix
  tdm <- TermDocumentMatrix(final)
  
  # Make word cloud
  cloud(path, final)
  
  # Text freq analysis
  freqwords <- list(findFreqTerms(dtm, lowfreq=infreq))
  freqanalysis(dtm, infreq, freqwords)
  
  # Associations
  association(freqwords, dtm)
  
  # Cluster
  dtmspar <- removeSparseTerms(dtm, 0.90)
  inspect(dtmspar)
  clust(dtmspar)
  kclust(dtmspar, clustk)
  
  setwd(home)
}

#################### word cloud function ####################

cloud <- function(path, final){
  # Make the word cloud
  wordcloud(final, scale=c(4,0.5), max.words=1000, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(9, "Set1"))
}

#################### Frequency function ####################

freqanalysis <- function(dtm, infreq, freqword){
  # Calculate the frequencies of each word
  freq <- colSums(as.matrix(dtm))
  length(freq)
  ord <- order(freq)   
  m <- as.matrix(dtm)   
  dim(m)   
  write.csv(m, file="DocumentTermMatrix.csv")
  
  # Find words that turn up more than n times
  print(findFreqTerms(dtm, lowfreq=4))
  
  # Plot word frequencies
  wf <- data.frame(Words=names(freq), Frequency=freq)
  p <- ggplot(subset(wf, freq>=infreq), aes(Words, Frequency))
  p <- p + geom_bar(stat="identity")
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1), panel.background = element_rect(fill = 'white', colour = 'black'))
  ggsave(file="freq_plot.png")
  n <- "freq_plot"
  SavePlot(p, n)
}

#################### save plot function ####################

SavePlot <- function(myPlot, name) {
  npdf <- paste(name, ".pdf", sep="")
  pdf(npdf)
  print(myPlot)
  dev.off()
}

#################### Association function ####################

association <- function(freqlist, dtm1)
  
  for(i in freqlist){
    print(findAssocs(dtm1, i, 0.30))
  }

#################### hierarchal cluster function ####################

clust <- function(dtms){
  d <- dist(t(dtms), method="euclidian") 
  fit <- hclust(d=d, method="ward.D")
  n <- "hclust"
  tiff("hclust.tiff")
  hc <- plot(fit)
  dev.off
}

#################### k-means cluster function ####################

kclust <- function(dtms, k){
  d <- dist(t(dtms), method="euclidian")   
  kfit <- kmeans(d, centers=k)
  n = "kclust"
  tiff("kclust.tiff")
  kc <- clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=k, lines=0)
  dev.off
}
