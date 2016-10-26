RunTextAnalysis <- function(path){
  
  # Load
  library(tm)
  library(wordcloud)
  
  # Load the directory to make a word cloud from (WARNING WILL USE ALL FILES IN THE DIRECTORY) 
  raw <- Corpus (DirSource(path))
  inspect(raw)
  
  # Prepare the text
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
  
  # Make the word cloud
  wordcloud(final, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
  
}
