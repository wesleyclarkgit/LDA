#Wesley Clark - LDA Nokia

#Create list of necessary packages
packages = c("tm","SnowballC","topicmodels", "bindr", "dplyr", "stringi", "LDAvis")

#check if package is on machine, load if yes, install and load if no
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

setwd("C:/Text")

# Get List of all samsung files
dir = list.files(getwd(), pattern="samsung*")
files = lapply(dir, readLines)

documents = Corpus(VectorSource(files))

#View file 10
writeLines(as.character(documents[[10]]))

##### Preprocessing ####
# Convert all characters to lowercase
documents = tm_map(documents, content_transformer(tolower))

#Replace "-" with white space
toSpace = content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
documents = tm_map(documents, toSpace, "-")

# Remove non characters
documents = tm_map(documents, removePunctuation)

# Remove stop words
documents = tm_map(documents, removeWords, stopwords("english"))

# Remove extraneous white spaces
documents = tm_map(documents, stripWhitespace)

# Word stemming
#documents = tm_map(documents, stemDocument)

# Create document term matrix
matrix = DocumentTermMatrix(documents)
rownames(matrix) = dir
freq = colSums(as.matrix(matrix))
ord = order(freq, decreasing = TRUE)
write.csv(freq[ord], "word_frequency.csv")

# Parameters
burn = 4000
iter = 2000
thin = 500
seed = list(1234, 4321, 123, 321, 1413)
nstart = 5

# Number of topics
k = 4

ldaOut = LDA(matrix, k, method = 'Gibbs', control = list(nstart=nstart, seed = seed, burnin = burn, iter = iter, thin = thin))

ldaOut.topics = as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs", k, "DocsToTOpics.csv"))

# Top 10 terms in each topic
ldaOut.terms = as.matrix(terms(ldaOut, 10))
write.csv(ldaOut.terms, file=paste("LDAGibbs", k, "TopicsToTerms.csv"))

# Probabilities associated with topic assignment
topicProbabilities = as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities, file=paste("LDAGibbs", k, 'TopicProbabilities.csv'))

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  ## Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  temp_frequency <- inspect(doc_term)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  ## Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq)
  
  return(json_lda)
}

#Display visualization in web browser
ldaOut.json = topicmodels_json_ldavis(ldaOut, documents, matrix)
serVis(ldaOut.json)