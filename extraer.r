
# https://www.red-gate.com/simple-talk/databases/sql-server/bi-sql-server/text-mining-and-sentiment-analysis-with-r/


# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("syuzhet") # for sentiment analysis
install.packages("ggplot2") # for plotting graphs
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")


# Read the text file from local machine , choose file interactively
text <- readLines(file.choose())   #enfer_f.csv o usar text=dat2
# Load the data as a corpus
TextDoc <- Corpus(VectorSource(text))

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team")) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)



# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)

# Plot the most frequent words
barplot(dtm_d[1:50,]$freq, las = 2, names.arg = dtm_d[1:50,]$word,
        col ="lightgreen", main ="Top 50 most frequent words",
        ylab = "Word frequencies")

#La correlación es una técnica estadística que puede demostrar
# si los pares de variables están relacionados y con qué fuerza. 
#Esta técnica se puede usar de manera efectiva para analizar qué palabras 
#aparecen con mayor frecuencia en asociación con las palabras que aparecen 
#con mayor frecuencia en las respuestas de la encuesta, lo que ayuda a ver 
#el contexto en torno a estas palabras.

# Find associations 
terms = c("illness","disease","sickness","disorder","ailment","complaint","epidemic","provoke","trigger","induce","elicit","drive","promote","draw","fuel","needle")
findAssocs(TextDoc_dtm, terms = terms, corlimit = 0.49)

##################################################################################

install.packages("BiocManager")
BiocManager::install("Rgraphviz")

crude <- readLines(file.choose())   #gl_enfer.csv
tdm <- TermDocumentMatrix(crude,
	control = list(removePunctuation = TRUE,
	removeNumbers = TRUE,
	stopwords = TRUE))
plot(tdm, corThreshold = 0.49, weighting = TRUE)

############

#find associations
library(ggthemes) 
associations = findAssocs( TextDoc_dtm, "complaint", 0.49) 
associations = as.data.frame( associations) 
associations $ terms = row.names( associations)
associations $ terms <- factor( associations $ terms, levels = associations $ terms)

#plot the associations
ggplot( associations, aes( y = terms)) + 
  geom_point( aes( x = "complaint"), data = associations, size = 1) + 
  theme_gdocs() + 
  geom_text( aes( x = "complaint", label = "complaint"), colour ="darkred", hjust = -0.25, size = 5) + 
  theme( text = element_text( size = 10), axis.title.y = element_blank())
###########

library( igraph) 
refund.m <- as.matrix( TextDoc_dtm)
refund.adj = refund.m %*% t( refund.m) 
refund.adj = graph.adjacency( refund.adj, weighted = TRUE, mode ="undirected", diag = T) 
refund.adj = simplify( refund.adj)
##########

library( qdap)
crude <- readLines(file.choose())   #gl_enfer.csv
word_network_plot(crude,match.string = "complaint",stopwords = c(Top200Words, "disorder"),network.plot = TRUE, cloud.colors = c("gray85", "darkred")) 
        title( main ='@ DeltaAssist Refund Word Network')

#########

install.packages("BiocManager")
BiocManager::install("Rgraphviz")
plot(TextDoc_dtm,corThreshold = 0.49)

#########

#install.packages("textmineR")
#install.packages("textclean")
#install.packages("igraph")


library(tm)
library(graph)
library(igraph)

# Install Rgraphviz
install.packages("BiocManager")
BiocManager::install("Rgraphviz")

acq <- readLines(file.choose())   #enfer_f.csv o usar acq <- dat2
dtm <- DocumentTermMatrix(acq,
  control = list(weighting = function(x) weightTfIdf(x, normalize=FALSE),
  stopwords = TRUE))
freq.terms <- findFreqTerms(dtm, lowfreq=10)[1:25]
terms = c("illness","disease","sickness","disorder","ailment","complaint","epidemic","provoke","trigger","induce","elicit","drive","promote","draw","fuel","needle")
assocs <- findAssocs(dtm, term=terms, corlimit=0.70)

# Recreate edges, using code from plot.DocumentTermMatrix
m <- dtm
corThreshold <- 0.70
m <- as.matrix(m[, freq.terms])
c <- cor(m)
c[c < corThreshold] <- 0
c[is.na(c)] <- 0
diag(c) <- 0
ig <- graph.adjacency(c, mode="undirected", weighted=TRUE)
g1 <- as_graphnel(ig)

# Make edge labels
ew <- as.character(unlist(edgeWeights(g1)))
ew <- ew[setdiff(seq(along=ew), Rgraphviz::removedEdges(g1))]
names(ew) <- edgeNames(g1)
eAttrs <- list()
elabs <- paste("        ", round(as.numeric(ew), 20)) # so it doesn't print on top of the edge
names(elabs) <- names(ew)
eAttrs$label <- elabs
fontsizes <- rep(8, length(elabs))
names(fontsizes) <- names(ew)
eAttrs$fontsize <- fontsizes

plot(dtm, term=freq.terms, corThreshold=0.70, weighting=T, 
  edgeAttrs=eAttrs)


# -----    https://stackoverflow.com/questions/38804749/text-mining-in-r-correlation-of-terms-plot-with-the-values 
weights <- as.numeric(ew)
names(weights) <- names(ew)

edgeRenderInfo(g1) <- list(label=elabs, fontsize=fontsizes, lwd=weights*5)
nodeRenderInfo(g1) <- list(shape="box", fontsize=20)
g1 <- layoutGraph(g1)
renderGraph(g1)

##################################################################################


# https://rpubs.com/komalbachhuka/TM_visuals
#install.packages("igraph")
library( igraph) 
refund.m <- as.matrix(TextDoc_dtm)
refund.adj = refund.m %*% t( refund.m) 
refund.adj = graph.adjacency( refund.adj, weighted = TRUE, mode ="undirected", diag = T) 
refund.adj = simplify( refund.adj)

plot.igraph( refund.adj, vertex.shape ="none", 
             vertex.label.font = 2, 
             vertex.label.color ="darkred", 
             vertex.label.cex = .7, 
             edge.color ="gray85") 
              title( main ='@ DeltaAssist Refund Word Network')


#install.packages("qdap")
library( qdap)
text <- readLines(file.choose()) # gl_enfer.csv
word_network_plot(text) 
title( main ='@ DeltaAssist Refund Word Network')


#################################################

# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
syuzhet_vector <- get_sentiment(text, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

# bing
bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)
#affin
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
d<-get_nrc_sentiment(text)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

#The output shows that the first line of text has;

#Zero occurrences of words associated with emotions of anger, disgust, fear, sadness and surprise
#One occurrence each of words associated with emotions of anticipation and joy
#Two occurrences of words associated with emotions of trust
#Total of one occurrence of words associated with negative emotions
#Total of two occurrences of words associated with positive emotions

#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:dim(d)[1]]+1))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")


#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)



#################################################

textp <- readLines(file.choose()) # gl_enfer.csv



################################################

# # Find associations for words that occur at least 50 times
#findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), corlimit = 0.49)










