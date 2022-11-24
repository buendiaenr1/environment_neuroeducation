install.packages("tm")  # for text mining
install.packages("concaveman") # for text stemming
install.packages("BTM") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("syuzhet") # for sentiment analysis
install.packages("ggplot2") # for plotting graphs
install.packages("ggraph")
install.packages("textplot")
install.packages("udpipe")
install.packages("igraph")
install.packages("graph")
install.packages("glasso")
install.packages("qgraph")
#install.packages("BiocManager")
BiocManager::install("Rgraphviz")


library(ggraph)
library(concaveman)
library(ggplot2)
library(BTM)
library(textplot)
library(udpipe)
library(igraph)
library(graph)
library(Rgraphviz)
library(tm)
library("RColorBrewer")


# Read the text file from local machine , choose file interactively
text <- readLines("enfer_f.csv")   #enfer_f.csv o usar text=dat2

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

####
x<-dtm_v
x <- sort(x)
textplot_bar(x, panel = "Frecuencia de palabras", col.panel = "darkgrey", xlab = "Listado", cextext = 0.75, addpct = TRUE, cexpct = 0.5)
####



######## transformar todo a x para poder usar textplot_correlation_glasso ########
#### un ejemplo está en https://towardsdatascience.com/textplot-r-library-for-visualizing-text-data-a8f1740a032d
############

# terms
m <- length(dimnames(dtm_m)[[1]]) #7793 columnas
term=c()
for( i in 1:m){
 	term<- c(term,dimnames(dtm_m)[[1]][[i]])
}
# Docs
n <- length(dimnames(dtm_m)[[2]]) #489 renglones
docs=c()
for( i in 1:n){
 	docs<- c(docs,dimnames(dtm_m)[[2]][[i]])
}

#relleno
  
dff<-data.frame()
for (i in 1:n){
	dff <- rbind(dff,dtm_m[,i])
}


colnames(dff) <- term
#head(dff)


##### convertir informacion al formato para graficar

todo <- c()
vequi<- c()
for (i in 1:n){ #n
	for (j in 1:m){ #m
		try(
		if(dff[i,j]>0){
			todo<-c(as.character(docs[i]),term[j],dff[i,j])
			vequi<- rbind(vequi,todo)
			todo<-c()
		},silent=TRUE)
	}	
	
}
todo<-vequi
colnames(todo) <- c("doc_id","term","freq")
write.csv(todo,"todo.csv", row.names = FALSE)


####################### saltar seleccion de una palabra especifica
x <- read.csv("todo.csv")#todos los items del corpus



####################### trabajar una palabra epecifica
doc_id=c()
term=c()
freq=c()

new<-c()

sle <- "environment"
for (i in 1:length(x[[2]])){
	if(grepl(sle,x[[2]][i])){
		doc_id=c(doc_id,x[[1]][i])
		term=c(term,x[[2]][i])
		freq=c(freq,x[[3]][i])
	}
}
new=data.frame(as.character(doc_id),term,freq)
colnames(new) <- c("doc_id","term","freq")
cat(" Cantidad de documentos relacionados con el termino > ",length(new$doc_id)," de >",length(x$doc_id)," relaciones\n")
cat(" estos son: \n",new$doc_id,"\n\n")

x<-new
#################################### fin de palabra especifica

dtm <- document_term_matrix(x)
dtm <- dtm_remove_lowfreq(dtm, maxterms = 500)
m <- dtm_cor(dtm)

textplot_correlation_glasso(m, exclude_zero = TRUE)









