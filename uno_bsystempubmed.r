

#### enrique ricardo pablo buendia lozada v3 nov/2022

#install.packages("xlsx")


install.packages("dplyr")
install.packages("RISmed")
install.packages("easyPubMed")
install.packages("rcrossref")
install.packages("PubMedWordcloud")
install.packages("wordcloud2")
install.packages("parallel")
install.packages("foreach")
install.packages("doParallel")
install.packages("tidytext")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("tm")
install.packages("topicmodels")
install.packages("viridis")
install.packages("forcats")

library(xlsx)
library(dplyr)
library(RISmed)
library(easyPubMed)
library(rcrossref)
library(PubMedWordcloud)
library(wordcloud2)
library(parallel)
library(foreach)
library(doParallel)
library(tidytext)
library(tidyr)
library(ggplot2)
library(tm)
library(topicmodels)
library(viridis)
library(forcats)


#################################################################### Busqueda

##search_topic <- '(PET-CT AND Cancer [Filter] Humans) AND "2015" [PDAT]'
##my_query <- "Damiano Fantini[AU]"
##ml_query <- "Machine Learning[TI] AND 2016[PD]"
search_topic <- '((neuroeducation AND mexico) OR (neuroeducacion AND mexico) OR (neuro AND mexico) OR (cognitive AND mexico) OR (neuroscience AND mexico) OR (education AND neuro AND mexico) OR (learn AND neuro AND mexico) OR (didactic AND neuro AND mexico) OR (decision AND neuro AND mexico))'

# consulta de los Query y cuantas publicaciones captura
my_query <- get_pubmed_ids(search_topic)

# Extracción de XML de cada uno de las publaciones (easyPubMed)
my_abstracts_xml <- fetch_pubmed_data(my_query,retmax = as.numeric(my_query$Count))
all_xml <- articles_to_list(my_abstracts_xml,encoding="UTF8",simplify = F)

# Extracción de XML de cada uno de las publaciones (RISmed)
#search_query <- EUtilsSummary(search_topic, retmax=as.numeric(my_query$Count))
#records <- EUtilsGet(search_query, type = "efetch", db = "pubmed")


#### Construccion de la base de datos
# función para extraer el número de citaciones por DOI en rcrossref sin errores
Ncitation <- function(doi){
  if(nchar(doi)==0){
    cit <- data.frame(doi=doi,count=NA)
  }else{
    cit <- cr_citation_count(doi,async = T)
  }
  names(cit) <- c("doi","Num_Citation")
  return(cit)
}

# Base de datos con la información adicional dada por RISmed
#pubmed_data <- data.frame(pmid=PMID(records),
#                          Language = Language(records),
#                          country=Country(records),stringsAsFactors = F)


# Proceso de paralelización para construir la base de datos final
cl <- makeCluster(4) #de acuerdo al número de núcleos que cuente el PC
registerDoParallel(cl)

fullDF <- tryCatch(
  {foreach(x=all_xml, 
           .packages = 'easyPubMed',
           .combine = rbind) %dopar% article_to_df(
             pubmedArticle = x,
             autofill = T, #Llena todos los campos
             max_chars = -1, #Extrae el abstract completo
             getKeywords = T,
             getAuthors = T)},
  error = function(e) {NULL}, #si encuentra un error no se para el código
  finally = {stopCluster(cl)}) #parar los núcleos



#fullDF$doi
# escribir los DOI
#con <- file("C:/Users/FACUFIAUDITORIO/Desktop/g_doi.txt")
#documento <- writeLines(text = fullDF$doi, con = con)
#close(con)


#con <- file("C:/Users/yo/Desktop/art2/gl_doi.txt")
#documento <- writeLines(text = fullDF$doi, con = con)
#close(con)


## guardar todo
write.csv(fullDF,"C:\\Users\\FACUFIAUDITORIO\\Desktop\\consulta_f.csv", row.names = FALSE)

write.csv(fullDF,"C:\\Users\\yo\\Desktop\\art2\\consulta_f.csv", row.names = FALSE)





