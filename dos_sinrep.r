
#### enrique r.p. buendia lozada v3 nov.2022

#install.packages("dplyr")
library(dplyr)


buu <- function(cad,buuu){

vec <- c()
resu <- c()
cuales <- c()
n=length(cad)

cont=1
doicc=0



while (cont <= n){
	
		x <- strsplit(cad[cont], split = " ")
           
	
#cat(" cad: ")
#print(x)

for (i in 1:length(x[[1]])) {
	#print(i)
	
	 cc <- x[[1]][i]
	 #print(cc)
      try(

		
	  if(length(cc)>0){
	  if(grepl(buuu,cc)){

	  	#cat(">>>",cont,"\n")
	      cuales <- c(cuales,cont)
	 	doicc=doicc+1
	  
	  }  #if
	  } #if
	,silent=FALSE)
	
}
cont=cont+1
}
cat(" n = ",doicc,"\n")
#print(cuales)
return(unique(cuales))
}







#### enrique r.p. buendia lozada v3 nov.2022

dat<-read.csv("consulta_f.csv") 




#### quitar duplicados en pmid y doi
t1 <- dat %>% distinct(pmid, .keep_all = TRUE)
t2 <- t1 %>% distinct(doi, .keep_all = TRUE)
dat2 <- t2 #usar str(dat2) para revisar = 4949

# cuantos resumenes faltan ?
#cat(" Resumenes faltantes : ")
#length(which(is.na(dat2$abstract)))
#cat(" Cuales :")
#which(is.na(dat2$abstract))

# juntar titulos y resumenes

#poner punto final a titulos y resumenes
for (i in 1:length(dat2$title)){
	x<-dat2$title[[i]]
	di <- nchar(x) # tamano de la cadena titulo
	n_last <- 1                                    # especificar el numero de caracteres a extraer
	ex1<-substr(x, nchar(x) - n_last+1, nchar(x))  # extraer los ultimos n_last caracteres
	if (!is.na(ex1)) {x=paste(x,".","")} else{
	if (ex1 != ".") {x=paste(x,".","")}}
	dat2$title[[i]] <- x

	x<-dat2$abstract[[i]]
	di <- nchar(x) # tamano de la cadena titulo
	n_last <- 1                                    # especificar el numero de caracteres a extraer
	ex1<-substr(x, nchar(x) - n_last+1, nchar(x))  # extraer los ultimos n_last caracteres
	if (!is.na(ex1)) {x=paste(x,".","")} else{
	if (ex1 != ".") {x=paste(x,".","")}}
	dat2$abstract[[i]] <- x	
	

}



dat3 <- dat2$title    # solo titulos
dat4 <- dat2$abstract # solo resumenes
dat2 <- paste(dat3,dat4)
dat2 <- tolower(dat2)

#write.csv(dat2,"C:\\Users\\FACUFIAUDITORIO\\Desktop\\sd_f.csv", row.names = FALSE)
#write.csv(dat2,"C:\\Users\\yo\\Desktop\\a22\\sd_f.csv", row.names = FALSE)    #4949

#### NO usar puesto que los animales tambien forman parte del ambiente
animal <- buu(dat2,"animal")
animal <- unique(animal)
#eliminar los art??culos que hablen de animal 351 de 4679
dat2 <- dat2[-(animal)] # dat2 es ahora de 4328

animal <- buu(dat2,"rat")
animal <- unique(animal)
#eliminar los art??culos que hablen de rat 3405 de 923
dat2 <- dat2[-(animal)]

# animals  no se encontr?? ya que la funci??n grepl de r busca subcadenas dentro de cadenas as?? mucchosanimaliferos 
# es true ya que contiene la subacdena animal y el corpus est?? en minusculas ya que si es sencible a mayusculas grepl

#################################################################################################################################
#################################################################################################################################

#library(dplyr)
dat2<-readLines("sd_f.csv") 

## sinonimos usados de     ImTranslator   v.16.30

illness <- buu(dat2,"illness") #185 con al menos una vez
disease<- buu(dat2,"disease") #2824
sickness<- buu(dat2,"sickness")#13
disorder<- buu(dat2,"disorder")#2361
ailment<- buu(dat2,"ailment")#4
malady<- buu(dat2,"malady")#0
infirmity<- buu(dat2,"infirmity")#0
affliction<- buu(dat2,"affliction")#0
complaint<- buu(dat2,"complaint")#53
epidemic<- buu(dat2,"epidemic")#23

enfermedades <- c(illness,disease,sickness,disorder,ailment,malady,infirmity,affliction,complaint,epidemic) 
enfermedades<-unique(enfermedades)#2082

provoke  <- buu(dat2,"provoke")#35 con al menos una vez
trigger <- buu(dat2,"trigger")#137
induce <- buu(dat2,"induce")#1155
elicit <- buu(dat2,"elicit")#88
incite <- buu(dat2,"incite")#0
drive <- buu(dat2,"drive")#211
promote <- buu(dat2,"promote")#262
draw <- buu(dat2,"draw")#113
fuel <- buu(dat2,"fuel")#48
goad <- buu(dat2,"goad")#0
touch_off  <- buu(dat2,"touch off")#0
set_up  <- buu(dat2,"set up")#0
throw_up <- buu(dat2,"throw up")#0
nettle <- buu(dat2,"nettle")#0
needle <- buu(dat2,"needle")#20
goad_on <- buu(dat2,"goad on")#0
lure_on <- buu(dat2,"lure on")#0

produce <- c(provoke,trigger,induce,elicit,incite,drive,promote,draw,fuel,goad,touch_off,set_up,throw_up,nettle,needle,goad_on,lure_on)
produce<-unique(produce)#1169

### intersecci??n entre enfermedades y produce bigrama de palabras no necesariamente continuas
###
# muchos an??lisis de texto interesantes se basan en las relaciones entre las palabras,
# ya sea examinando qu?? palabras tienden a seguir a otras inmediatamente o
# si tienden a coexistir en los mismos documentos.
# https://www.tidytextmining.com/ngrams.html 
conand=c()
for (i in produce){
  for (j in enfermedades)
	if (i == j) {
		conand=c(conand,i)
	} 
}
print(conand)
conand<-unique(conand)#488
conand
write.csv(dat2[conand],"enfer_f.csv", row.names = FALSE)







head(dat2,10)
####################################################################################################
####################################################################################################
#nube de palabras

#install.packages("pacman")
library("pacman")
#install.packages("tm")
library("tm")
#install.packages("wordcloud2")
library("wordcloud2")

p_load("tm") # Biblioteca para realizar el preprocesado del texto,
p_load("tidyverse") # Biblioteca con funciones para manipular datos.
p_load("wordcloud") # Biblioteca para graficar nuestra nube de palabras.
p_load("RColorBrewer") # Biblioteca para seleccionar una paleta de colores de nuestra nube de palabras.

texto <- dat2

texto <- VCorpus(VectorSource(texto),readerControl = list(reader = readPlain, language = "english"))



# Preprocesado de texto
texto <- tm_map(texto, tolower)
#espanol
##texto <- texto %>%
##            tm_map(removePunctuation) %>%
##            tm_map(removeNumbers) %>%
##            tm_map(removeWords, stopwords("spanish"))
##texto <- tm_map(texto, removeWords, c("puede", "ser", "pues", "si", "a??n", "c??mo"))
##texto <- tm_map(texto, stripWhitespace)

#ingles
texto <- texto %>%
            tm_map(removePunctuation) %>%
            tm_map(removeNumbers) %>%
            tm_map(removeWords, stopwords("english"))
#texto <- tm_map(texto, removeWords, c("puede", "ser", "pues", "si", "a??n", "c??mo"))
texto <- tm_map(texto, stripWhitespace)



# Construyendo la tabla de frecuencia

texto <- tm_map(texto, PlainTextDocument)
tabla_frecuencia <- DocumentTermMatrix(texto)

tabla_frecuencia <- cbind(palabras = tabla_frecuencia$dimnames$Terms,
                          frecuencia = tabla_frecuencia$v)

# Convertimos los valores enlazados con cbind a un objeto dataframe.
tabla_frecuencia<-as.data.frame(tabla_frecuencia)
# Forzamos a que la columna de frecuencia contenga valores num??ricos.
tabla_frecuencia$frecuencia<-as.numeric(tabla_frecuencia$frecuencia)
# Ordenamos muestra tabla de frecuencias de acuerdo a sus valores num??ricos.
tabla_frecuencia<-tabla_frecuencia[order(tabla_frecuencia$frecuencia, decreasing=TRUE),]


#####
which(tabla_frecuencia$frecuencia > 10)
rr<-which(tabla_frecuencia$frecuencia > 10) #864
tabla_frecuencia[rr,]
n=length(tabla_frecuencia[rr,]$frecuencia)
plot(1:n,tabla_frecuencia[rr,]$frecuencia)
text(1:n,tabla_frecuencia[rr,]$frecuencia,tabla_frecuencia[rr,]$palabras) #poner etiquetas


length(which(tabla_frecuencia$palabras=="education"))


ttff<-tabla_frecuencia[which(tabla_frecuencia$palabras=="education")]
which(tabla_frecuencia$palabras=="mexico")



length(which(tabla_frecuencia$frecuencia > 100))

#####

install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)
#grafica
wordcloud(words = tabla_frecuencia$palabras,
          freq = tabla_frecuencia$frecuencia,
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
	  rot.per=0.35,
          colors = brewer.pal(8,"Paired"))

wordcloud2(data=tabla_frecuencia, size = 0.7, shape = 'pentagon')

wordcloud(words = tabla_frecuencia$palabras
        , scale=c(3,0.01)     # Set min and max scale
        , max.words=100      # Set top n words
        , random.order=FALSE # Words in decreasing freq
        , rot.per=0.35       # % of vertical words
        , use.r.layout=FALSE # Use C++ collision detection
        , colors=brewer.pal(8, "Dark2"))
