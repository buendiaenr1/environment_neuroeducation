
#### enrique r.p. buendia lozada v3 nov.2022

#install.packages("dplyr")


library(dplyr)
library(stringr)


is_empty <- function(x) {
  if (length(x) == 0 & !is.null(x)) {
    TRUE
  } else {
    FALSE
  }
}



buu2 <- function(cad,registro){

	#tt <- c("gggggg kkkk disorder yyyy fuel kkkk kkk",
	#	"kkkkkk fuel disorder jjjjj",
	#	"gggggg dddd kkkk disorder xcv xvxcv vcxxcv cxvx vxcvxcx xcvxc trigger mmmmm ")
	
	tt=cad	
	
	# las dos palabras al mismo tiempo
	terms1 = c("illness|disease|sickness|disorder|ailment|complaint|epidemic|cogni|relat|link|contribu|neuro|pedag|didac|learn|teach")
	terms2 = c("provoke|trigger|induce|elicit|drive|promote|draw|fuel|needle|symptom|acumm|conne|depend|reference|relative")
	if(grepl(terms1,tt)&grepl(terms2,tt)){

	
	# ubicacion de los pares de palabras (donde inician?)
	words <- unlist(str_split(tt, " "))
	vv <- which(grepl(terms1,words))
	vv<-unique(vv)
	cat(" Pos 1: ",vv,"\n  palabras encontradas:> ",words[vv],"\n")

	vv1 <- which(grepl(terms2,words))
	vv1<-unique(vv1)
	cat(" Pos 2: ",vv1,"\n  palabras encontradas:> ",words[vv1],"\n")

	rr<-sort(unique(c(vv,vv1)))
	cat(" Registro ",registro," Posiciones : ",rr,"\n")

	if(length(rr) <= 1){return(0)}
	n=length(rr)-1
	d=c()
	for (i in 1:n){
		d=c(d,abs(rr[i]-rr[i+1]))
	}
	
	if (length(d) >= 1){
		
# mostrar lo mejor que se pueda
		
		fuente    <-c()
		ini=1
		cont=1
		for(i in rr){
			fin=i-1
			for(j in ini:fin){
				fuente=c(fuente,words[j])
				cont=cont+1
				if(cont>10){
					fuente=paste(fuente,collapse=" ")
					print(fuente)
					fuente=c()
					cont=0
				}
			}
			fuente=c(fuente,"[_:>",words[i],"<:_]")
			ini=i+1
		}
		
		fuente=paste(fuente,collapse=" ")
		print(fuente)
		cat("  distancias entre palabras : ",d,"\n")
		

		return(1)
	}else{ return(0)}
	}else{ return(0)}
}


dat2<-readLines("sd_f.csv")

fras=0
cont=0
si=0
reg=c()
for (i in 1:length(dat2)){
	##dd=buu2(dat2[i],i)
	#dd=dd[!is.na(dd)]
	#if(length(dd) >= 0 | dd[1] != "") {cat(" dd= ",dd,"\n")}
	si<-buu2(dat2[i],i)
	if (si == 1) {reg=c(reg,i)}
	fras=fras+si
	cont=cont+1
	readline(prompt="Press [enter] to continue")
}
cat(" Registros revisados                          : ",length(dat2),"\n")
cat(" Cantidad de registros con frases importantes : ",fras,"\n")
cat(" Número de registro con frases importantes    : ",reg,"\n")

fileConn<-file("final.txt")
writeLines(dat2[reg], fileConn)
close(fileConn)


