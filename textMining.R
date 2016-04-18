#Loading some libraries
#library(lattice)
#require(MASS)
#require(grid)
#require(colorspace)
library(stringr)
library(DBI)
library(RMySQL)
#library(tm)
#library(RColorBrewer)
#library(wordcloud)
#library(biclust)
#library(cluster)
#library(igraph)
#library(fpc)

#Connection with mySQL
mcon = dbConnect(MySQL(), user='root', password='',dbname='dbtest', host='localhost') 

##LOADING TEXTS FOR EXTRACTION
#MediaCongo webpages for job opp : http://www.mediacongo.net
mdX1 <- readLines("http://mediacongo.net/emplois.html")
mdX2 <- readLines("http://mediacongo.net/emplois-titre--societe--page-2.html");
mdX3 <- readLines("http://mediacongo.net/emplois-titre--societe--page-3.html")
mdX4 <- readLines("http://mediacongo.net/emplois-titre--societe--page-4.html")
#TonJob webpages for job opp : http://www.tonjob.net/
tbX <- readLines("http://www.tonjob.net")
#RDC News webpages for job opp : http://www.rdcnews.net
rdX <- readLines("http://www.rdcnews.net/job-offer/")
#Sesomo webpages for job opp : http://http://recrutement.sesomo.cd/
soX  <- readLines("http://recrutement.sesomo.cd/home.php?page=0")
soX1 <- readLines("http://recrutement.sesomo.cd/home.php?page=1")
soX2 <- readLines("http://recrutement.sesomo.cd/home.php?page=2")

##EXPLORATORY DATA ANALYSIS
#Encoding function : French characters
enFrench <- function (eX){
   eX <- str_replace_all (eX,"â€™","")
   eX <- str_replace_all (eX,"ã´","o")
   eX <- str_replace_all (eX,"â¯","i")
   eX <- str_replace_all (eX,"ã©","e")
   eX <- str_replace_all (eX,"ã","a")
   eX <- str_replace_all (eX,"â€™","")
   eX <- str_replace_all (eX,"ã´","o")
   eX <- str_replace_all (eX,"â¯","i")
   eX <- str_replace_all (eX,"ã©","e")
   eX <- str_replace_all (eX,"dâ€™u","du")
   eX <- str_replace_all (eX,"dâ€™i","di")
   eX <- str_replace_all (eX,"a¯","i")
   eX <- str_replace_all (eX,"aâ€“","")
   eX <- str_replace_all (eX,"aª","e")
   eX <- str_replace_all (eX,"dâ€™e","de")
   eX <- str_replace_all (eX,"dâ€™a","da")
   eX <- str_replace_all (eX,"dâ€™o","do")
   eX <- str_replace_all (eX,"a¨","e")
   eX <- str_replace_all (eX,"™","")
   eX <- str_replace_all (eX,"\u20AC","")
   eX <- str_replace_all (eX,"dâ","d")
  
   return (eX)
}
FR <- function(eX)
{
    eX <- str_replace_all(eX,"â€™","")
	return(eX)
}
#Function for TonJob : tX Data retrieval
dR <- function (data){
#Treatment key word in the data
    stL <- "offre-demploi-"
	ct <- unlist(gregexpr(pattern = stL,data))
	if (ct > 1) {
	    st <- substr(data,ct+str_count(stL),1000)
	}else{
	    b <- "offres-demploi-"
		ct <- unlist(gregexpr(pattern = b,data))
       	st <- substr(data,ct+str_count(b),1000)
	}
	v  <- unlist(gregexpr(pattern = '/">', st)) - 1
    substr(st,1,v)
}
#Function for TonJob
tXC <- function (r,p,data,tab)
{
#Retrieval of the name of the company
	 a <- p-r #Here we have the difference between two main factors	
	if (as.integer(tab[length(tab)]) <= r)#Condition when the last factor is equal to the factor in the loop
	{ 
	   #print("Last factor in the first grep")	   	   
	   rightLine <- NULL
	   for (k in 1:200){ 
	   #200 is a value which does not make sense. But we go 200 lines more to find if we can find the pattern. Probably before 200th line we will find
		    reject <- NULL
		    vx <- r+k
            val <- unlist(gregexpr(pattern = 'Entreprise:', data[vx]))
			
		            if (val > 1)
					    {rightLine <- data[vx]} #The expected value
					else{ reject <- NULL} #The value to reject because it is unuseful
	    }
	    #Substring with GSUB function : The patterns are related to the data
		rightLine <- gsub("\t\t\t\t\t<b>Entreprise:</b>","",rightLine)
		#rightLine <- gsub("\t\t\t\t\t <br/><b>Salaire:</b>  \t\t\t\t\t <br/><b>Job category:</b>","",rightLine)
		nb <- unlist(gregexpr(pattern = '<b>Salaire:</b>', rightLine)) - 1 
        rightLine <- substr(rightLine,1,nb)
        rightLine <- gsub("\t\t\t\t\t <br/>","",rightLine)		
		rightLine <- str_trim(rightLine)
		#print(rightLine)
	   #cat(as.integer(tab[length(tab)]),as.integer(tab[i]),sep=",")
	}else{
	   #print ("Factors between main factors in the first grep")	   
	   rightLine <- NULL
	   for (k in 1:a){ 
	     
		    reject <- NULL
		    vx <- r+k
            val <- unlist(gregexpr(pattern = 'Entreprise:', data[vx]))
		
		            if (val > 1)
					    {rightLine <- data[vx]}#The expected value
					else{ reject <- NULL}#The value to reject because it is unuseful
	    }
	    #Substring with GSUB function : The patterns are related to the data
		rightLine <- gsub("\t\t\t\t\t<b>Entreprise:</b>","",rightLine)
		#rightLine <- gsub("\t\t\t\t\t <br/><b>Salaire:</b>  \t\t\t\t\t <br/><b>Job category:</b>","",rightLine)
		nb <- unlist(gregexpr(pattern = '<b>Salaire:</b>', rightLine)) - 1 
        rightLine <- substr(rightLine,1,nb)
        rightLine <- gsub("\t\t\t\t\t <br/>","",rightLine)		
		rightLine <- str_trim(rightLine)
		#print(rightLine)
	}
	#print(tab[length(tab)])
	#print(class(a)
	return(rightLine)
}
#Function for TonJob
tXDate <- function (r,p,data,tab)
{
#Retrieval of the name of the company
	 a <- p-r #Here we have the difference between two main factors	
	if (as.integer(tab[length(tab)]) <= r)#Condition when the last factor is equal to the factor in the loop
	{ 
	   #print("Last factor in the first grep")	   	   
	   rightLine <- NULL
	   for (k in 1:200){ 
	   #200 is a value which does not make sense. But we go 200 lines more to find if we can find the pattern. Probably before 200th line we will find
		    reject <- NULL
		    vx <- r+k
            val <- unlist(gregexpr(pattern = '<div id="date"><span class="year">', data[vx]))
		
		            if (val > 1)
					    {rightLine <- data[vx]} #The expected value
					else{ reject <- NULL} #The value to reject because it is unuseful
	    }
	rightLine <- gsub ("<div id=\"date\"><span class=\"year\">","",rightLine)
	rightLine <- gsub ("</span></div>","",rightLine)
	}else{
	   #print ("Factors between main factors in the first grep")	   
	   rightLine <- NULL
	   for (k in 1:a){ 
	     
		    reject <- NULL
		    vx <- r+k
            val <- unlist(gregexpr(pattern = '<div id="date"><span class="year">', data[vx]))
			
		            if (val > 1)
					    {rightLine <- data[vx]}#The expected value
					else{ reject <- NULL}#The value to reject because it is unuseful
	    }
	rightLine <- gsub ("<div id=\"date\"><span class=\"year\">","",rightLine)
	rightLine <- gsub ("</span></div>","",rightLine)  
	}
	
	rightLine <- str_replace_all(rightLine,'/',';')
	M <- unlist(gregexpr(pattern = ';', rightLine))
	month <- substr(rightLine,M[1]+1,M[2]-1)
	day <- substr(rightLine,1,M[1]-1)
	year <- substr(rightLine,M[2]+1,str_length(rightLine))
	
	month <- tolower(month)
	month <- str_replace_all(month,"ã©","e")
	if (month == "janv" || month == "jan" || month == "janvier"){month='01'}
	if(month == "fev" || month== "fevr" || month == "fevrier"){month='02'}
	if(month == "mar" || month== "mars"){month='03'}
	if(month == "avr" || month== "av" || month == "avril"){month='04'}
	if(month == "main"){month='05'}
	if(month == "jui" || month== "juin" || month == "jun"){month='06'}
	if(month == "juil" || month== "juillet"){month='07'}
	if(month == "aou" || month== "aout"){month='08'}
	if(month == "sep" || month== "sept" || month == "septembre"){month='09'}
	if(month == "oc" || month== "oct" || month == "octobre"){month='10'}
	if(month == "no" || month== "nov" || month == "novembre"){month='11'}
	if(month == "dec" || month== "de" || month == "decembre"){month='12'}
	
	rightLine <- paste (str_trim(day),"-",month,"-",str_trim(year),sep="")
	
	return(rightLine)
}
#Function for TonJob
tXLoc <- function (data,tData)
{
#Retrieve the location of the offer if possible
#Semicolon as reference in line coming from the the factor
LenData <- str_length(data)
delim = unlist(gregexpr(pattern = ';',data))

L <- length(delim)
#Case there are many semicolon
if (L > 1){ sb <- substr (data, delim[L],LenData)}
else{sb <- substr (data,delim[L],LenData)}

#Removing unuseful characters in the data
sb <- gsub("</a></strong>","",sb)
sb <- gsub (";","",sb)

                if (str_length(sb) <= 30 && unlist(gregexpr(pattern = ';', sb)) == -1 && unlist(gregexpr(pattern = ',', sb)) == -1)
				{
                    sb <- str_trim(sb)
                }else{
				    
					del <- unlist(gregexpr(pattern = ',', sb))
					Ld  <- length(del)
				    if( Ld > 1){
					   sb <- substr (sb, del[Ld],str_length(sb))					   
					   if (str_length(sb) > 30)
					   {
					        sb <- 'N/A'
					   }else{
					        sb <- gsub("</a></strong>","",sb)
					        sb <- gsub (",","",sb)
					        sb <- str_trim(sb)
					   }
					}else{
					   sb <- substr (sb, del[Ld],str_length(sb))
					   sb <- gsub("</a></strong>","",sb)
					   sb <- gsub (",","",sb)
					   sb <- str_trim(sb)
					}
					
					d <- unlist(gregexpr(pattern = '-', sb))
					L <- length(d)
				    if (L > 1){
						sb <- substr (sb, d[L],str_length(sb))
						sb <- gsub("</a></strong>","",sb)
						sb <- gsub ("-","",sb)
						sb <- str_trim(sb)
				    }
					if (L == 1){
						sb <- substr (sb, d[L],str_length(sb))
						sb <- gsub("</a></strong>","",sb)
						sb <- gsub ("-","",sb)
						sb <- str_trim(sb)
				    }
				}
				
				if(str_length(sb) > 50){
				    d <- unlist(gregexpr(pattern = '-', sb))
					L <- length(d)
				    if (L > 1){
						sb <- substr (sb, d[L],str_length(sb))
						sb <- gsub("</a></strong>","",sb)
						sb <- gsub ("-","",sb)
						sb <- str_trim(sb)
				    }else{
						td <- unlist(gregexpr(pattern = '-', tData)) + 1
						Ld <- length(td)
						if(Ld > 1){
						    sb <- substr (tData,td[Ld],str_length(tData))
						}else{
						    sb <- 'N/A'
						}
				    }
					if (L == 1){
						sb <- substr (sb, d[L],str_length(sb))
						sb <- gsub("</a></strong>","",sb)
						sb <- gsub ("-","",sb)
						sb <- str_trim(sb)
				    }
				}
    if(str_length(sb) >= 30){
	    td <- unlist(gregexpr(pattern = '-', tData)) + 1
		d  <- length(td)
		tb <- substr (tData,td[d],str_length(tData))
	    
		T <- unlist(gregexpr(pattern = tolower(tb), tolower(tData)))
		
		if(T>1){
		    sb <- tb
		}else{
		    sb <- 'N/A'
		}
	}
return(str_trim(sb))
}
#Function for TonJob
tXPos <- function(LC,CP,SM){
#Retrieve the position 
LC <- tolower(LC) #From the result of location
CP <- tolower(CP) #From the result of company
SM <- tolower(SM) #From the result of details

val <- gsub(LC,"",SM)
val <- gsub(CP,"",val)
val <- gsub("-"," ",val)
return(str_trim(val))
}
#Function for TonJob
tX <- function (bZL){
#First grep as reference to find the offer details
        tab <- grep ('titlo',bZL)
		k <- NULL
		S <- 0 
             
			for (i in 1 : length(tab))
            { 
                    dSum <- dR(bZL[tab[i]+2]) #Retrieve the second line containing all details from the first grep and call the function dR
					#Treatment of main factors in order to retrieve the company name in the data with a new grep
					r <- as.integer(tab[i]+2)
					p <- as.integer(tab[i+1])
					dComp <- tXC(r,p,bZL,tab) #Here,the name of the company
					#Treatment for retrieving the location
					dLoc <- tXLoc(bZL[tab[i]+2],dSum)#Here,the location
					#Treatment for the positio
					dPos <- tXPos(dLoc,dComp,dSum)
					#Treatment to retrieve the date
					dDate <- tXDate(r,p,bZL,tab)
					#Validity
					dVal <- ''
					#JobSource
	                JS <- "www.tonjob.net"
	                QR <- paste("INSERT INTO jobopportunity (Position,Company,Location,PostedDate,Validity,JobSource) VALUES('",toupper(dPos),"','",toupper(dComp),"','",toupper(dLoc),"','",dDate,"','",dVal,"','",JS,"')")
                    dbGetQuery(mcon,QR)
                    S <- S + 1
            }
        print("Records");print(S)
}
#Function for RDC News
rXPos <- function (data){
    val <- unlist(gregexpr(pattern = 'Lire', data))-6
	data <- substr(data,1,val)
	val <- unlist(gregexpr(pattern = '.net/', data)) + 5
	data <- substr(data,val,str_length(data))
	data <- gsub ("-"," ",data)
	return(data)
}
#Function for RDC News
rXLoc <- function (data){
    tStOne <- "</span><span>"
	tStTwo <- "</span></div>"
	val <- unlist(gregexpr(pattern = tStOne, data)) + str_count(tStOne)
	data <- substr(data,val,str_length(data))
	data  <- gsub(tStTwo,"",data)
	data  <- str_trim(data)
	  
	    val <- unlist(gregexpr(pattern = '[(]', data))	- 1   
		if(length(val)> 1)
		{data <- substr(data,1,val[1])}
		else{data <- substr(data,1,val[1])}
		return(str_trim(data))
}
#Function for RDC News
rXComp <- function (data){
    tStOne <- "</span><span>"
	tStTwo <- "</span></div>"
	val  <- unlist(gregexpr(pattern = tStOne, data)) + str_count(tStOne)
	data <- substr(data,val,str_length(data))
	data <- gsub(tStTwo,"",data)
	data <- str_trim(data)
	return(data)
}
#Function for RDC News
rXDate <- function (data){
    tStOne <- "</span><span>"
	tStTwo <- "</span></div>"
	val <- unlist(gregexpr(pattern = tStOne, data)) + str_count(tStOne)
	data <- substr(data,val,str_length(data))
	data  <- gsub(tStTwo,"",data)
	data  <- str_trim(data)
	data  <- tolower(gsub ("Ã©","e",data))
	
	if(str_length(data) > 5){
	            regexp <- "([[:digit:]]{1,2}) ([[:alpha:]]+) ([[:digit:]]{4})"
				day   <- sub(pattern = regexp, replacement = "\\1", x = data) # returns the first part of the regular expression
				month <- sub(pattern = regexp, replacement = "\\2", x = data) # returns the second part
				year  <- sub(pattern = regexp, replacement = "\\3", x = data)
				
				if(str_count(str_trim(day))== 1){day <- paste('0',str_trim(day),sep="")}
				else{day <- day}
				
				month <- tolower(month)
				month <- str_replace_all(month,"é","e")
				if (month == "janv" || month == "jan" || month == "janvier"){month='01'}
				if(month == "fev" || month== "fevr" || month == "fevrier"){month='02'}
				if(month == "mar" || month== "mars"){month='03'}
				if(month == "avr" || month== "av" || month == "avril"){month='04'}
				if(month == "main"){month='05'}
				if(month == "jui" || month== "juin" || month == "jun"){month='06'}
				if(month == "juil" || month== "juillet"){month='07'}
				if(month == "aou" || month== "aout"){month='08'}
				if(month == "sep" || month== "sept" || month == "septembre"){month='09'}
				if(month == "oc" || month== "oct" || month == "octobre"){month='10'}
				if(month == "no" || month== "nov" || month == "novembre"){month='11'}
				if(month == "dec" || month== "de" || month == "decembre"){month='12'}
	            
				if(str_count(day)> 2){data <- "00-00-0000"}
				else{data  <- paste (str_trim(day),"-",month,"-",str_trim(year),sep="")}
	    }else{ data <- "00-00-0000"}
	return(data)
}
#Function for RDC News
rX <- function (bZL){
#First grep as reference to find the offer details
    tab <- grep ('single-row clearfix job',bZL)
    k <- NULL
    S <- 0
    for (i in 1 : length(tab))
    {
	  #Treatment to retrieve the position
	  dPos <- rXPos(str_trim(bZL[tab[i]+2]))
	  #Treatment to retrieve the location
	  dLoc <- rXLoc(str_trim(bZL[tab[i]+11]))
	  #Treatment to retrieve the company
	  dComp <- rXComp(str_trim(bZL[tab[i]+12]))
	  #Treatment to retrieve the postDate
	  dDate <- rXDate (str_trim(bZL[tab[i]+13]))
	  #JobSource
	  JS   <- "www.rdcnews.net"
	  JVaL <- ''
	  QRy <- paste("INSERT INTO jobopportunity (Position,Company,Location,PostedDate,Validity,JobSource) VALUES('",toupper(dPos),"','",toupper(dComp),"','",toupper(dLoc),"','",dDate,"','",str_trim(JVaL),"','",JS,"')")
	  dbGetQuery(mcon,QRy)
	  S <- S + 1
    }
}
#Function for MediaCongo
medXPos <- function (data){
#Treatment of the position
    pData <- data
    pData <- gsub("</strong></a></td>","",pData)
    pData <- gsub("'","",pData)
    pData <- str_trim(pData)
    return (pData)
}
#Function for MediaCongo
medXComp <- function (data){
#Treatment of the company name
    cData <- gsub("<td class=\"blue\"><a href=\"#\">","",data)
    cData <- gsub("</a></td>","",cData)
    cData <- gsub("'","",cData)
    cData <- str_trim (cData)
	return (cData)
}
#Function for MediaCongo
medXLoc <- function (data){
#Treatment of the location
    lData <- substr(data,41,1000)
	lData <- gsub("</a></td>","",lData)
	lData <- gsub("'","",lData)
	lData <- str_trim (lData)
	return (lData)
}
#Function for MediaCongo
medXDate <- function (data){
#Treatment of the date
    dData <- gsub("<td class=\"blue\"><a href=\"#\">","",data)
	dData <- gsub("</a></td>","",dData)
	dData <- gsub("'","",dData)
	dData <- str_trim(dData)
	dData <- str_replace_all(dData,"[.]","-")
	return (dData)
}
#Function for MediaCongo
medX <- function (bZL){
#Grep emploi-offre in the codesource
    tab <- grep('emploi-offre',bZL)
	k <- NULL
	J <- 0 
    for (i in 1 : length(tab)){ 
			JVaL  <- grep("clas3",bZL[tab[i]]) #1 : Expired job opp and 0: Valide job opp
			dPos  <- medXPos(enFrench(tolower(bZL[tab[i]+2])))#Line to retrieve the position #Call the function
			dComp <- medXComp(enFrench(tolower(bZL[tab[i]+3])))#Line to retrieve the company name #Call the function 
			dLoc  <- medXLoc (enFrench(tolower(bZL[tab[i]+4])))#Line to retrieve the location #Call the function
			dDate <- medXDate (bZL[tab[i]+5])#Line to retrieve the date #Call the function
			#dPos <- enFrench(dPos)
			print(dPos)
			print(dComp)
			print(dLoc)
			#JobSource
			JS <- "www.mediacongo.net"
			#k[i]<- paste (pval,pval2,pval3,pval4,JVaL,sep=",")
			QRy <- paste("INSERT INTO jobopportunity (Position,Company,Location,PostedDate,Validity,JobSource) VALUES('",toupper(dPos),"','",toupper(dComp),"','",toupper(dLoc),"','",dDate,"','",str_trim(JVaL),"','",JS,"')")
			dbGetQuery(mcon,QRy)
			J <- J + 1
    }
    print("Records");print(J)
}
#Function for Sesomo
sX <- function (bZL){
#First grep in the data of Sesomo
        tSt <- "_offre-emploi-"
        tab <- grep(tSt,bZL)
		k <- NULL
		S <- 0
		for(i in 1:length(tab)){
		    #Treatment to retrieve the position
			val <- unlist(gregexpr(pattern = tSt, bZL[tab[i]])) + str_count(tSt)
			st  <- substr(bZL[tab[i]],val,str_length(bZL[tab[i]]))
		    posVal <- st
			valOne <- unlist(gregexpr(pattern = "title",posVal))-3
			dPos <- substr(posVal,1,valOne)
			dPos <- gsub ("-"," ", dPos)
			dPos <- gsub (".html","", dPos)
			dPos <- gsub ("'","",dPos)
			#Treatment to retrieve the date
			valTwo <- unlist(gregexpr(pattern = "<small>",st))+7
			dDate <- str_trim(substr(st,valTwo,valTwo+10))
			#Treatment to retrieve the company
			dComp <- gsub("<td>","",bZL[tab[i]+1])
			dComp <- gsub("</td>","",str_trim(dComp))
			dComp <- gsub("'","",dComp)
			#Treatment to retrieve the location
			dLoc <- bZL[tab[i]+2]
			dLoc <- gsub ("</td>","",str_trim(dLoc))
			dLoc <- gsub ("<td>","",str_trim(dLoc))			 
			dLoc <- gsub ("ex","",str_trim(dLoc))
			dLoc <- gsub ("-","",str_trim(dLoc))
			dLoc <- gsub ("'","",dLoc)
			#Substring the dLoc
			valThree <- unlist(gregexpr(pattern = 'Zone',dLoc))
			    if (length(valThree) > 1){
					if(length(valThree)==2){dLoc <- paste(str_trim(substr(dLoc, valThree[1]+6,valThree[2]-3)),"-",str_trim(substr(dLoc,valThree[2]+6,str_count(dLoc))))}
					if(length(valThree)==3){dLoc <- paste(str_trim(substr(dLoc, valThree[1]+6,valThree[2]-3)),"-",str_trim(substr(dLoc, valThree[2]+6,valThree[3]-3)),"-",str_trim(substr(dLoc,valThree[3]+6,str_count(dLoc))))}
			    }else{
			        dLoc <- substr(dLoc, valThree+4, str_length(dLoc))
					dLoc <- str_trim(dLoc)
			    }
			#JobSource
			JS   <- "recrutement.sesomo.cd"
			JVaL <- ''
			QRy  <- paste("INSERT INTO jobopportunity (Position,Company,Location,PostedDate,Validity,JobSource) VALUES('",toupper(dPos),"','",toupper(dComp),"','",toupper(dLoc),"','",dDate,"','",str_trim(JVaL),"','",JS,"')")
            dbGetQuery(mcon,QRy)
			#print(QRy)
			S <- S + 1 
		}
}
#mdX area
AE <- medX (mdX1)
AE <- medX (mdX2)
AE <- medX (mdX3)
AE <- medX (mdX4)
AE <- tX(tbX)
AE <- rX(rdX)
AE <- sX(soX)
AE <- sX(soX1)
AE <- sX(soX2)

#Disconnection
dbListConnections(MySQL())
list()

#2.Autres sites
#3.Dplyr, SNA, etc
#ctree dans la library party