# SCRIPT TO POPULATE A TABLE OF country_iso,province_id,country_wos,country_wos_biol,country_wos_cons,province_wos,province_wos_biol,province_wos_cons WITH WEB OF SCIENCE SCI PUBLICATION NUMBERS

library("tidyverse")
library("sf")
library("rwos")
library("countrycode")
library("RCurl")
library("xml2")

GADMSHP_FILE																		<- "data/gadm28_adm1_multi.gpkg"
TIMESTAMP																				<- make.names(format(Sys.time(), "%b-%Y"))
MAIN_PATH																				<- "results/"
INPUT_FILE																			<- "data/empty_data.csv"
MAP_OUTPUT_FILE																	<- paste0(INPUT_FILE,TIMESTAMP,"_map.gpkg")
TAB_OUTPUT_FILE																	<- paste0(INPUT_FILE,TIMESTAMP,"_data.csv")

WOS_RESULT_FOLDER																<- paste0("WOSQUERIES",TIMESTAMP)
WOSCOUNTRYSTART 																=  1993 # SHOULD BE 1993 IN FINAL
WOSCOUNTRYEND																		=  2016
WOSPROVINCESTART 																=  1993
WOSPROVINCEEND																	=  2016
#WOSSID 																					<- wos_authenticate()
COUNTRY.LANG			 															<- c("de","ar","fr","en","es","ru","zh")
WOSPATH																					<- paste0(MAIN_PATH,WOS_RESULT_FOLDER,"/")
TIMER																						<- Sys.time()

wos_search_sci_ssci 														<- function(sid, query = "",
                       api = "lite",
                       editions = if (api == "lite") c("SCI", "SSCI")) {
	# THIS FUNCTION IS A MODIFICATION OF gihub/juba/rwos (I CHANGED THE EDITIONS ONLY!)
  if (api == "lite") {

    ## Editions tags
    editions_str <- paste0("<editions><collection>WOS</collection><edition>",
                           editions,
                           "</edition></editions>",
                           collapse = "\n")

    ## SOAP request
    body <- paste0('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
  xmlns:woksearchlite="http://woksearchlite.v3.wokmws.thomsonreuters.com">
    <soapenv:Header/>
    <soapenv:Body>
    <woksearchlite:search>
    <queryParameters>
    <databaseId>WOS</databaseId>
    <userQuery>', query, '</userQuery>',
    editions_str,
    '<queryLanguage>en</queryLanguage>
    </queryParameters>
    <retrieveParameters>
    <firstRecord>1</firstRecord>
    <count>100</count>
    </retrieveParameters>
    </woksearchlite:search>
    </soapenv:Body>
    </soapenv:Envelope>')

    url <- "http://search.webofknowledge.com/esti/wokmws/ws/WokSearchLite"
  }

  if (api == "premium") {

    body <- paste0('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
  xmlns:woksearchlite="http://woksearch.v3.wokmws.thomsonreuters.com">
    <soapenv:Header/>
    <soapenv:Body>
    <woksearch:search>
    <queryParameters>
    <databaseId>WOS</databaseId>
    <userQuery>', query, '</userQuery>
    <editions>
    <collection>WOS</collection>
    <edition>SCI</edition>
    </editions>
    <queryLanguage>en</queryLanguage>
    </queryParameters>
    <retrieveParameters>
    <firstRecord>1</firstRecord>
    <count>100</count>
    </retrieveParameters>
    </woksearch:search>
    </soapenv:Body>
    </soapenv:Envelope>')

    url <- "http://search.webofknowledge.com/esti/wokmws/ws/WokSearch"
  }


  headers <- c(
    Accept = "multipart/*",
    'Content-Type' = "text/xml; charset=utf-8",
    'Cookie' = paste0("SID=", sid),
    SOAPAction = ""
  )

  h <- RCurl::basicTextGatherer()
  RCurl::curlPerform(
    url = url,
    httpheader = headers,
    postfields = body,
    writefunction = h$update
  )

  resp <- xml2::read_xml(h$value())

  err <- xml2::xml_find_first(resp, xpath = ".//faultstring")
  if (length(err) > 0) {
    stop("Error : ", xml2::xml_text(err))
  }

  results <- as.numeric(xml_text(xml_find_first(resp, xpath = "//return/recordsFound")))
  query_id <- xml_text(xml_find_first(resp, xpath = "//return/queryId"))

  cat(paste0(results, " records found"))

  return(list(sid = sid, results = results, id = query_id))

}
clean 																					<- function(filenames){
	for (filename in filenames) {
		if (file.exists(filename)) file.remove(filename)
	}
	return(NULL)
}
buildcountryquery 															<- function(iso3,province = NULL,cat=NULL) {	
	query <- ""

	print(paste("DOING COUNTRY",iso3,"PROVINCE",province,"CATEGORY",cat))
	
	country_names <- GADM0SHP %>% as.data.frame() %>% filter(ISO == iso3) %>% select(starts_with("NAME_")) %>% top_n(1) %>% unique() %>% unlist(., use.names=FALSE) %>% droplevels() %>% levels() 
	print(country_names)
	
	for (z in (1:length(country_names))){	
		if (!is.null(country_names[z]) & !is.na(country_names[z]) & tolower(country_names[z]) != "<null>" & !is.null(cleanstr(country_names[z]))) 
			if (query=="")
				query<-paste(query,'(TS = \'',cleanstr(country_names[z]),'\'',sep="")
			else
				query<-paste(query,' OR TS = \'',cleanstr(country_names[z]),'\'',sep="")	
	}
	
	for (lang in COUNTRY.LANG) {
		mtry <- try(countrycode(iso3, "iso3c", paste('country.name.',lang,sep="")))
		if (!inherits(mtry, "try-error")) {
			if (query=="") {
				# SEE: MIGHT NEED CLEANSTRING!				
				query<-paste('(')				
				mtry <- try(countrycode(iso3, "iso3c", "country.name.en"))
				if (!inherits(mtry, "try-error")) if (!(is.na((countrycode(iso3,"iso3c","country.name.en"))))) 	query<-paste(query,'TS = \'',cleanstr(countrycode(iso3,"iso3c","country.name.en")),'\'',sep="")
				
				mtry <- try(countrycode(iso3, "iso3c", "p4.name"))
				if (!inherits(mtry, "try-error")) if (!(is.na((countrycode(iso3,"iso3c","p4.name"))))) 				query<-paste(query,' OR TS = \'',cleanstr(countrycode(iso3, "iso3c", 'p4.name'		)),'\'',sep="")
					
				mtry <- try(countrycode(iso3, "iso3c", "undp.name"))
				if (!inherits(mtry, "try-error")) if (!(is.na((countrycode(iso3,"iso3c","undp.name"))))) 			query<-paste(query,' OR TS = \'',cleanstr(countrycode(iso3, "iso3c", 'undp.name'	)),'\'',sep="")

				mtry <- try(countrycode(iso3, "iso3c", "wb_api.name"))
				if (!inherits(mtry, "try-error")) if (!(is.na((countrycode(iso3,"iso3c","wb_api.name"))))) 		query<-paste(query,' OR TS = \'',cleanstr(countrycode(iso3, "iso3c", 'wb_api.name')),'\'',sep="")

				mtry <- try(countrycode(iso3, "iso3c", "un.name.en"))
				if (!inherits(mtry, "try-error")) if (!(is.na((countrycode(iso3,"iso3c","un.name.en"))))) 		query<-paste(query,' OR TS = \'',cleanstr(countrycode(iso3, "iso3c", 'un.name.en'	)),'\'',sep="")

				if (!(is.na((countrycode(iso3, "iso3c", paste('country.name.',lang,sep="")))))) query<-paste(query,' OR TS = \'',cleanstr(countrycode(iso3, "iso3c", paste('country.name.',lang,sep=""))),'\'',sep="")	
			}
			else 
				if (!(is.na((countrycode(iso3, "iso3c", paste('country.name.',lang,sep="")))))) query<-paste(query,' OR TS = \'',cleanstr(countrycode(iso3, "iso3c", paste('country.name.',lang,sep=""))),'\'',sep="")	
		}
	}
	

	
	query<-paste(query,' )',sep="")
	
	# part II (the ones to be excluded)
	# not_query <- NOTS[NOTS$iso3 == iso3,]
	# if (!is.null(not_query)) query<-paste(query,not_query,sep="")	

	# part III (the province)
	if (!is.null(province[1])){
		province_query <- ""
		for (name in province) {
			# FIXME add and test gsbub <null> below
			if (is.null(name) || is.na(name) || name=="<Null>" || name==" " || name=="" || is.null(name) || gsub("[[:punct:]]", "",name) =="" || gsub("[[:punct:]]", "",name) ==" " || gsub("[[:punct:]]", "",name) =="  " || gsub("[[:punct:]]", "",name) =="NA") next
			if (province_query=="") province_query<-paste(' AND (TS = \'',cleanstr(name),'\' ',sep="")
			else province_query<-paste(province_query,' OR TS = \'',cleanstr(name),'\' ',sep="")
		}
		province_query<-paste(province_query,')',sep="")
		query<-paste(query,province_query)
	}

	biology_query <-  ' AND WC=(\'*ecolog*\' OR \'*environment*\' OR \'*evolution*\' OR \'*biol*\' OR \'Geogra*\' OR \'Zool*\' OR \'Ornitho*\' OR \'Plant*\' OR \'Biodiversity Conservation\')'
	conservation_query <- ' AND WC=\'Biodiversity Conservation\''
	
	# Categories from https://images.webofknowledge.com/WOKRS511B4_1/help/WOS/hp_subject_category_terms_tasca.html

	if	(cat == "biol") query<-paste(query,biology_query,sep="")
	if	(cat == "cons") query<-paste(query,conservation_query,sep="")
	
	if (is.null(province[1])){
		# part IV (the year)
		year_query <- ""
		for (year in WOSCOUNTRYSTART:WOSCOUNTRYEND){
			if (year_query=="") {
				year_query<-paste(' AND (PY = ',year,sep="")
			}
			else 
				year_query<-paste(year_query,' OR PY = ',year,sep="")
		}
		year_query<-paste(year_query,' )',sep="")
		
		query<-paste(query,year_query,sep="")	
	}
	else {
		# part IV (the year)
		year_query <- ""
		for (year in WOSPROVINCESTART:WOSPROVINCEEND){
			if (year_query=="") {
				year_query<-paste(' AND (PY = ',year,sep="")
			}
			else 
				year_query<-paste(year_query,' OR PY = ',year,sep="")
		}
		year_query<-paste(year_query,' )',sep="")
		
		query<-paste(query,year_query,sep="")	
	}
	print(query)
	return(query)
}
runcountryquery 																<- function(query1) {
	res1 = tryCatch({
	    wos_search_sci_ssci(WOSSID,query1)
	}, warning = function(w) {
		Sys.sleep(.1)
		res1 <- try(wos_search_sci_ssci(WOSSID,query1))
		w
	}, error = function(e) {
		Sys.sleep(.5)
		res1 <- try(wos_search_sci_ssci(WOSSID,query1))
		e
	}, finally = {
		Sys.sleep(.5)
		res1 <- try(wos_search_sci_ssci(WOSSID,query1))
	})
}
runwos 																					<- function(iso3,lvl=0,provinceID=NULL,provincename = NULL,cat="all"){
	if (as.numeric((Sys.time()-TIMER), units="hours")%/%2 > 0) {
		print("reset WOS ID due to time (2hrs)")
		print(as.numeric((Sys.time()-TIMER), units="hours")%/%2)
		TIMER 																	<<- Sys.time()
		WOSSID 																	<<- wos_authenticate()
		WOSSID
	}
	resfilename	<- 	paste(WOSPATH,iso3,lvl,"_",provinceID,"_wos_country_",cat,".csv",sep="")	
	if (!(file.exists(resfilename))) {
		# # TO JUST READ CACHED FILES, USE
		# return(0)
		
		query1																	<-	buildcountryquery(iso3,provincename,cat)
		res																			<-	runcountryquery(query1)
		type 																		<- 	"Freq_Occurrence"	            		
		if(is.null(provincename)) provincename 	<- ""		
		if(is.null(res$results)) res$results 		<- NA	
			
		else if (as.integer(res$id) > 2400) {
			print("reset WOS ID due to ID number (>2400)")
			TIMER 																<<- Sys.time()
			WOSSID 																<<- wos_authenticate()
		}
		result																	<- 	data.frame(iso3,provincename[1],type,cat,query1,WOSPROVINCESTART,WOSPROVINCEEND,as.integer(res$results))		
		names(result)														<-c("iso3","province","type","cat","query1","start","end","TOTAL")
		# ONLY WRITE THE NON 0 VALUES
		if(!is.na(res$results) & as.integer(res$results)>0) {
			clean(cbind(paste0(resfilename,"_NA.csv")))
			write.table(result, resfilename , sep = ",", col.names = colnames(result),row.names=FALSE)
		}
		else if(is.na(res$results) | as.integer(res$results)==0) {
			clean(cbind(paste0(resfilename,"_NA.csv")))
			write.table(result, paste0(resfilename,"_NA.csv") , sep = ",", col.names = colnames(result),row.names=FALSE)
		}
	}
	else
		result 																	<-	read.csv(file = resfilename, header = TRUE, stringsAsFactors = FALSE)	
	
	if (is.na(result$TOTAL)) result$TOTAL <- 0
	
	return(result$TOTAL)
}
cleanstr																				<- function(str){	
	str<-tolower(str)
	# x<-gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "",gsub("k.","k ",gsub("u.s","us ",gsub("u.s.a","usa ",gsub("l\'","l ",gsub("d\'","d ",gsub("-","XXXXXXX",str,fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
	#
	# if (is.null(str) | is.na(str) | x == "" | x == " " | (str=="<NULL>")) return(NULL)
	
	if (grepl("-", str, fixed=TRUE)) 	return(paste0(
									gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("k.","k ",gsub("u.s","us ",gsub("u.s.a","usa ",gsub("l\'","l ",gsub("d\'","d ",gsub("-","XXXXXXX",str,fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
									'\' OR TS = \'',
									gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", " ",gsub("k.","k ",gsub("u.s","us ",gsub("u.s.a","usa ",gsub("l\'","l ",gsub("d\'","d ",gsub("-","XXXXXXX",str,fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
									'\' OR TS = \'',
									gsub("XXXXXXX"," - ",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("-","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
									'\' OR (TS = \'',
									as.list(strsplit(gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("-","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), '-',fixed=TRUE)[[1]])[1],
									'\' AND TS = \'',
									as.list(strsplit(gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("-","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), '-',fixed=TRUE)[[1]])[2],
									'\') OR TS = \'',
									gsub("XXXXXXX"," ",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("-","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
									
									))
	if (grepl(" and ", str, fixed=TRUE)) 	return(paste0(
									gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("k.","k ",gsub("u.s","us ",gsub("u.s.a","usa ",gsub("l\'","l ",gsub("d\'","d ",gsub("-","XXXXXXX",str,fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
									'\' OR TS = \'',
									gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", " ",gsub("k.","k ",gsub("u.s","us ",gsub("u.s.a","usa ",gsub("l\'","l ",gsub("d\'","d ",gsub("-","XXXXXXX",str,fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
									'\' OR TS = \'',
									gsub("XXXXXXX"," - ",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("-","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
									'\' OR (TS = \'',
									as.list(strsplit(gsub("XXXXXXX"," and ",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub(" and ","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), ' and ',fixed=TRUE)[[1]])[1],
									'\' AND TS = \'',
									as.list(strsplit(gsub("XXXXXXX"," and ",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub(" and ","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), ' and ',fixed=TRUE)[[1]])[2],
									'\') OR TS = \'',
									gsub("XXXXXXX"," ",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("-","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)							
									))
	return(
		paste0(gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("k.","k ",gsub("u.s","us ",gsub("u.s.a","usa ",gsub("l\'","l ",gsub("d\'","d ",gsub("-","XXXXXXX",str,fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
		'\' OR TS = \'',
		gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", " ",gsub("k.","k ",gsub("u.s","us ",gsub("u.s.a","usa ",gsub("l\'","l ",gsub("d\'","d ",gsub("-","XXXXXXX",str,fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
		))
}
meanWithoutNA																		<- function(x) {
   mean(x[which(!is.na(x))])
}
# CREATE THE BASE FOLDER
dir.create(file.path(MAIN_PATH,WOS_RESULT_FOLDER), showWarnings = T)
clean(cbind(MAP_OUTPUT_FILE,TAB_OUTPUT_FILE))

GADM_COMPLETED_INPUT_TABLE											<- read_csv(file = INPUT_FILE,na=c(""," ","NA"))

GADMSHP																					<- st_read(GADMSHP_FILE)

GADM_COMPLETED_INPUT_TABLE$province_wos 				<-0
GADM_COMPLETED_INPUT_TABLE$province_wos_biol 		<-0
GADM_COMPLETED_INPUT_TABLE$province_wos_cons 		<-0
GADM_COMPLETED_INPUT_TABLE$country_wos 					<-0
GADM_COMPLETED_INPUT_TABLE$country_wos_biol 		<-0
GADM_COMPLETED_INPUT_TABLE$country_wos_cons 		<-0

# CHECK TO REMOVE DUPLICATES
GADM_COMPLETED_INPUT_TABLE 											<- GADM_COMPLETED_INPUT_TABLE 	%>% group_by(ISO,ID_1,province_file_id) %>% filter(row_number() == 1)

glimpse(GADM_COMPLETED_INPUT_TABLE)
print(nrow(GADM_COMPLETED_INPUT_TABLE))

for (z in (1:1)){
	for (i in (1:nrow(GADM_COMPLETED_INPUT_TABLE))){
		if(is.null(GADM_COMPLETED_INPUT_TABLE[i,]$province_wos) | is.na(GADM_COMPLETED_INPUT_TABLE[i,]$province_wos) | GADM_COMPLETED_INPUT_TABLE[i,]$province_wos == 0) GADM_COMPLETED_INPUT_TABLE[i,]$province_wos <-
		runwos(
			iso						=	GADM_COMPLETED_INPUT_TABLE[i,]$ISO,
			lvl						=	1,
			provinceID		=	GADM_COMPLETED_INPUT_TABLE[i,]$ID_1,
			provincename	=	as.list(strsplit(paste(paste(GADM_COMPLETED_INPUT_TABLE[i,]$province_name,GADM_COMPLETED_INPUT_TABLE[i,]$VARNAME_1,sep="|"),GADM_COMPLETED_INPUT_TABLE[i,]$NL_NAME_1,sep="|"), '|',fixed=TRUE)[[1]]),
			cat						=	"all"
			)

		if(is.null(GADM_COMPLETED_INPUT_TABLE[i,]$province_wos_biol) | is.na(GADM_COMPLETED_INPUT_TABLE[i,]$province_wos_biol) |GADM_COMPLETED_INPUT_TABLE[i,]$province_wos_biol == 0) GADM_COMPLETED_INPUT_TABLE[i,]$province_wos_biol <-
		runwos(
			iso						=	GADM_COMPLETED_INPUT_TABLE[i,]$ISO,
			lvl						=	1,
			provinceID		=	GADM_COMPLETED_INPUT_TABLE[i,]$ID_1,
			provincename	=	as.list(strsplit(paste(paste(GADM_COMPLETED_INPUT_TABLE[i,]$province_name,GADM_COMPLETED_INPUT_TABLE[i,]$VARNAME_1,sep="|"),GADM_COMPLETED_INPUT_TABLE[i,]$NL_NAME_1,sep="|"), '|',fixed=TRUE)[[1]]),
			cat						=	"biol"
			)

		if(is.null(GADM_COMPLETED_INPUT_TABLE[i,]$province_wos_cons) | is.na(GADM_COMPLETED_INPUT_TABLE[i,]$province_wos_cons) | GADM_COMPLETED_INPUT_TABLE[i,]$province_wos_cons == 0) GADM_COMPLETED_INPUT_TABLE[i,]$province_wos_cons <-
		runwos(
			iso						=	GADM_COMPLETED_INPUT_TABLE[i,]$ISO,
			lvl						=	1,
			provinceID		=	GADM_COMPLETED_INPUT_TABLE[i,]$ID_1,
			provincename	=	as.list(strsplit(paste(paste(GADM_COMPLETED_INPUT_TABLE[i,]$province_name,GADM_COMPLETED_INPUT_TABLE[i,]$VARNAME_1,sep="|"),GADM_COMPLETED_INPUT_TABLE[i,]$NL_NAME_1,sep="|"), '|',fixed=TRUE)[[1]]),
			cat						=	"cons"
			)

		if(is.null(GADM_COMPLETED_INPUT_TABLE[i,]$country_wos) | is.na(GADM_COMPLETED_INPUT_TABLE[i,]$country_wos) |GADM_COMPLETED_INPUT_TABLE[i,]$country_wos == 0) GADM_COMPLETED_INPUT_TABLE[i,]$country_wos <-
		runwos(
			iso						=	GADM_COMPLETED_INPUT_TABLE[i,]$ISO,
			lvl						=	0,
			cat						=	"all"
			)

		if(is.null(GADM_COMPLETED_INPUT_TABLE[i,]$country_wos_biol) | is.na(GADM_COMPLETED_INPUT_TABLE[i,]$country_wos_biol) |GADM_COMPLETED_INPUT_TABLE[i,]$country_wos_biol == 0) GADM_COMPLETED_INPUT_TABLE[i,]$country_wos_biol <-
		runwos(
			iso						=	GADM_COMPLETED_INPUT_TABLE[i,]$ISO,
			lvl						=	0,
			cat						=	"biol"
			)

		if(is.null(GADM_COMPLETED_INPUT_TABLE[i,]$country_wos_cons) | is.na(GADM_COMPLETED_INPUT_TABLE[i,]$country_wos_cons) |GADM_COMPLETED_INPUT_TABLE[i,]$country_wos_cons == 0) GADM_COMPLETED_INPUT_TABLE[i,]$country_wos_cons <-
		runwos(
			iso						=	GADM_COMPLETED_INPUT_TABLE[i,]$ISO,
			lvl						=	0,
			cat						=	"cons"
			)
		print(GADM_COMPLETED_INPUT_TABLE[i,]$province_wos)
	}
}
																																																																		
GADM_WRITE 																			<- GADM_COMPLETED_INPUT_TABLE %>% select(country_iso,starts_with('province_'),starts_with('country_'))  %>% as_tibble()

write.table(GADM_WRITE, paste0(TAB_OUTPUT_FILE,"_fin.csv") , sep = ",", col.names = colnames(GADM_WRITE),row.names=FALSE)

GADMSHP %>%	inner_join(GADM_COMPLETED_INPUT_TABLE) %>% st_write(MAP_OUTPUT_FILE)							