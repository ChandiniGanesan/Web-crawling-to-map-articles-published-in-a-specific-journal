#Project 5 R program to crawl, parse and extract all articles published in a specific journal
#Group : Chandini Ganesan, Hemanth Karumanchi, Ravali Gatta
#2019 Spring


#Journal Name: Hereditas
#Main page of the journal: https://hereditasjournal.biomedcentral.com

library(bitops)
library(RCurl)
library(XML)
library(stringr)
library(httr)

#FUNCTION NAME: generateGroupId
#FUNCTION:To generate the groupId for given lastnames of the team members
#INPUT:[vector with group member's lastnames]
#OUTPUT:[integer ranging from 0 to 9 corresponding to project title)]
generateGroupId<-function(Lname)
{
asc<-lapply(Lname,utf8ToInt)
Journal_ID<-(sum(asc[[1]], asc[[2]], asc[[3]])%%10)
Journal_ID
}

#FUNCTION NAME: getArticleDOIURL
#FUNCTION:extract DOI and url of article lists in the specified page
#INPUT:[page_url:the url of article list page]
#OUTPUT:[data.frame(DOI,URL:url of article full text)]
article.list=""
getArticleDOIURL = function(page_url){
  html = GET(page_url)
  doc = htmlParse(html, asText=TRUE)
  article.list = xpathSApply(doc,"//div[@class='c-teaser__group']/h3/a",xmlGetAttr,"href")
  #print(article.list)
  #DOI.list = xpathSApply(doc,"//ul[@class='c-teaser__view-options']",xmlGetAttr,"href")
  #gregexpr(pattern ="/10","/articles/10.1186/s41065-017-0040-6")
  #gregexpr(pattern ="/10",article.list)
  #x="/articles/10.1186/s41065-017-0040-6"
  #DOI.list=substr(x,10,nchar(x, type = "chars"))
  DOI.list=substr(article.list,gregexpr(pattern ="10\\.",article.list),nchar(article.list, type = "chars"))
  #print(DOI.list)
  return(data.frame(DOI=DOI.list,url=article.list))
}


#FUNCTION NAME: parseArticle
#FUNCTION:extract all required field from the specified article full text page
#INPUT:[DOI,URL:url of article full text]
#OUTPUT:[data.frame(DOI, title, author, authorAffiliation, correspondingAuthor,
#                   correspondingAuthorEmail, publicationDate, abstract, keywords, fullText)], single row
parseArticle = function(DOI, url){
  html = GET(url)
  doc = htmlParse(html, asText=TRUE)
  options(warn=-1)
  dir.create("HTMLs")
  article.filename = paste("HTMLs/", gsub("\\/","",DOI),".html",sep="")#generate article filename: DOI.html
  saveXML(doc, article.filename)#save to file
  
  author.info = getAuthors(doc)#function call:getAuthors()
  DOI = DOI;
  title = getAttributes(doc, "//meta[@name='citation_title']","content")
  author = author.info[1]
  authorAffiliation = getAttributes(doc, "//meta[@name='citation_author_institution']","content")
  correspondingAuthor = author.info[2]
  correspondingAuthorEmail = author.info[3]
  publicationDate = getAttributes(doc, "//meta[@name='prism.publicationDate']","content")
  abstract = getdata(doc, "//div[@class='AbstractSection']")
  keywords = getdata(doc,"//li[@class='c-keywords__item']")
  fullText = getdata(doc, "//div[@id='Background']")
  #Append NA when any of the above fields are empty or not available
  if(is.null(title))
    title="NA"
  if(is.null(author)|author=="NULL")
    author="NA"
  if(is.null(authorAffiliation)|authorAffiliation=="NULL")
    authorAffiliation="NA"
  if(is.null(correspondingAuthor)|correspondingAuthor=="NULL")
    correspondingAuthor="NA"
  if(is.null(correspondingAuthorEmail)|correspondingAuthorEmail=="NULL")
    correspondingAuthorEmail="NA"
  if(is.null(publicationDate)|publicationDate=="NULL")
    publicationDate="NA"
  if(is.null(abstract)|abstract=="NULL")
    abstract="NA"
  if(is.null(keywords)|keywords=="NULL")
    keywords="NA"
  if(is.null(fullText)|fullText=="NULL")
    fullText="NA"
  if(fullText=="NULL")
    fullText="NA"

  #temp=c(DOI, title, author, authorAffiliation, correspondingAuthor,
  #       correspondingAuthorEmail, publicationDate, abstract, keywords, fullText)
  
  #temp=sub("NULL", NA, temp)
  #extracted.data.row = data.frame(temp)
  extracted.data.row = data.frame(DOI, title, author, authorAffiliation, correspondingAuthor,
                              correspondingAuthorEmail, publicationDate, abstract, keywords, fullText)
  return(extracted.data.row)
}


#FUNCTION NAME: getdata
#FUNCTION:extract xmlValue from specified XML tag/node
#INPUT:[parsedHtml, pattern]
#OUTPUT:[string of xmlValue embedded in the tag/node]
getdata = function(parsedHtml, pattern){
  resultList = xpathSApply(parsedHtml, pattern, xmlValue)
  if(length(resultList) == 0) return("NULL")
  result = ""
  for(str in resultList){
    if(str == ""|str ==" ") next
    if(result == ""){
      result = str
    } else {
      result = paste(result, ";", str, sep="")
    }
    result = str_replace(result,"\\n","")
  }
  return(result)
}


#FUNCTION NAME: getAttributes
#FUNCTION:extract XML attribute value from specified XML tag/node and attribute name
#INPUT:[parsedHtml, pattern, attribute]
#OUTPUT:[string of attribute value]
getAttributes = function(parsedHtml, pattern, attribute){
  resultList = xpathSApply(parsedHtml, pattern, xmlGetAttr, attribute)
  if(length(resultList) == 0) return("NULL")
  result = ""
  for(str in resultList){
    if(str == ""|str ==" ") next
    if(result == ""){
      result = str
    } else {
      result = paste(result, ";", str, sep="")
    }
    result = str_replace(result,"\\n","")
  }
  return(result)
}

#FUNCTION NAME: getAuthors
#FUNCTION:extract author, corresponding author, corresponding author's email
#INPUT:[parsedHtml]
#OUTPUT:[vector(author, corresponding.author, email)]
getAuthors = function(parsedHtml){
  #data.frame : (author, affliationId, isCorresponding, email)
  author.data = data.frame(author=c(), affliationId=c(), isCorresponding=c(), email=c())
  author.list = xpathSApply(parsedHtml, "//li[@class='Author hasAffil']", xmlDoc)
  for(authorHTML in author.list){
    author =getdata(authorHTML, "//span[@class='AuthorName']")
    affliationId = getdata(authorHTML, "//a[@class='AffiliationID']")
    email = str_replace_all(getAttributes(authorHTML, "//a[@class='EmailAuthor']", "href"),"mailto:","")
    isCorresponding = !(email == "NULL")
    author.data.row = data.frame(author, affliationId, isCorresponding, email)
    author.data = rbind(author.data, author.data.row)
  }
  author.filtered.list = author.data[which(author.data$isCorresponding == FALSE),]
  author = ""
  corresponding.author.list = author.data[which(author.data$isCorresponding == TRUE),]
  corresponding.author = ""
  email = ""
  
  if(length(author.filtered.list)>0){
    for(i in 1:length(author.filtered.list[,1])){
      if(i == 1){
        seperator = ""
      } else {
        seperator = ";"
      }
      author = paste(author, seperator, author.filtered.list[i,1],sep="")
    }
  
  
    for(i in 1:length(corresponding.author.list[,1])){
      if(i == 1){
        seperator = ""
      } else {
        seperator = ";"
      }
      corresponding.author = paste(corresponding.author, seperator, corresponding.author.list[i,1],sep="")
      email = paste(email, seperator, corresponding.author.list[i,4],sep="")
    }
  }
  return(c(author, corresponding.author, email))
}

