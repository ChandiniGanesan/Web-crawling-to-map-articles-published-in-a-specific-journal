# crawling the journals
#Loading All required libraries
library(bitops)
library(RCurl)
library(XML)
library(stringr)
library(httr)

#Loading User-Defined library
source(paste(getwd(),"/ourfunctions.R",sep=""))

#Getting the group Id for project title
name<-c('karumanchi','ganesan','katta')
generateGroupId(name)

##PHASE-I CRAWLING

#Journal main urls
journal.url="https://hereditasjournal.biomedcentral.com"
journal.url.copy="https://hereditasjournal.biomedcentral.com"

#Reading the journal Main Page and saving it as home.html
journal.mainpage=readLines(journal.url)
options(warn=-1)
dir.create("Websites")
write(journal.mainpage,file="Websites/home.html")

#Obtaining url of articles tab from the home.html
grep("(<a class=.c-navbar__link.+ href).+(>Articles<\\/a>)",journal.mainpage)
articletab.url=paste(journal.url,substr(journal.mainpage[grep("(<a class=.c-navbar__link.+ href).+(>Articles<\\/a>)",journal.mainpage)],82,89),sep="/")
articletab.url

#Reading the Articles Tab Page
articletab.list=readLines(articletab.url)
write(articletab.list,file="Websites/articlestab.html")

#Get total number of pages in the Journal for extraction
page.total = str_extract_all(articletab.list[grep('<p class=\\"u-text-sm u-reset-margin\\">Page \\d+ of \\d+<\\/p>',articletab.list)][1],"\\d+")[[1]]
page.min = page.total[1]
page.max = page.total[2]
page.min
page.max

#Obtaining generic page url to get each page 
articletab.page.url = paste(journal.url, str_extract(articletab.list[tail(grep('<li class="c-pagination__item">',articletab.list),n=1)+2][1],"articles\\?.+page\\="),sep="/")
articletab.page.url = xpathApply(htmlParse(articletab.page.url, asText=TRUE),"//body//text()", xmlValue)[[1]]
articletab.page.url

#Traverse each page of the journal and retrieve all the DOI and Urls of the articles
#generate article url list

articletab.page.url.list = ""
article.data = data.frame(DOI=c(),url=c())
for(i in page.min:page.max){
  percent = toString(as.integer((i/as.integer(page.max))*100))
  cat("\r",paste("[APP]: loading page number: ",i," [", percent, "%]"))
  full.url = paste(articletab.page.url,i,sep="")
  articleUrl_and_doi =getArticleDOIURL(full.url)#function call:getArticleDOIURL()
  articleUrl_and_doi$url = paste(journal.url.copy, articleUrl_and_doi$url, sep="")#add site url, to fulfill the url of an article
  article.data = rbind(article.data, articleUrl_and_doi)#rown bind, store all article DOI, urls
  articletab.page.url.list = c(articletab.page.url.list, full.url)
}
articletab.page.url.list = articletab.page.url.list[-1]
write(articletab.page.url.list, "articletab.page.url.list.txt")
print("[I/O]: FILE: articletab.page.url.list.txt created.")
write.csv(article.data, "article.DOI.URL.list.csv")
print("[I/O]: FILE: article.DOI.URL.list.csv created.")

##PHASE-II PARSING 

#Analyse each article and extract the required(ten) fields
extracted.data = data.frame(DOI=c(),Title=c(),Author=c(), "Author Affiliation"=c(), "Corresponding Author"=c(), "Corresponding_Author_email"=c(), 
                            "Publication Date"=c(), Abstract=c(), Keywords=c(), "Full Text"=c())
total.number = as.integer(length(article.data[,1]))
for(i in 1:total.number){
  #print(paste("Use this--->",article.data[i,2]))
  extracted.data = rbind(extracted.data, parseArticle(article.data[i,1], article.data[i,2]))#function call: parseArticle
  cat("\r", paste("[I/O]: FILE:", toString(i), article.data[i,1], ".html created. [",toString(as.integer(i/total.number*100)), "%]"))
}


#Write the final result to Hereditas.txt
options(warn=-1)
dir.create("output")
write.table(extracted.data,"output/Hereditas.txt",sep="\t",row.names=FALSE,fileEncoding = "UTF-8")
print("[I/O]: FILE: output/Hereditas.txt created.")

