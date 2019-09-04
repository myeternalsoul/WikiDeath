library(httr)
library(XML)
library(stringr)
library(rvest)
library(lubridate)
library(tidyverse)

df<-data.frame(Name=integer(),Age=integer(),comment=character(),Nation=character(),Month=character(),Year=integer())

#function to compare columns in the loop 
#  and remove the name from the comment section,
#  (which would happen if the parsing wasn't right)
compareCols<-function(x,y) {
  return(gsub(x,"",y))
}

#function to clean up various inconsistencies
#  and extract the Nation from the comment section
extractNation<-function(x) {
  prefixes<-c("New","Sri","Hong","Costa","North","South","East","West","Cook","Saint","Cape","Los","São","El","San")
  x1<-str_extract(x,"^[A-Z]\\S+")
  x2<-str_extract(x,"^[A-Z]\\S+ \\S+")
  retval<-ifelse(x1 %in% prefixes,x2,x1)
  retval<-gsub("–","-",retval,perl=T)
  retval<-gsub("-born","",retval,perl=T)
  retval<-gsub(",$","",retval,perl=T)
  retval<-gsub("\\'s$","",retval,perl=T)
  return(retval)
}


thisYear=year(today(tz="US/Eastern"))
thisMonth=month(today(tz="US/Eastern"))
## Loop thru all monthly Wiki pages for the "death list"
for(i in 2005:thisYear) {
  for(j in month.name){
    if(i==thisYear & j==month.name[thisMonth]) break
    print(paste0("Processing ",j," ",i))
    url<-paste0("https://en.wikipedia.org/wiki/Deaths_in_",j,"_",i)
    attWebPage<-GET(url)
    if (status_code(attWebPage) >= 300) {
      print("This page doesn't exist (yet)")
      break
    }
    xp<-'//*[(@id = "mw-content-text")]//li'
    
    #    if (status_code(attWebPage) >= 300) break
    
    iDeaths<-attWebPage %>%
      read_html() %>%
      html_nodes(xpath=xp) %>%
      html_text()
    #    write.csv(iDeaths,paste0("data/Deaths_in_",j,"_",i))
    
    
    #Remove dead records (so to speak)
    invalidWords<-c(month.abb,"v","e","t")
    aa<-subset(iDeaths,!str_detect(iDeaths,"Name,"))
    # View(as.data.frame(aa))
    aa<-subset(aa,!str_detect(aa,"^Lists of deaths by year"))
    aa<-subset(aa,!str_detect(aa,"^\\^"))
    aa<-subset(aa,!str_detect(aa,"^[1-9]"))
    aa<-subset(aa,!str_detect(aa,"^Deaths in"))
    aa<-subset(aa,!str_detect(aa,"^Notable"))
    aa<-subset(aa,!str_detect(aa,"^[Oo]bituaries"))
    aa<-subset(aa,!str_detect(aa,"[Oo]bituaries$"))
    aa<-subset(aa,!str_detect(aa,"U+"))
    aa<-subset(aa,!(aa %in% invalidWords))

    #fix a couple of other glitches

    aa<-gsub(", Jr.,"," Jr.,",aa)
    aa<-gsub(", Sr.,"," Sr.,",aa)
    
    
    #Strip out the pertinent information, bind to master dataframe
    dNames<-sapply(strsplit( aa, ","), "[", 1)
    dNames<-gsub(".\\([a-z]{2}\\)$","",dNames)
    dAge<-as.integer(str_extract(aa," \\d{1,3}(?=, )"))
    dComment<-gsub("[\\S ]+, \\d{1,3}, ","",aa,perl=T)
    dComment<-gsub("[\\S ]+, c. \\d{1,3}, ","",dComment,perl=T)
    dComment<-gsub("\\[\\d+\\]","",dComment)
    dComment<-as.character(mapply(compareCols,dNames,dComment))
    dComment<-str_trim(dComment)
    dComment<-gsub(", \\d+, (?=[A-Z])","",dComment,perl=T)
    
    dNation<-extractNation(dComment)
    #    dNation<-str_extract(dComment,"^[A-Z]\\S+")
    #    dNation[dNation=="New"]<-str_extract(dComment,"^[A-Z]\\S+ \\S+")
    
    currData<-data.frame(Name=dNames,Age=dAge,Comment=dComment,Nation=dNation,Month=j,Year=i)
    
    
    df<-rbind(df,currData)
    
    remove(currData)
    
    print(paste0("Done with ",j," ",i))
    
    # s<-floor(runif(1, min=0, max=2))
    # print(paste0("Sleeping...",s))
    # Sys.sleep(s)
  }
  print("Done")
}

df$Name<-as.character(df$Name)
df$Comment<-as.character(df$Comment)
df <- df %>%
  filter(Name!="List of deaths By year") %>%
  filter(Name!="Lists of deaths by year")



write.csv(df,"DeathData.csv")
save(df,file = "DeathData.rda")


# 
# df$dDate<-as.Date(df$chDate,"%B %d, %Y")
# df$gMonth<-month(df$dDate)
# 
# ###########################################
# library(ggplot2)
# gg<-ggplot(df[df$venue=="BB&T Ballpark",],aes(x=temp,y=attend,color=month.name[gMonth]))
# gg<-gg+geom_point() 
# gg<-gg+scale_color_discrete(name="Month", labels=month.name)
# gg

