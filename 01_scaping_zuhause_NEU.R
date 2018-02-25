rm(list = ls())

library("rvest")
library("RSelenium")
library("wdman")
library(stringr)

results <- list()

categories <- 
  c("Lehrende:",
    "Veranstaltungsart:",
    "Orga-Einheit:",
    "Anzeige im Stundenplan:",
    "Semesterwochenstunden:",
    "Credits",
    "Standort",
    "Unterrichtssprache:",
    "Min. | Max. Teilnehmerzahl:",
    "Inhalte:",
    "Zugelassene Hilfsmittel f?r die Pr?fung:",
    "Literatur:",
    "Lernziele:",
    "Weitere Informationen zu den Pr?fungsleistungen:"  )

ms_text <- function(category = "Lehrende:",
                    tt = tt1){
  
  start <- str_locate(tt,category)[,2]+2 
  endvector <- 
    str_locate_all(string = tt,pattern = categories [!categories %in% c(category)]) %>%
    unlist()
  
  end <- endvector[endvector > start ] %>% min()
  
  
  text <- str_sub(tt,
                  start,
                  end-1)
  return(text)
}

human_wait = function(t = 2, tt = 4){
  Sys.sleep(sample(seq(t, tt, by=0.001), 1)) 
}





########

url = "https://zuhause.zeppelin-university.net/scripts/mgrqispi.dll?APPNAME=CampusNet&PRGNAME=STARTPAGE_DISPATCH&ARGUMENTS=-N000000000000001"

#ws <- remdr$getWindowSize()


 # print(paste0("PLZ: ",plz," ",Sys.time()))
  
  rd = rsDriver()
  remdr = rd[["client"]]  
  
# Reset window size to standard
#remdr$setWindowSize(ws$width, ws$height)
  
remdr$navigate(url)
human_wait()

# Enter Username
remdr$findElement(using = 'xpath', value = '//*[@id="username"]')$sendKeysToElement(list("14201770"))

# Enter Username
remdr$findElement(using = 'xpath', value = '//*[@id="password"]')$sendKeysToElement(list("Rojo0175"))

# Login Button clicken
remdr$findElement(using = 'xpath', value = '//*[@id="contentSpacer_IE"]/h2/form/fieldset/div[3]/input')$clickElement()




# All Semesters
semesters <- function(inner){
  
  links <- remdr$findElements(using = 'css selector', "a")
  linktext <- unlist(lapply(links, function(x){x$getElementText()}))
  sem_index <<- which(sapply(linktext,function(x)stringr::str_detect(x,"Veranstaltungen\n")))
  semesters <- links[sem_index]
  
  for(level1 in 1:length(semesters)){ 
    
    l1 <<- level1
    
    semlinks <- remdr$findElements(using = 'css selector', "a")
    semlinktext <- unlist(lapply(semlinks, function(x){x$getElementText()}))
    sem_index <- which(sapply(semlinktext,function(x)stringr::str_detect(x,"Veranstaltungen\n")))
    semesters <- links[sem_index]
    
    semesters[[level1]]$clickElement()
    
    inner
  }
  
}
  

av <- c("Zusatzkurse und StudentStudies","Workshop |ZU|plus","PhD Courses","ZU Courses Taught in English")


nextlevel <- function(xpath = '//*[@id="auditRegistration_list"]//*',
                      whichlevel,
                      avoid = NULL,
                      inner = NULL,
                      innereval = NULL){
  

  links <- remdr$findElements(using = 'xpath', xpath)
  links <- links[seq(2,length(links),by = 2)]
  linktext <- unlist(lapply(links, function(x){x$getElementText()}))
  
  eval(parse(text=paste0("linktext",whichlevel,"<<- linktext"))) 
  
  #types <-  which(!linktext %in% c(avoid))
  
  for(level in c(1:length(links))){
    
    eval(parse(text=paste0("l",whichlevel," <<- level"))) 
    
    links <- remdr$findElements(using = 'xpath', '//*[@id="auditRegistration_list"]//*')
    links <- links[seq(2,length(links),by = 2)]
    linktext <- unlist(lapply(links, function(x){x$getElementText()}))
    
    links[[level]]$clickElement()
    
    if(!is.null(inner)) {inner}
   
    
    
  }
  
  remdr$goBack()
  
  
}

laststep <- function(){
  
  source("02_source_functions.R")
  remdr$goBack()

}
laststep()

############# ACTUAL

# Click on Vorlesungsverzeichnis
remdr$findElement(using = 'xpath', value = '//*[@id="link000621"]/a')$clickElement()

results <- data.frame()

speSS18 <- results

spe <- dplyr::bind_rows(speFS14 %>% data.frame(sem = "FS14"),
                        speSS15 %>% data.frame(sem = "SS15"),
                        speFS15 %>% data.frame(sem = "FS15"),
                        speSS16 %>% data.frame(sem = "SS16"),
                        speFS16 %>% data.frame(sem = "FS16"),
                        speSS17 %>% data.frame(sem = "SS17"),
                        speFS17 %>% data.frame(sem = "FS17"),
                        speSS18 %>% data.frame(sem = "SS18"))

write.csv2(spe,"spe.csv")

laststep()


semesters(inner = 
            nextlevel(whichlevel = "2",
                      avoid = av,
                      inner = nextlevel(whichlevel = "3",
                                        avoid = "Zeppelin Jahr",
                                        inner = nextlevel(whichlevel = "4",
                                                          avoid = "xxx",
                                                          inner = laststep())))

)


# level studiengang
links3 <- remdr$findElements(using = 'xpath', '//*[@id="auditRegistration_list"]//*')
links3 <- links3[seq(2,length(links3),by = 2)]
linktext3 <- unlist(lapply(links3, function(x){x$getElementText()}))


without_old_zeppelin <- 
  which(! str_detect(linktext3,"Zeppelin Jahr"))


for(level3 in c(1:length(links3))[without_old_zeppelin]){
  links3 <- remdr$findElements(using = 'xpath', '//*[@id="auditRegistration_list"]//*')
  links3 <- links3[seq(2,length(links3),by = 2)]
  linktext3 <- unlist(lapply(links3, function(x){x$getElementText()}))
  
  links3[[level3]]$clickElement()

# level studienphase
links4 <- remdr$findElements(using = 'xpath', '//*[@id="auditRegistration_list"]//*')
links4 <- links4[seq(2,length(links4),by = 2)]
linktext4 <- unlist(lapply(links4, function(x){x$getElementText()}))

humboldt <- which(!linktext4 %in% c("Humboldtjahr","Bachelor-Phase"))


for(level4 in c(1:length(links4))[humboldt]){ 
  links4 <- remdr$findElements(using = 'xpath', '//*[@id="auditRegistration_list"]//*')
  links4 <- links4[seq(2,length(links4),by = 2)]
  linktext4 <- unlist(lapply(links4, function(x){x$getElementText()}))
  links4[[level4]]$clickElement()
  
  
  source("02_source_functions.R")
  remdr$goBack()
  
####

} # level5 over
remdr$goBack()
} #level4 over
remdr$goBack()
} #level 3 over
remdr$goBack()
} #level 2 over
remdr$goBack()




results$X1[is.na(results$X1)] <- results$lehrende_all[is.na(results$X1)] 

write.csv2(results,"zuhause_abfall16.csv")
write.csv2(results,"zuhause_spring15bisfall16.csv")
write.csv2(results,"zuhause_fall14.csv")




# Close Window
remdr$close() 
rd$server$stop()


