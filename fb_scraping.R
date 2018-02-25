rm(list = ls())

library("rvest")
library("RSelenium")
library("wdman")
library(stringr)

results <- list()

human_wait = function(t = 2, tt = 4){
  Sys.sleep(sample(seq(t, tt, by=0.001), 1)) 
}

# disable pictures in browser
prefs = list("profile.managed_default_content_settings.images" = 2L)
cprof <- list(chromeOptions = list(prefs = prefs))

rd <- rsDriver(extraCapabilities = cprof)
Sys.sleep(1)
remdr = rd[["client"]]  

url1 = "https://m.facebook.com"
url2 = "https://m.facebook.com/groups/170325662996340"

remdr$navigate(url1)
human_wait()

# Enter Username
remdr$findElement(using = 'xpath', value = '//*[@id="m_login_email"]')$sendKeysToElement(list("fi.sihhatik@gmail.com"))

# Enter Username
remdr$findElement(using = 'xpath', value = '//*[@id="m_login_password"]')$sendKeysToElement(list("tinder"))

# Login Button clicken
remdr$findElement(using = 'xpath', value = '//*[@id="u_0_5"]')$clickElement()

# Go to group
remdr$navigate(url2)

# Scrolling
for(q in 1:3){
print(q)
webElem <- remdr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))
human_wait()
}

# Get all posts 
html <- remdr$getPageSource()[[1]] %>% read_html() 
nodes <- html %>% html_nodes(xpath = "//div[@class='story_body_container']") 

# Get all links: 
all_links = nodes %>% 
  html_nodes("div") %>% 
  html_nodes("a") %>% 
  html_attr("href")

postlinks = all_links [(stringr::str_detect(all_links,"groups")) & (stringr::str_detect(all_links,"permalink")) & (stringr::str_detect(all_links,"top_level"))]
postlinks_sub = postlinks %>% str_sub(start = 1,end = unlist(str_locate(.,"&r")[,1]))
postlinks_list = postlinks_sub %>% unique()


#test
#remdr$navigate(paste0("https://m.facebook.com",postlinks_list[40]))


postcontent <- remdr$getPageSource()[[1]] %>% read_html() 

postcontenttext <- 
  postcontent %>% 
  html_nodes('p')%>% 
  html_text()


##########
#reactions
reacs <- remdr$findElements(using = 'xpath', "//div[@class='story_body_container']")
reacs[[1]]$clickElement()
reactions <- remdr$getPageSource()[[1]] %>% read_html() 
reactionnodes <- 
  reactions %>% 
  html_nodes(xpath='//*[@id="root"]/div/div/div/div[2]') %>% 
  html_nodes('div') %>% 
  html_nodes('a') %>% 
  html_nodes('i') %>% 
  html_attr('aria-label')
remdr$goBack()

## comments
## hier komme ich auch irgendwie noch an unique link ran oder so

comments <- remdr$getPageSource()[[1]] %>% read_html() 
# commentnames <- 
#   comments %>% 
#   html_nodes(xpath='//*[@id="ufi_2016664461695775"]/div/div[4]') %>% 
#   html_nodes('div') %>% 
#   html_nodes('a') %>% 
#   html_nodes('i') %>% 
#   html_attr('aria-label')


# PREVIOUS COMMENTS ETC


commentspairs <- # evtl. noch html_nodes('a)
  comments %>%
  html_nodes(xpath='//*[@id="ufi_2016664461695775"]/div/div[4]') %>%
  html_nodes('div') %>%
  html_nodes('div') %>%
  html_nodes('div') %>%
  html_nodes('div') %>%
  html_nodes('div') %>%
  html_text()

commentauthor <- commentspairs %>% .[seq(1,length(.)-1,by = 2)]
commenttext <- commentspairs %>% .[seq(2,length(.),by = 2)]

remdr$goBack()

# Close Window
remdr$close() 
rd$server$stop()

