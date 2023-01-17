pakker <- c("rvest", "dplyr", "tibble", "httr", "writexl", "tm", "tidytext", "stringr", "stopwords", 
            "ggplot2", "topicmodels", "SnowballC", "quanteda", "lubridate", "stm", "plotly")
lapply(pakker, library, character.only = TRUE)


### Document ###

## Web scraping
# OBS Tar lang tid å kjøre.
# Document har endret sideoppbygning etter at jeg samlet inn tekstmaterialet, så denne koden fungerer ikke lenger
get_doc_text <- function(doc_link) {
  doc_page <- read_html(doc_link)
  doc_text <- doc_page %>%
    html_nodes("article p") %>%
    html_text() %>%
    paste(collapse = ",")
  return(doc_text)
}  

document <- data.frame()

for(i in seq(from = 2003, to = 2021, by = 1)) {
  fortsettloop <- TRUE
  j <- 0
  while(fortsettloop){
    Sys.sleep(2:10) # Venter 2-10 sek mellom hver side for å ikke overbelaste serveren
    j <- j+1
    if(j == 1){
      link2 <- paste0("https://www.document.no/", i)
    } else{
      link2 <- paste0("https://www.document.no/", i,"/page/", j, "/")
    }
    fortsettloop <- httr::GET(link2)$status_code == "200"
    if(!fortsettloop ){
      next
    }
    
    page2 <- read_html(link2)
    
    doc_title <- page2 %>% # Henter overskriftene
      html_nodes("h2 > a") %>% 
      html_text()
    
    doc_links <- page2 %>% # Henter linkene til hver atikkel
      html_nodes("h2 > a") %>%
      html_attr("href") %>%
      paste(., sep = "")
    
    doc_date <- page2 %>% # Henter publiseringsdato
      html_nodes("font a") %>%
      html_text()
    
    
    doc_text <- sapply(doc_links, FUN = get_doc_text, USE.NAMES = FALSE)
    
    document <- rbind(document, data.frame(Tittel=doc_title, Publiseringsdato=doc_date, Tekst=doc_text, stringsAsFactors = FALSE))
    
    print(paste(link2, fortsettloop))
  }
}

View(document)

# Lagrer
# write.csv(document, "documentcsv.csv", row.names = FALSE)
# document <- read.csv(file = "documentcsv.csv", colClasses = c("character", "character", "character"))


### Rights ###

## Web scraping
# OBS Tar lang tid å kjøre
get_rights_text <- function(rights_link) { # Henter artikkeltekstene
  rights_page <- read_html(rights_link)
  rights_text <- rights_page %>% 
    html_nodes(".article-body > p , .article-body p+ p , .MsoNormal") %>%
    html_text() %>%
    paste(collapse = ",")
  return(rights_text)
}

rights <- data.frame() 

for(i in seq(from = 2003, to = 2021, by = 1)) {
  fortsettloop2 <- TRUE
  j <- 0
  while(fortsettloop2){
    Sys.sleep(2:10) # Venter 2-10 sek mellom hver side for å ikke overbelaste serveren
    j <- j+1
    if(j == 1){
      link3 <- paste0("https://www.rights.no/", i)
    } else{
      link3 <- paste0("https://www.rights.no/", i,"/page/", j, "/")
    }
    fortsettloop2 <- httr::GET(link3)$status_code == "200"
    if(!fortsettloop2 ){
      next
    }
    
    page4 <- read_html(link3)
    
    rights_title <- page4 %>% # Henter overskrifter
      html_nodes(".title") %>%
      html_text()
    
    rights_links <- page4 %>% # Henter linker
      html_nodes("a.link-wrapper") %>%
      html_attr("href") %>%
      paste(., sep = "")
    
    rights_date <- page4 %>% # Henter datoer
      html_nodes(".published") %>%
      html_text()
    
    rights_text <- sapply(rights_links, FUN = get_rights_text, USE.NAMES = FALSE)
    
    rights <- rbind(rights, data.frame(Tittel=rights_title, Publiseringsdato=rights_date, Tekst=rights_text, stringsAsFactors = FALSE))
    
    print(paste(link3, fortsettloop2))
  }
}

View(rights)

# Lagrer
# write.csv(rights, "rightscvs.csv", row.names = FALSE)
# rights <- read.csv("rightscvs.csv", colClasses = c("character", "character", "character"))


# Lager histogrammer over publiserte artikler, totalt og kun muslim/islam
document2 <- document[grepl("muslim|muslimer|muslimene|muslimske|islam|islamistiske|islamske|islamistisk", 
                            document$Tekst, ignore.case = TRUE), ]
rights2 <- rights[grepl("muslim|muslimer|muslimene|muslimske|islam|islamistiske|islamske|islamistisk", 
                        rights$Tekst, ignore.case = TRUE), ]

document2 <- document2 %>%
  add_column(Kun_muslim = "Kun muslim/islam")
document <- document %>%
  add_column(Kun_muslim = "Totalt")

rights2 <- rights2 %>%
  add_column(Kun_muslim = "Kun muslim/islam")
rights <- rights %>%
  add_column(Kun_muslim = "Totalt")

rights_alle <- rbind(rights, rights2)
document_alle <- rbind(document, document2)

# Labels
labels <- seq(from = 12055, to = 18625, by = 730)
labels <- c("12055" = "2003", "12785" = "2005", "13515" = "2007", "14245" = "2009",
            "14975" = "2011", "15705" = "2013", "16435" = "2015", "17165" = "2017",
            "17895" = "2019", "18625" = "2021")

doc_hist <- document_alle %>%
  ggplot(aes(x = Publiseringsdato, fill = Kun_muslim)) +
  geom_histogram(color="#e9ecef", alpha=0.6, binwidth=92, position = "identity")  +
  scale_x_continuous("Publiseringsdato", breaks = seq(from = 12055, to = 18625, by = 730), labels = labels) +
  scale_fill_manual(values=c("#404080", "#00BFC4")) +
  labs(title = "Antall artikler fra Document", y = "Antall", fill = "Artikler")
print(doc_hist)

rights_hist <- rights_alle %>%
  ggplot(aes(x = Publiseringsdato, fill = Kun_muslim)) +
  geom_histogram(color="#e9ecef", alpha=0.6, binwidth=92, position = "identity")  +
  scale_x_continuous("Publiseringsdato", breaks = seq(from = 12055, to = 18625, by = 730), labels = labels) +
  scale_fill_manual(values=c("#404080", "#00BFC4")) +
  labs(title = "Antall artikler fra Rights", y = "Antall", fill = "Artikler")
print(rights_hist)


