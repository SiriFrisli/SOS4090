pakker <- c("rvest", "dplyr", "tibble", "httr", "writexl", "tm", "tidytext", "stringr",
            "stopwords", "ggplot2", "topicmodels", "SnowballC", "quanteda", "lubridate",
            "stm", "plotly", "geometry", "stminsights", "Rtsne", "rsvd", "forcats", "purrr",
            "factoextra", "RColorBrewer", "wordcloud", "corrr", "labelled", "tidyverse",
            "tidystm", "stminsights", "GGally", "ggraph", "igraph", "gridExtra")
lapply(pakker, library, character.only = TRUE)
set.seed(1234)

# Laster inn og fikser korpuset til Rights
rights <- read.csv("rightscvs.csv", colClasses = c("character", "character", "character"))
rights$Publiseringsdato <- dmy(rights$Publiseringsdato)
rights$Publiseringsdato <- as.numeric(rights$Publiseringsdato)

# Innimellom står navnene til Hege og Rita sammen med HRS i starten av artiklene på rights, som forfatternavn, vil fjerne disse
rights <- rights %>%
  mutate(start = tolower(str_sub(Tekst, 1, 30)),
         hege = str_detect(start, pattern = "hege" ),
         storhaug = str_detect(start, pattern = "storhaug" ),
         rita = str_detect(start, pattern = "rita"),
         karlsen = str_detect(start, pattern = "karlsen")) %>%
  mutate( fjernstorhaug = (storhaug & hege) | (rita & karlsen)) %>%
  mutate( Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "Storhaug"), Tekst),
          Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "Hege"), Tekst),
          Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "storhaug"), Tekst),
          Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "hege"), Tekst),
          Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "rita"), Tekst),
          Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "Rita"), Tekst),
          Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "karlsen"), Tekst),
          Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "Karlsen"), Tekst))

rights <- subset(rights, select = -c(start, hege, storhaug, rita, karlsen, fjernstorhaug))

# Laster inn og fikser korpuset til Document
document <- read.csv(file = "documentcsv.csv", colClasses = c("character", "character",
                                                              "character"))
document$Publiseringsdato <- mdy(document$Publiseringsdato)
document$Publiseringsdato <- as.numeric(document$Publiseringsdato)

# Lager ekstra kolonne på hver dataframe med info om hvilken nettside artiklene er fra
document <- document %>%
  add_column(Nettside = "Document")

rights <- rights %>%
  add_column(Nettside = "Rights")

# Kombinerer
begge <- rbind(document, rights)
begge <- begge[order(begge$Publiseringsdato), ] # Sorterer artiklene etter publiseringsdato

# Velger å kun inkludere ord som refererer direkte til muslimer eller islam
begge <- begge[grepl("muslim|muslimer|muslimene|muslimske|islam|islamistiske|islamske|islamistisk",
              begge$Tekst, ignore.case = TRUE), ]
# write.csv(begge, "begge.csv", row.names = FALSE)
# begge <- read.csv("begge.csv")

# Liste med ekstra stoppord
egne_stoppord <- c("må", "måtte", "sier", "få", "mer", "får", "ta", "går", "gikk", "dag",
                   "altså", "år", "nok", "hele", "mens", "gjør", "gjøre", "gjort", "fått", "se", "ser", "sett",
                   "står", "fikk", "tar", "tatt", "sa", "gå", "vet", "the", "new", "way", "all", "last", "get",
                   "why", "who", "you", "me", "us", "your", "mine", "will", "this", "that", "u", "mest", "one",
                   "two", "have", "are", "or", "without", "with", "his", "to", "of", "and", "in", "a", "is", "on",
                   "it", "was", "as", "af", "och", "sig", "att", "he", "not", "be", "they", "an", "är", "till",
                   "inte", "we", "but", "their", "jo", "för", "which", "would", "its", "if", "there", "scanpix",
                  "what", "also", "said", "has", "foto", "ntb", "when", "our", "than", "other", "she", "can",
                  "them", "do", "by", "been", "had", "out", "how", "where", "were", "http", "any", "those", "end",
                  "blev", "high", "blevet", "comes", "come", "vist", "del", "self", "her", "on", "once", "second",
                  "if", "until", "into", "from", "does", "its", "his", "theirs", "them", "jag", "från", "när",
                  "ska", "hon", "alla", "där", "vad", "andra", "vara", "hur", "nu", "finns", "också", "nu",
                  "något", "någon", "hvad", "nu", "havde", "noget", "mig", "blive", "nogle", "os", "vores", "här",
                  "än", "över", "allt", "bara", "these", "than", "him", "now", "op", "ind", "uden", "gøre", "gør",
                  "blandt", "detta", "sedan", "efter", "deras", "mycke", "mycket", "efter", "ëven", "säger",
                  "utan", "sina", "frå", "bliver", "siger", "fik", "nogen", "sagde", "sige", "gærne", "mer",
                  "brug", "brugt", "about", "only", "even", "mer", "brug", "giv", "giver", "seem", "like", "most",
                  "andre", "månge", "även", "måst", "måsta", "någre")
 # En del engelske og noen svenske og danske, sammen med litt ekstra norske

begge$Tekst <- str_replace_all(begge$Tekst, "[^[:alnum:]]", " ")
begge$Tekst <- str_replace_all(begge$Tekst, "[0-9]", " ")
begge$Tekst <- gsub('\\b\\w{1,2}\\b','', begge$Tekst)
corp_begge <- corpus(begge, text_field = "Tekst")

tokens_begge <- tokens(corp_begge,
 remove_punct = TRUE,
 remove_symbols = TRUE,
 remove_numbers = TRUE,
 remove_url = TRUE) %>%
 tokens_remove(pattern = c(stopwords(source = "snowball", language = "no"), egne_stoppord)) %>%
 tokens_remove(pattern = c(stopwords(source = "snowball", language = "en")))

tokens_begge <- tokens_compound(tokens_begge, phrase(c("hege storhaug", "joe biden", "sylvi
                   listhaug", "human rights service", "paul collier", "abid raja", "erna solberg", "naser khader",
                   "per willy amundsen", "bruce bawer", "flemming rose", "jonas gahr støre", "donald trump", "islam
                   net", "saudi arabia", "al qaida", "stefan löfven", "islamsk råd", "george w bush", "donald
                   trump", "osama bin laden", "christian tybring gjedde", "hadia tajik", "einar gelius", "audun
                   lysbakken", "theo van gogh", "geert wilders", "boris johnson", "mullah krekar", "new york",
                   "saddam hussein", "charlie hebdo", "lars vilks", "kurt westergaard", "yom kippur", "arfan
                   bhatti", "anders behring breivik", "jyllands posten", case_insensitive = TRUE)))
tokens_begge <- tokens_wordstem(tokens_begge, language = "no")
dfm_begge <- dfm(tokens_begge, tolower = TRUE)
begge_stm <- convert(dfm_begge, to = "stm")

out_begge <- prepDocuments(begge_stm$documents,
 begge_stm$vocab,
 begge_stm$meta,
 lower.thresh = 20)
# saveRDS(out_begge, "out_begge.rds")
# out_begge <- readRDS("out_begge.rds")

docs_begge <- out_begge$documents
vocab_begge <- out_begge$vocab
meta_begge <- out_begge$meta

out_begge$meta$Nettside <- as.factor(out_begge$meta$Nettside)
 # Nettsidene som faktorvariabel med to nivåer
table(begge$Nettside)

# Finne hvor mange emner som er best
stmfinn_begge <- stm(documents = docs_begge,
 vocab = vocab_begge,
 K = 0,
 prevalence = ~Nettside + s(Publiseringsdato),
 content = ~Nettside,
 max.em.its = 1000,
 data = meta_begge,
 init.type = "Spectral",
 set.seed(1234),
 verbose = TRUE)
# 48 emner, oppleves som for mange

tm10_begge <- stm(documents = docs_begge,
 vocab = vocab_begge,
 K = 10,
 prevalence = ~Nettside + s(Publiseringsdato),
 content = ~Nettside,
 max.em.its = 1000,
 data = meta_begge,
 init.type = "Spectral",
 set.seed(1234),
 verbose = TRUE)
# saveRDS(stm10_begge, "stm10_begge.rds")

stm15_begge <- stm(documents = docs_begge,
 vocab = vocab_begge,
 K = 15,
 prevalence = ~Nettside + s(Publiseringsdato),
 content = ~Nettside,
 max.em.its = 1000,
 data = meta_begge,
 init.type = "Spectral",
 set.seed(1234),
 verbose = TRUE)
# saveRDS(stm15_begge, "stm15_begge.rds")

stm20_begge <- stm(documents = docs_begge,
 vocab = vocab_begge,
 K = 20,
 prevalence = ~Nettside + s(Publiseringsdato),
 content = ~Nettside,
 max.em.its = 1000,
 data = meta_begge,
 init.type = "Spectral",
 set.seed(1234),
 verbose = TRUE)
# saveRDS(stm20_begge, "stm20_begge.rds")

stm25_begge <- stm(documents = docs_begge,
 vocab = vocab_begge,
 K = 25,
 prevalence = ~Nettside + s(Publiseringsdato),
 content = ~Nettside,
 max.em.its = 1000,
 data = meta_begge,
 init.type = "Spectral",
 set.seed(1234),
 verbose = TRUE)
# saveRDS(stm25_begge, "stm25_begge.rds")

stm30_begge <- stm(documents = docs_begge,
 vocab = vocab_begge,
 K = 30,
 prevalence = ~Nettside + s(Publiseringsdato),
 content = ~Nettside,
 max.em.its = 1000,
 data = meta_begge,
 init.type = "Spectral",
 set.seed(1234),
 verbose = TRUE)
# saveRDS(stm30_begge, "stm30_begge.rds")

# Lagrer emnene med ord
emner20 <- sageLabels(stm20_begge, n = 20) # Endre n etter hvor mange ord per emne vil ha med
sink("emner20.txt", append = FALSE, split = TRUE)
print(emner20)
sink()