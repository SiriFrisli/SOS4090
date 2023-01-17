pakker <- c("rvest", "dplyr", "tibble", "httr", "writexl", "tm", "tidytext", "stringr", 
            "stopwords", "ggplot2", "topicmodels", "SnowballC", "quanteda", "lubridate", 
            "stm", "plotly", "tidystm", "geometry", "Rtsne", "rsvd", "forcats", "purrr", 
            "factoextra", "RColorBrewer", "wordcloud", "corrr", "labelled", "tidyverse", 
            "stminsights", "GGally", "text2vec", "ggraph", "igraph", "gridExtra")
lapply(pakker, library, character.only = TRUE)
set.seed(1234)

citation("SnowballC")

# Laster inn og fikser korpuset til Rights
rights <- read.csv("rightscvs.csv", colClasses = c("character", "character", "character"))
rights$Publiseringsdato <- dmy(rights$Publiseringsdato)
rights$Publiseringsdato <- as.numeric(rights$Publiseringsdato) 

# Laster inn og fikser korpuset til Document
document <- read.csv(file = "documentcsv.csv", sep=",", colClasses = c("character", "character", "character"))
document$Publiseringsdato <- mdy(document$Publiseringsdato)
document$Publiseringsdato <- as.numeric(document$Publiseringsdato) 

  #Innimellom står navnene til Hege og Rita sammen med HRS i starten av artiklene på rights, som forfatternavn, vil fjerne disse
rights <- rights %>% 
  mutate(start = tolower(str_sub(Tekst, 1, 30)),
         hege = str_detect(start, pattern = "hege" ),
         storhaug = str_detect(start, pattern = "storhaug" ),
         rita = str_detect(start, pattern = "rita"),
         karlsen = str_detect(start, pattern = "karlsen")) %>%
  mutate( fjernstorhaug = (storhaug & hege) | (rita & karlsen))  %>% 
  mutate( Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "Storhaug"), Tekst),
          Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "Hege"), Tekst),
          Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "storhaug"), Tekst),
          Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "hege"), Tekst),
          Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "rita"), Tekst),
          Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "Rita"), Tekst),
          Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "karlsen"), Tekst),
          Tekst = ifelse(fjernstorhaug, str_remove(Tekst, "Karlsen"), Tekst)
  )
rights <- subset(rights, select = -c(start, hege, storhaug, rita, karlsen, fjernstorhaug))

# Lager ekstra kolonne på hver dataframe med info om hvilken nettside artiklene er fra
document <- document %>%
  add_column(Nettside = "Document")

rights <- rights %>%
  add_column(Nettside = "Rights")

# Kombinerer 
begge <- rbind(document, rights)
begge <- begge[order(begge$Publiseringsdato), ] # Sorterer artiklene etter publiseringsdato
# write.csv(begge, "begge_ny.csv", row.names = FALSE)
# begge <- read.csv("begge_ny.csv")

# Liste med ekstra stoppord
egne_stoppord <- c("må", "måtte", "sier", "få", "mer", "får", "ta", "går", "gikk", "dag", "altså", 
                   "år", "nok", "hele", "mens", "gjør", "gjøre", "gjort", "fått", 
                   "se", "ser", "sett", "står", "fikk", "tar", "tatt", "sa", "gå", 
                   "vet", "the", "new", "way", "all", "last", "get", "why", "who", 
                   "you", "me", "us", "your", "mine", "will", "this", "that", "u", "mest", "one", 
                   "two", "have", "are", "or", "without", "with", "his", "to", "of",
                   "and", "in", "a", "is", "on", "it", "was", "as", "af", "och", "sig", "att", "he", 
                   "not", "be", "they", "an", "är", "till", "inte",  "we", "but", "their", "jo", "för", "which",
                   "would", "its", "if", "there", "scanpix", "what", "also", "said", "has", "foto", 
                   "ntb", "when", "our", "than", "other", "she", "can", "them", "do", "by", "been", "had", "out",
                   "how", "where", "were", "http", "any", "those", "end", "blev", "high", "blevet", "comes", 
                   "come", "vist", "del", "self", "her", "on", "once", "second", "if", "until", "into",
                   "from", "does", "its", "his", "theirs", "them", "jag", "från", "när", "ska", "hon", "alla",
                   "där", "vad", "andra", "vara", "hur", "nu", "finns", "också", "nu", "något", "någon", "hvad",
                   "nu", "havde", "noget", "mig", "blive", "nogle", "os", "vores")
    # En del engelske og noen svenske, får ikke fjernet stoppord fra flere språk i textprocessor


# Klargjør dataen for å kunne lage emnemodeller
begge_prosessert <- textProcessor(documents = begge$Tittel, 
                                  metadata = begge,
                                  lowercase = TRUE,
                                  removestopwords = TRUE,
                                  removepunctuation = TRUE,
                                  stem = FALSE, # uten stemming
                                  language = "norwegian",
                                  customstopwords = egne_stoppord,
                                  onlycharacter = TRUE)

  # Ser over hvor mange ord som blir fjernet om jeg utelater de minst brukte ordene
plotRemoved(begge_prosessert$documents, lower.thresh = seq(from = 1, to = 30, by = 1))

out_begge <- prepDocuments(begge_prosessert$documents, 
                            begge_prosessert$vocab, 
                            begge_prosessert$meta,
                            lower.thresh = 5)
# saveRDS(out_begge, "out_begge_ngram.rds")
# out_begge <- readRDS("out_begge_ngram.rds")
docs_begge <- out_begge$documents
vocab_begge <- out_begge$vocab
meta_begge <- out_begge$meta

out_begge$meta$Nettside <- as.factor(out_begge$meta$Nettside) 
  # Nettsidene som faktorvariabel med to nivåer

table(meta_begge$Nettside)
  # Se hvor mange artikler fra hver nettside

# Lete etter beste antall emner
finnK_begge <- searchK(documents = docs_begge, 
                       vocab = vocab_begge,
                       K = 0,
                       prevalence = ~Nettside + s(Publiseringsdato),
                       content = ~Nettside,
                       data = meta_begge,
                       set.seed(1234),
                       verbose = TRUE)
# 48 emner
# saveRDS(finnK_begge, "finnK_begge2.rds")

stm48_begge <- stm(documents = docs_begge,
                   vocab = vocab_begge,
                   K = 48,
                   prevalence = ~Nettside + s(Publiseringsdato),
                   content = ~Nettside,
                   max.em.its = 1000, 
                   data = meta_begge,
                   init.type = "Spectral",
                   set.seed(1234),
                   verbose = TRUE)
# saveRDS(stm48_begge, "stm48_begge_stem.rds")
  # 48 emner oppleves som for mange, bør nedjusteres

# Undersøker manuelt 15, 20, 25, 30, 40 emner
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
# saveRDS(stm15_begge, "stm15_begge_stem.rds")

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
# saveRDS(stm20_begge, "stm20_begge_ngram.rds")

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
# saveRDS(stm25_begge, "stm25_begge_stem.rds")

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
# saveRDS(stm30_begge, "stm30_begge_stem.rds")

stm40_begge <- stm(documents = docs_begge,
                   vocab = vocab_begge,
                   K = 40,
                   prevalence = ~Nettside + s(Publiseringsdato),
                   content = ~Nettside,
                   max.em.its = 1000, 
                   data = meta_begge,
                   init.type = "Spectral",
                   set.seed(1234),
                   verbose = TRUE)
# saveRDS(stm40_begge, "stm40_begge2.rds")

# Lagrer emnene og leser over manuelt
emner20 <- sageLabels(stm10_begge, n = 20)
sink("emner20_endelig10.txt", append = FALSE, split = TRUE)
print(emner20)
sink()

plot(stm20_begge)

findThoughts(stm20_begge, meta_begge$Tittel, topics = 5, n = 1)
  # Forstår ikke helt hvorfor jeg får de resultata jeg får, og finner lite info om 
  # hva denne koden egentlig gjør. Finner dokumenter assosiert med de ulike emnene på andre måter
 
theta <- make.dt(stm20_begge) # theta-verdiene er emneandelene til hvert enkelt dokument
theta[1,1:20]
glimpse(theta)

meta_begge %>%
  filter(str_detect(Tittel, "FNs menneskerettighetsråd er en skandale"))

theta %>% 
  mutate(max = max(Topic6)) %>% 
  filter(Topic6 == max)

theta5 <- theta %>%
  arrange(desc(Topic5)) %>% 
  slice(1:3)
theta5
  # Finner topp tre artikler med høyest theta verdi per emne. Gir meg radnummer, ikke radnavn

sink("tekst1.txt", append = FALSE, split = TRUE)
theta1 <- theta %>%
  arrange(desc(Topic1)) %>% 
  slice(1,148)
theta1
meta_begge[68401,]
meta_begge[30684,]
sink()

sink("tekst5.txt", append = FALSE, split = TRUE)
theta5 <- theta %>%
  arrange(desc(Topic5)) %>% 
  slice(1,)
theta5
meta_begge[,]
meta_begge[,]
sink()



theta %>% 
  mutate(mean = mean(Topic14)) %>% 
  filter(Topic14 >= mean) %>% 
  filter(row_number()==1)

theta %>%
  mutate(maks = colnames(theta[,-1])[apply(theta[,-1],1, which.max)]) %>%
  filter(Topic14 == max(theta[,15]))


# Topic proportion
plot(stm20_begge, type = "s", xlim = c(0, .4))
  #Hva er de viktigste emnene?

summary(stm20_begge)

findTopic(stm20_begge, c("muslimer", "muslim", "muslimske", n = 20, type = "frex"))
findTopic(stm20_begge, c("islam", n = 20, type = "frex"))
findTopic(stm20_begge, c("innvandrere", "innvandring", n = 20, type = "frex"))
  # Hvilke emner bruker de spesifike ordene?


stm20_gamma <- tidy(stm20_begge,
                    matrix = "gamma")

gamma_terms <- stm20_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma)) +
  geom_col(fill = "#00bfc4", color = "black", show.legend = FALSE) +
  labs(title = "Emnefordeling", y = "", x = "") +
  coord_flip()


# Ser på bruken av emnene etter nettside
prep20 <- estimateEffect(1:20 
                       ~ Nettside + s(Publiseringsdato),
                       stmobj = stm20_begge,
                       metadata = meta_begge,
                       set.seed(1234),
                       uncertainty = "Global")

effect_20_nett <- extract.estimateEffect(prep20, 
                                         "Nettside", 
                                         method = "pointestimate") # Henter ut estimatene fra estimateEffect

effect_20_nett$topic <- reorder(x = effect_20_nett$topic, 
                                effect_20_nett$estimate) # Omsorterer emnene etter estimatene

stm20_nett <- ggplot(effect_20_nett, aes(x = topic, y = estimate, group = covariate.value, color = covariate.value)) +
  geom_point(size = 3) +
  scale_y_continuous(name = "") +
  scale_x_discrete(name = "Emne", limits = rev(levels(effect_20_nett$topic))) +
  theme(legend.title = element_blank(),legend.position = "top") 
print(stm20_nett)


# Etter dato
  # Lager labels til x-aksen
labels2 <- seq(from = 12055, to = 18625, by = 730)
labels2 <- c("12055" = "2003", "12785" = "2005", "13515" = "2007", "14245" = "2009",
            "14975" = "2011", "15705" = "2013", "16435" = "2015", "17165" = "2017",
            "17895" = "2019", "18625" = "2021")

  # emner 1-4
effect_20_dato1 <- extract.estimateEffect(prep20,
                                          "Publiseringsdato",
                                          topics = c(1:20),
                                          method = "continuous")

effect_20_dato1 <- effect_20_dato1 %>%
  mutate(topic = fct_reorder(as.factor(topic), covariate.value))

stm20_dato1 <- ggplot(effect_20_dato1, aes(x = covariate.value, y = estimate, group = topic, color = topic)) +
  geom_line(size = 1) +
  scale_y_continuous() +
  scale_x_continuous(breaks = seq(from = 12055, to = 18625, by = 730), labels = labels2) +
  theme(legend.position = "top") + 
  labs(y = "", x = "")
prin(stm20_dato1)

# emner 5-8
effect_20_dato2 <- extract.estimateEffect(prep20,
                                          "Publiseringsdato",
                                          topics = c(5:8),
                                          method = "continuous")

effect_20_dato2 <- effect_20_dato2 %>%
  mutate(topic = fct_reorder(as.factor(topic), covariate.value))

stm20_dato2 <- ggplot(effect_20_dato2, aes(x = covariate.value, y = estimate, group = topic, color = topic)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0.01, 0.23)) +
  scale_x_continuous(breaks = seq(from = 12055, to = 18625, by = 730), labels = labels2) +
  theme(legend.position = "top") + 
  labs(y = "", x = "")

  # emner 9-12
effect_20_dato3 <- extract.estimateEffect(prep20,
                                          "Publiseringsdato",
                                          topics = c(9:12),
                                          method = "continuous")

effect_20_dato3 <- effect_20_dato3 %>%
  mutate(topic = fct_reorder(as.factor(topic), covariate.value))

stm20_dato3 <- ggplot(effect_20_dato3, aes(x = covariate.value, y = estimate, group = topic, color = topic)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0.01, 0.23)) +
  scale_x_continuous(breaks = seq(from = 12055, to = 18625, by = 730), labels = labels2) +
  theme(legend.position = "top") +
  labs(y = "", x = "")

# emner 13-16
effect_20_dato4 <- extract.estimateEffect(prep20,
                                          "Publiseringsdato",
                                          topics = c(13:16),
                                          method = "continuous")

effect_20_dato4 <- effect_20_dato4 %>%
  mutate(topic = fct_reorder(as.factor(topic), covariate.value))

stm20_dato4 <- ggplot(effect_20_dato4, aes(x = covariate.value, y = estimate, group = topic, color = topic)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0.01, 0.23)) +
  scale_x_continuous(breaks = seq(from = 12055, to = 18625, by = 730), labels = labels2) +
  theme(legend.position = "top")+
  labs(y = "", x = "")

# emner 17-20
effect_20_dato5 <- extract.estimateEffect(prep20,
                                          "Publiseringsdato",
                                          topics = c(17:20),
                                          method = "continuous")

effect_20_dato5 <- effect_20_dato5 %>%
  mutate(topic = fct_reorder(as.factor(topic), covariate.value))

stm20_dato5 <- ggplot(effect_20_dato5, aes(x = covariate.value, y = estimate, group = topic, color = topic)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0.01, 0.23)) +
  scale_x_continuous(breaks = seq(from = 12055, to = 18625, by = 730), labels = labels2) +
  theme(legend.position = "top")+
  labs(y = "", x = "")

grid.arrange(stm20_dato1, stm20_dato2, stm20_dato3, stm20_dato4, stm20_dato5, ncol = 2, nrow = 3)

# Ser hvordan bruken har forandret seg mellom nettsidene gjennom åra
prep <- estimateEffect(c(1:20) 
                       ~ Nettside * s(Publiseringsdato), 
                       stm20_begge,
                       metadata = meta_begge, 
                       uncertainty = "Global")

effect <- get_effects(prep, variable = "Publiseringsdato",
                      type = "continuous",
                      moderator = "Nettside",
                      modval = "Rights") %>%
  bind_rows(
    get_effects(prep, variable = "Publiseringsdato",
                type = "continuous",
                moderator = "Nettside",
                modval = "Document")
  )

effect_plot <- effect %>%
  filter(topic==20) %>% # Endrer sammen med overskrifter for hvert emne
  mutate(moderator = as.factor(moderator)) %>%
  ggplot(aes(x = value, y = proportion, color = moderator,
             group = moderator, fill = moderator)) +
  geom_line(size = 1) +
  scale_x_continuous("", breaks = seq(from = 12055, to = 18625, by = 730), labels = labels2) +
  theme(legend.position = "top")+
  labs(title = "Emne 20", 
       x = "", y = "",
                        color = "Nettside", group = "Nettside", fill = "Nettside")
print(effect_plot)


# Ser på kontrastord mellom nettsidene i emnet
plot(stm20_begge, type = "perspectives", topics = 6)


# Cluster
out_corr20 <- topicCorr(stm20_begge, set.seed(1234))
out_corr20$cor[out_corr20$cor<0.04] <- 0
nettverk <- graph_from_adjacency_matrix(out_corr20$cor, weighted = T, mode = "undirected", diag = F, set.seed(1234))
plot(nettverk, vertex.color = "light blue", vertex.label.color = "black")

out_corr20 <- topicCorr(stm20_begge, set.seed(1234))
cluster <- eclust(scale(out_corr20$cor), "hclust", k=5, nboot = 500)
fviz_dend(cluster, rect = TRUE) # Dendrogram

clust_col <- cluster$cluster
clust_col <- to_factor(clust_col)
levels(clust_col) <- c("red", "yellow", "green","blue", "purple")

leg.txt <- c("Svensk/dansk", "Engelsk", "Midtøsten", "Politikk og lov", "Div")

netverk <- graph_from_adjacency_matrix(out_corr20$cor, weighted = TRUE, mode = "undirected", diag = FALSE, set.seed(1234))
plot(netverk, vertex.color = clust_col, vertex.label.color = "black",) # Med farger etter dendrogrammet
legend("topleft", leg.txt, fill = c("green", "blue", "orange", "light blue", "yellow"))



  # Korrelasjonsmatrix
out_corr20 <- topicCorr(stm20_begge)
corr <- as.data.frame(out_corr20$cor)

ggc <- ggcorr(out_corr20$cor, hjust = .85, size = 0, label = TRUE, method = c("pairwise", "pearson"))

dat <- data.frame(x=seq(corr), y=seq(corr),
                  lbs = gsub("V", "\n", names(corr) ))
ggc + 
  geom_text(data = dat, aes(x, y, label = lbs), nudge_x  = 0.1, nudge_y = 0.21)



