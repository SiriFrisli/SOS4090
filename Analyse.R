pakker <- c("rvest", "dplyr", "tibble", "httr", "writexl", "tm", "tidytext", "stringr",
            "stopwords", "ggplot2", "topicmodels", "SnowballC", "quanteda", "lubridate",
            "stm", "plotly", "geometry", "stminsights", "Rtsne", "rsvd", "forcats", "purrr",
            "factoextra", "RColorBrewer", "wordcloud", "corrr", "labelled", "tidyverse",
            "tidystm", "stminsights", "GGally", "ggraph", "igraph", "gridExtra")
lapply(pakker, library, character.only = TRUE)
set.seed(1234)

# Før koden kjøres hentes det inn en STM med 20 emner

# Emnefordelinger
stm20_gamma <- tidy(stm20_begge,
                    matrix = "gamma")

gamma_terms20 <- stm20_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms20 %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma)) +
  geom_col(fill = "#00bfc4", color = "black", show.legend = FALSE) +
  labs(title = "Emnefordeling", y = "", x = "") +
  coord_flip()

# Finne artikler mest assosiert med de spesifikke emnene, brukes for tolkning og analyse
findThoughts(stm20_begge, meta_begge$Tekst, topics = 9, n = 1)
# Forstår ikke helt hvorfor jeg får de resultata jeg får, og finner lite info om
# hva denne koden egentlig gjør. Finner dokumenter assosiert med de ulike emnene på andre måter

theta <- make.dt(stm20_begge) # theta-verdiene er emneandelene til hvert enkelt dokument
theta8 <- theta %>%
  arrange(desc(Topic8)) %>%
  slice(1:20)
theta8 # Finner topp tre artikler med høyest theta verdi per emne. Gir meg radnummer, ikke radnavn

meta_begge[, ] # Legg inn tall fra forrige ledd, bare for å se hvilken nettside artikkel er fra
meta_begge[, ] #

# Lagrer artikler for emnene
sink("8.csv", append = FALSE, split = TRUE)
theta8 <- theta %>%
  arrange(desc(Topic8)) %>%
  slice(1)
theta8

theta8 <- theta %>%
  arrange(desc(Topic8)) %>%
  slice(2)
theta8
sink()

# Ser på bruken av emnene etter nettside
prep20 <- estimateEffect(1:20
                         ~ Nettside + s(Publiseringsdato),
                         stmobj = stm20_begge,
                         metadata = meta_begge,
                         set.seed(1234),
                         uncertainty = "Global")

effect_20_nett <- extract.estimateEffect(prep20,
                                         "Nettside",
                                         136
                                         method = "pointestimate") # Henter ut estimatene fra
estimateEffect
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
# Utvalgte emner, samlet for nettsidene, ikke inkludert i oppgava
effect_20_dato1 <- extract.estimateEffect(prep20,
                                          "Publiseringsdato",
                                          topics = c(5, 16),
                                          method = "continuous")

effect_20_dato1 <- effect_20_dato1 %>%
  mutate(topic = fct_reorder(as.factor(topic), covariate.value))

stm20_dato1 <- ggplot(effect_20_dato1, aes(x = covariate.value, y = estimate, group = topic,color = topic)) +
  geom_line(size = 1) +
  scale_y_continuous() +
  scale_x_continuous(breaks = seq(from = 12055, to = 18625, by = 730), labels = labels2) +
  theme(legend.position = "top") +
  labs(title = "Innvandringskritikk", y = "", x = "", color = "Emner:") +
  scale_color_manual(labels = c("5: Verdibasert uforenlighet", "16: Økonomiske konsekvenser"), values=c("#F8766D", "#00BFC4"))
print(stm20_dato1)
# Alle emnene, ikke inkludert i oppgava
effect_20_dato2 <- extract.estimateEffect(prep20,
                                          "Publiseringsdato",
                                          topics = c(1:20),
                                          method = "continuous")

effect_20_dato2 <- effect_20_dato2 %>%
  mutate(topic = fct_reorder(as.factor(topic), covariate.value))

stm20_dato2 <- ggplot(effect_20_dato2, aes(x = covariate.value, y = estimate, group = topic, color = topic)) +
  geom_line(size = 1.5) +
  scale_y_continuous() +
  scale_x_continuous(breaks = seq(from = 12055, to = 18625, by = 730), labels = labels2) +
  theme(legend.position = "top") +
  labs(title = "Emnene over tid", y = "", x = "")
print(stm20_dato2)

# Klynger
out_corr20 <- topicCorr(stm20_begge, cutoff = 0.04, set.seed(1234))
out_corr20$cor[out_corr20$cor<0.04] <- 0

nettverk <- graph_from_adjacency_matrix(out_corr20$cor, weighted = T, mode = "undirected", diag =
                                          F, set.seed(1234))

plot(nettverk, vertex.color = "light blue", vertex.label.color = "black")

out_corr20 <- topicCorr(stm20_begge, cutoff = 0.04, set.seed(1234))
cluster <- eclust(scale(out_corr20$cor), "hclust", k=5, nboot = 500)

fviz_dend(cluster, rect = TRUE) # Dendrogram

clust_col <- cluster$cluster
clust_col <- to_factor(clust_col)

leg.txt <- c("Svensk/dansk", "Engelsk", "Midtøsten", "Politikk og lov", "Div") # Labels

netverk <- graph_from_adjacency_matrix(out_corr20$cor, weighted = TRUE, mode = "undirected", diag = FALSE, set.seed(1234))
plot(netverk, vertex.color = clust_col, vertex.label.color = "black",) # Med farger etter dendrogrammet
legend("topleft", leg.txt, fill = c("dark green", "blue", "orange", "light blue", "yellow"))

# Korrelasjonsmatrix
out_corr20 <- topicCorr(stm20_begge, cutoff = 0.04)
corr <- as.data.frame(out_corr20$cor)

ggc <- ggcorr(out_corr20$cor, hjust = .85, size = 0, label = TRUE, method = c("pairwise", "pearson"))
dat <- data.frame(x=seq(corr), y=seq(corr),
                  lbs = gsub("V", "\n", names(corr) ))

ggc +
  geom_text(data = dat, aes(x, y, label = lbs), nudge_x = 0.1, nudge_y = 0.21)

# Ser forandring over tid mellom sidene
prep <- estimateEffect(c(1:20)
                       ~ Nettside * s(Publiseringsdato),
                       stm20_begge,
                       metadata = meta_begge,
                       uncertainty = "Global")

effect <- get_effects(prep, variable = "Publiseringsdato",
                      type = "continuous",
                      moderator = "Nettside",
                      modval = "Rights") %>%
  bind_rows(get_effects(prep, variable = "Publiseringsdato",
                type = "continuous",
                moderator = "Nettside",
                modval = "Document"))

effect_plot <- effect %>%
  filter(topic==20) %>% # Endrer sammen med overskrifter for hvert emne
  mutate(moderator = as.factor(moderator)) %>%
  ggplot(aes(x = value, y = proportion, color = moderator, group = moderator, fill = moderator)) +
  geom_line(size = 1) +
  scale_x_continuous("", breaks = seq(from = 12055, to = 18625, by = 730), labels = labels2) +
  scale_y_continuous(limits = c(0.02,0.25)) +
  theme(legend.position = "top") +
  labs(title = "Emne 20: Religiøsitet", x = "", y = "", color = "Nettside", group = "Nettside", fill = "Nettside")
print(effect_plot)