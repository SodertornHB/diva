#
#
#publicering_amne
#180206 GL
#Sammanställning av publikationer för ett eller flera ämnen. Antal, tidskrifter, förlag, OA-statistik.
#
#

library(tidyverse)

source('/home/shub/src/common/lib/sh_parameters.R')
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')

sh_archive_start("Eng_Litt")

doaj_listan <- read_csv("/home/shub/assets/doaj.csv")

diva <- read_csv("/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>%
  filter(between(Year, 2013, 2017)) %>%
  filter_publsubtype_papers()

eng_pub <- diva %>%
  filter_orgs(engelska)

litt_pub <- diva %>%
  filter_orgs(litteraturvetenskap)

amnet_pub <- bind_rows(eng_pub, litt_pub)
amnet_pub <- select(amnet_pub, -(list_of_orgs))


amnet_pub_tab <- amnet_pub %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ") #SORTERING
amnet_pub_tab[is.na(amnet_pub_tab)] <- 0L 

sh_archive_df(amnet_pub, "Ämnet_publikationer.csv")
sh_archive_df(amnet_pub_tab, "Ämnet_tabell")


#Antal indexeringar i WoS, Scopus och nivå 1 eller 2 i norska listan
WoS <- subset(amnet_pub, ISI != "")
Scopus <- subset(amnet_pub, ScopusId !="")
Norska <- subset(amnet_pub, FridaLevel !="0")

WoSn <- nrow(WoS)
Scopusn <- nrow(Scopus)
norn <- nrow(Norska)
indexering <- c("WoS"=WoSn, "Scopus"=Scopusn, "Norska listan"=norn)

#Statistik för förlag och tidskrift
amnet_forlag <- amnet_pub %>%
  filter(PublicationType == "Bok" |PublicationType == "Kapitel i bok, del av antologi" |PublicationType == "Konferensbidrag" |PublicationType == "Samlingsverk (redaktörskap)" |PublicationType == "Proceeding (redaktörskap)") %>%
  count(Publisher) %>%
  arrange(Publisher)

amnet_tidsk <- amnet_pub %>%
  filter(PublicationType == "Artikel i tidskrift" |PublicationType == "Artikel, forskningsöversitk" |PublicationType == "Artikel, recension") %>%
  count(Journal) %>%
  arrange(Journal)


# OA-statistik ------------------------------------------------------------
#Kopierad version av OA för hela organisationen. DOAJ-listan läses in i början av skriptet.
#Ändra namn på filen för ämnet, så behöver inte skriptet skrivas om.

diva <- amnet_pub

diva$JournalISSN[is.na(diva$JournalISSN)] <- 0L
diva$JournalEISSN[is.na(diva$JournalEISSN)] <- 0L
diva$FreeFulltext[diva$FreeFulltext == "true"] <- TRUE

oa_diva_art <- diva %>%
  select(PID, PublicationType, ContentType, Language, JournalISSN, JournalEISSN, Status, Year, FullTextLink, FreeFulltext) %>%
  filter(ContentType %in% content_type[c("a", "b")]) %>%
  filter(PublicationType %in% publication_type[c("a", "b")]) %>%
  mutate(doaj_issn = ((JournalISSN %in% doaj_listan$`Journal ISSN (print version)`)|
                        (JournalEISSN %in% doaj_listan$`Journal EISSN (online version)`)))

oa_diva_chap <- diva %>%
  select(PID, PublicationType, ContentType, Language, Publisher, Status, Year, FullTextLink, FreeFulltext) %>%
  filter(ContentType %in% c("Övrigt vetenskapligt", "Refereegranskat")) %>%
  filter(PublicationType %in% c("Kapitel i bok, del av antologi"))

oa_diva_book <- diva %>%
  select(PID, PublicationType, ContentType, Language, Publisher, Status, Year, FullTextLink, FreeFulltext) %>%
  filter(ContentType %in% c("Övrigt vetenskapligt", "Refereegranskat")) %>%
  filter(PublicationType %in% c("Bok"))

#Kapitel olika OA
stats_freefulltext_chap <- oa_diva_chap %>%
  filter(FreeFulltext == TRUE) %>%
  group_by(Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

stats_divafulltext_chap <- oa_diva_chap %>%
  filter(!(is.na(FullTextLink))) %>%
  group_by(Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

stats_tot_chap <- oa_diva_chap %>%
  group_by (Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

stats_unique_chap <- oa_diva_chap %>%
  filter(FreeFulltext == TRUE | (!(is.na(FullTextLink)))) %>%
  count(Year)

stats_oa_chap <- full_join(stats_tot_chap, stats_freefulltext_chap, by = "Year")
names(stats_oa_chap) <- c("Year", "chap_tot", "chap_freefulltext")
stats_oa_chap <- full_join(stats_oa_chap, stats_divafulltext_chap, by = "Year")
names(stats_oa_chap) <- c("Year", "chap_tot", "chap_freefulltext", "chap_divafulltext")
stats_oa_chap <- full_join(stats_oa_chap, stats_unique_chap, by = "Year")
names(stats_oa_chap) <- c("Year", "chap_tot", "chap_freefulltext", "chap_divafulltext", "chap_unique")

shares_chap <- stats_oa_chap %>%
  mutate(chap_percent_freefulltext = chap_freefulltext/chap_tot*100, 
         chap_percent_divafulltext = chap_divafulltext/chap_tot*100,
         chap_percent_unique = chap_unique/chap_tot*100) %>%
  select(Year, chap_percent_freefulltext, chap_percent_divafulltext, chap_percent_unique) %>%
  arrange(Year)

#Artikel olika OA
stats_doaj <- oa_diva_art %>%
  filter(doaj_issn == TRUE) %>%
  group_by(Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

stats_freefulltext <- oa_diva_art %>%
  filter(FreeFulltext == TRUE) %>%
  group_by (Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

stats_divafulltext <- oa_diva_art %>%
  filter(!(is.na(FullTextLink))) %>%
  group_by (Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

stats_tot <- oa_diva_art %>%
  group_by (Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

stats_unique_art <- oa_diva_art %>%
  filter(FreeFulltext == TRUE | doaj_issn == TRUE | (!(is.na(FullTextLink)))) %>%
  count(Year)

stats_oa_art <- full_join(stats_tot, stats_doaj, by = "Year")
names(stats_oa_art) <- c("Year", "art_tot", "art_doaj")
stats_oa_art <- full_join(stats_oa_art, stats_freefulltext, by = "Year")
names(stats_oa_art) <- c("Year", "art_tot", "art_doaj", "art_freefulltext")
stats_oa_art <- full_join(stats_oa_art, stats_divafulltext, by = "Year")
names(stats_oa_art) <- c("Year", "art_tot", "art_doaj", "art_freefulltext", "art_divafulltext")
stats_oa_art <- full_join(stats_oa_art, stats_unique_art, by = "Year")
names(stats_oa_art) <- c("Year", "art_tot", "art_doaj", "art_freefulltext", "art_divafulltext", "art_unique")

shares_art <- stats_oa_art %>%
  mutate(art_percent_doaj = art_doaj/art_tot*100,
         art_percent_freefulltext = art_freefulltext/art_tot*100,
         art_percent_divafulltext = art_divafulltext/art_tot*100, 
         art_percent_unique = art_unique/art_tot*100) %>%
  select(Year, art_percent_doaj, art_percent_freefulltext, art_percent_divafulltext, art_percent_unique)%>%
  arrange(Year)

#Bok olika OA
stats_freefulltext_book <- oa_diva_book %>%
  filter(FreeFulltext == TRUE) %>%
  group_by(Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

stats_divafulltext_book <- oa_diva_book %>%
  filter(!(is.na(FullTextLink))) %>%
  group_by(Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

stats_tot_book <- oa_diva_book %>%
  group_by (Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

stats_unique_book <- oa_diva_book %>%
  filter(FreeFulltext == TRUE | (!(is.na(FullTextLink)))) %>%
  count(Year)

stats_oa_book <- full_join(stats_tot_book, stats_freefulltext_book, by = "Year")
names(stats_oa_book) <- c("Year", "book_tot", "book_freefulltext")
stats_oa_book <- full_join(stats_oa_book, stats_divafulltext_book, by = "Year")
names(stats_oa_book) <- c("Year", "book_tot", "book_freefulltext", "book_divafulltext")
stats_oa_book <- full_join(stats_oa_book, stats_unique_book, by = "Year")
names(stats_oa_book) <- c("Year", "book_tot", "book_freefulltext", "book_divafulltext", "book_unique")

shares_book <- stats_oa_book %>%
  mutate(book_percent_freefulltext = book_freefulltext/book_tot*100,
         book_percent_divafulltext = book_divafulltext/book_tot*100,
         book_percent_unique = book_unique/book_tot*100) %>%
  select(Year, book_percent_freefulltext, book_percent_divafulltext, book_percent_unique) %>%
  arrange(Year)

#Tabell med antal
counts_oa <- full_join(stats_oa_art, stats_oa_chap, by = "Year")
counts_oa <- full_join(counts_oa, stats_oa_book, by = "Year")
counts_oa <- counts_oa %>%
  select(Year, art_doaj, art_freefulltext, art_divafulltext, art_unique, 
         chap_freefulltext, chap_divafulltext, chap_unique,
         book_freefulltext, book_divafulltext, book_unique) %>%
  t
counts_oa[is.na(counts_oa)] <- 0L

#Tabell med andel
shares_oa <- full_join(shares_art, shares_chap, by = "Year")
shares_oa <- full_join(shares_oa, shares_book, by = "Year")
shares_oa <- t(shares_oa)
shares_oa[is.na(shares_oa)] <- 0

#Tabell med totala
counts_tot <- full_join(stats_oa_art, stats_oa_chap, by = "Year")
counts_tot <- full_join(counts_tot, stats_oa_book, by = "Year")
counts_tot <- counts_tot %>%
  select(Year, art_tot, chap_tot, book_tot) %>%
  t
counts_tot[is.na(counts_tot)] <- 0L


# Arkivering --------------------------------------------------------------

sh_archive_df(indexering, "Antal_indexerade")
sh_archive_df(amnet_forlag, "Ämnet_förlag")
sh_archive_df(amnet_tidsk, "Ämnet_tidskrifter")

sh_archive_df(counts_oa, "Ämnet_antal_oa.csv")
sh_archive_df(shares_oa, "Ämnet_andel_oa.csv")
sh_archive_df(counts_tot, "Ämnet_tot_pub.csv")

sh_archive_end()

