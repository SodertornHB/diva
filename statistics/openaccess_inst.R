#
#
#Öppen publicering institution
#180117 GL
#Kopia av skriptet för hela SH, men med filter för institution
#
#


library(tidyverse)

source('/home/shub/src/common/lib/sh_parameters.R')
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')

#Byt namn på arkivet
sh_archive_start("OA HS")

#Välj institution att filtrera på: hs, ikl, nmt, sam
diva <- read_csv("/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>%
  filter(between(Year, 2013, 2017)) %>%
  filter_orgs(hs)

doaj_listan <- read_csv("/home/shub/assets/doaj.csv")

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

#Spara i excel innan leverans
diva <- select(diva, -(list_of_orgs))
sh_archive_df(diva, "DiVA_rådata_inst.csv")
sh_archive_df(counts_oa, "Antal_oa_inst.csv")
sh_archive_df(shares_oa, "Andel_oa_inst.csv")
sh_archive_df(counts_tot, "Tot_pub_inst.csv")
sh_archive_end()

