#
#
#artiklar_index_inst
#180925 GL / 200518 GL / 201119 GL / 211110 GL / 230117 GL
#Sammanställning av artiklar för en institution. WoS, Scopus, Norska listan och DOAJ.
#Nivå X införd i Norska listan september 2021
#
#

library(tidyverse)

source('/home/shub/src/common/lib/sh_parameters.R')
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')

sh_archive_start("Institution_pub")

doaj_listan <- read_csv("/home/shub/assets/doaj.csv")

#2021 parsas fel. Alt ändra vectornamn
n_issn <- read.csv(file="/home/shub/assets/nsd.issn.csv",
                   header=TRUE,
                   sep=";",
                   na.strings = c("", "NA"),
                   stringsAsFactors = FALSE,
                   encoding = "utf8")
#n_issn$Nivå.2022 <- as.numeric(n_issn$Nivå.2022, "X" = "8")

diva <- read_csv("/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>% filter(between(Year, 2020, 2024))

diva_author <- read_csv(file="/home/shub/assets/diva/diva_author_sh_latest.csv")
diva_author <- diva_author %>% filter(between(Year, 2020, 2024))

diva$JournalISSN[is.na(diva$JournalISSN)] <- 0L
diva$JournalEISSN[is.na(diva$JournalEISSN)] <- 0L
diva$FreeFulltext[diva$FreeFulltext == "true"] <- TRUE

#Samtliga pulikationer begränsat till institution
inst_pub <- diva %>% filter_orgs(sam)
inst_pub_author <- diva_author %>% filter_orgs_author(sam)


# Institutionens artiklar -------------------------------------------------
#Urval vetenskapliga artiklar, published och in press
art <- inst_pub %>%
  filter(PublicationType %in% c("Artikel, forskningsöversikt", "Artikel i tidskrift")) %>%
  filter(ContentType != "Övrig (populärvetenskap, debatt, mm)") %>%
  filter(Status == "published"|Status == "inPress") %>%
  mutate(doaj = ((JournalISSN %in% doaj_listan$`Journal ISSN (print version)`)|
                   (JournalEISSN %in% doaj_listan$`Journal EISSN (online version)`)))%>%
  mutate(nsd = ((JournalISSN %in% n_issn$`Print.ISSN`)|
                  (JournalEISSN %in% n_issn$`Online.ISSN`)))%>%
  select(PID, Name, NumberOfAuthors, Title, Journal, JournalISSN, JournalEISSN, Year, ContentType, PublicationSubtype, Status, 
         ISI, ScopusId, nsd, FullTextLink, FreeFulltext, doaj)

#Antal artiklar med status Ahead of print
ahead <- inst_pub %>%
  filter(PublicationType %in% c("Artikel, forskningsöversikt", "Artikel i tidskrift")) %>%
  filter(ContentType != "Övrig (populärvetenskap, debatt, mm)") %>%
  filter(Status == "aheadofprint") %>%
  mutate(doaj = ((JournalISSN %in% doaj_listan$`Journal ISSN (print version)`)|
                   (JournalEISSN %in% doaj_listan$`Journal EISSN (online version)`)))%>%
  mutate(nsd = ((JournalISSN %in% n_issn$`Print.ISSN`)|
                  (JournalEISSN %in% n_issn$`Online.ISSN`)))%>%
  select(PID, Name, NumberOfAuthors, Title, Journal, JournalISSN, JournalEISSN, Year, ContentType, PublicationSubtype, Status, 
         ISI, ScopusId, nsd, FullTextLink, FreeFulltext, doaj)

# Författare --------------------------------------------------------------

art_author <- inst_pub_author %>% 
  filter(PublicationType %in% c("Artikel, forskningsöversikt", "Artikel i tidskrift")) %>%
  filter(ContentType != "Övrig (populärvetenskap, debatt, mm)") %>%
  filter(Status == "published"|Status == "inPress") %>%
  select(PID, Name.x, Position, OrganisationIds, Title, Journal, Year, ContentType, PublicationSubtype, Status, ISI, ScopusId)


# Index -------------------------------------------------------------------
# Matchning norska listan.
# nsd_kol = vektor som bestämmer vilken kolumn nivåvärdet hämtas från ur norska filerna
# Omatchade publikationer måste tas bort innan nivån hämtas. Läggs tillbaka efteråt.

year1 <- 2020
year2 <- 2021
year3 <- 2022
year4 <- 2023
year5 <- 2024


art_1 <- art %>%
  filter(Year == year1) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", year1) 

art_norsk <- art_1 %>%
  filter(nsd_row > 0) %>%
  #rowwise behövs för att indexeringen i nästa rad ska fungera:
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  #för att ta bort rowwise:
  ungroup()

art_ej_norsk <- art_1 %>%
  filter(nsd_row == 0)

year_1 <- bind_rows(art_norsk, art_ej_norsk)

art_2 <- art %>%
  filter(Year == year2) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", year2) 

art_norsk <- art_2 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

art_ej_norsk <- art_2 %>%
  filter(nsd_row == 0)

year_2 <- bind_rows(art_norsk, art_ej_norsk)

art_3 <- art %>%
  filter(Year == year3) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", year3)

art_norsk <- art_3 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

art_ej_norsk <- art_3 %>%
  filter(nsd_row == 0)

year_3 <- bind_rows(art_norsk, art_ej_norsk)

art_4 <- art %>%
  filter(Year == year4) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", year4)

art_norsk <- art_4 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

art_ej_norsk <- art_4 %>%
  filter(nsd_row == 0)

year_4 <- bind_rows(art_norsk, art_ej_norsk)

art_5 <- art %>%
  filter(Year == year5) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", year5)

art_norsk <- art_5 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

art_ej_norsk <- art_5 %>%
  filter(nsd_row == 0)

year_5 <- bind_rows(art_norsk, art_ej_norsk)

art_alla <- bind_rows(year_1, year_2, year_3, year_4, year_5)
art_alla$ISI[!is.na(art_alla$ISI)] <- 1L
art_alla$ISI[is.na(art_alla$ISI)] <- 0L
art_alla$ScopusId[!is.na(art_alla$ScopusId)] <- 1L
art_alla$ScopusId[is.na(art_alla$ScopusId)] <- 0L
art_alla$doaj[art_alla$doaj == TRUE] <- 1L
art_alla$doaj[art_alla$doaj == FALSE] <- 0L
art_alla$nivå[is.na(art_alla$nivå)] <- 0L

norsk <- art_alla %>%
  group_by(nivå) %>% 
  count(Year) %>%
  spread(Year, n)
norsk ["Indikator"] <- "Nivå norska listan"
norsk <- norsk %>%
  unite(nivå, Indikator, nivå, sep=": ") %>%
  rename(Index = nivå)

ISI <- art_alla %>%
  group_by(ISI) %>% 
  count(Year) %>%
  spread(Year, n)
ISI ["Indikator"] <- "Web of Science"
ISI <- ISI %>% 
  unite(ISI, Indikator, ISI, sep=": ") %>%
  rename(Index = ISI)

Scopus <- art_alla %>%
  group_by(ScopusId) %>% 
  count(Year) %>%
  spread(Year, n)
Scopus ["Indikator"] <- "Scopus"
Scopus <- Scopus %>% 
  unite(ScopusId, Indikator, ScopusId, sep=": ") %>%
  rename(Index = ScopusId)

Doaj <- art_alla %>%
  group_by(doaj) %>% 
  count(Year) %>%
  spread(Year, n)
Doaj ["Indikator"] <- "DOAJ"
Doaj <- Doaj %>% 
  unite(doaj, Indikator, doaj, sep=": ") %>%
  rename(Index = doaj)

art_ind <- bind_rows(norsk, ISI, Scopus, Doaj)


# Konferensbidrag ---------------------------------------------------------
inst_konf <- inst_pub %>%
  filter(PublicationType == "Konferensbidrag") %>%
  select(PID, Name, Title, Year, ContentType, PublicationSubtype, ScopusId)


# Artiklar SH -------------------------------------------------------------
art_sh <- diva %>%
  filter(PublicationType %in% c("Artikel, forskningsöversikt", "Artikel i tidskrift")) %>%
  filter(ContentType != "Övrig (populärvetenskap, debatt, mm)") %>%
  filter(Status == "published"|Status == "inPress") %>%
  mutate(doaj = ((JournalISSN %in% doaj_listan$`Journal ISSN (print version)`)|
                   (JournalEISSN %in% doaj_listan$`Journal EISSN (online version)`)))%>%
  mutate(nsd = ((JournalISSN %in% n_issn$`Print.ISSN`)|
                  (JournalEISSN %in% n_issn$`Online.ISSN`)))%>%
  select(PID, Name, NumberOfAuthors, Title, Journal, JournalISSN, JournalEISSN, Year, ContentType, PublicationSubtype, Status, 
         ISI, ScopusId, nsd, FullTextLink, FreeFulltext, doaj)

art_1 <- art_sh %>%
  filter(Year == year1) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", year1) 

art_norsk <- art_1 %>%
  filter(nsd_row > 0) %>%
  #rowwise behövs för att indexeringen i nästa rad ska fungera:
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  #för att ta bort rowwise:
  ungroup()

art_ej_norsk <- art_1 %>%
  filter(nsd_row == 0)

year_1 <- bind_rows(art_norsk, art_ej_norsk)

art_2 <- art_sh %>%
  filter(Year == year2) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", year2) 

art_norsk <- art_2 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

art_ej_norsk <- art_2 %>%
  filter(nsd_row == 0)

year_2 <- bind_rows(art_norsk, art_ej_norsk)

art_3 <- art_sh %>%
  filter(Year == year3) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", year3)

art_norsk <- art_3 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

art_ej_norsk <- art_3 %>%
  filter(nsd_row == 0)

year_3 <- bind_rows(art_norsk, art_ej_norsk)

art_4 <- art_sh %>%
  filter(Year == year4) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", year4)

art_norsk <- art_4 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

art_ej_norsk <- art_4 %>%
  filter(nsd_row == 0)

year_4 <- bind_rows(art_norsk, art_ej_norsk)

art_5 <- art_sh %>%
  filter(Year == year5) %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", year5)

art_norsk <- art_5 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

art_ej_norsk <- art_5 %>%
  filter(nsd_row == 0)

year_5 <- bind_rows(art_norsk, art_ej_norsk)

art_alla_sh <- bind_rows(year_1, year_2, year_3, year_4, year_5)
art_alla_sh$ISI[!is.na(art_alla_sh$ISI)] <- 1L
art_alla_sh$ISI[is.na(art_alla_sh$ISI)] <- 0L
art_alla_sh$ScopusId[!is.na(art_alla_sh$ScopusId)] <- 1L
art_alla_sh$ScopusId[is.na(art_alla_sh$ScopusId)] <- 0L
art_alla_sh$doaj[art_alla_sh$doaj == TRUE] <- 1L
art_alla_sh$doaj[art_alla_sh$doaj == FALSE] <- 0L
art_alla_sh$nivå[is.na(art_alla_sh$nivå)] <- 0L

norsk <- art_alla_sh %>%
  group_by(nivå) %>% 
  count(Year) %>%
  spread(Year, n)
norsk ["Indikator"] <- "Nivå norska listan"
norsk <- norsk %>%
  unite(nivå, Indikator, nivå, sep=": ") %>%
  rename(Index = nivå)

ISI <- art_alla_sh %>%
  group_by(ISI) %>% 
  count(Year) %>%
  spread(Year, n)
ISI ["Indikator"] <- "Web of Science"
ISI <- ISI %>% 
  unite(ISI, Indikator, ISI, sep=": ") %>%
  rename(Index = ISI)

Scopus <- art_alla_sh %>%
  group_by(ScopusId) %>% 
  count(Year) %>%
  spread(Year, n)
Scopus ["Indikator"] <- "Scopus"
Scopus <- Scopus %>% 
  unite(ScopusId, Indikator, ScopusId, sep=": ") %>%
  rename(Index = ScopusId)

Doaj <- art_alla_sh %>%
  group_by(doaj) %>% 
  count(Year) %>%
  spread(Year, n)
Doaj ["Indikator"] <- "DOAJ"
Doaj <- Doaj %>% 
  unite(doaj, Indikator, doaj, sep=": ") %>%
  rename(Index = doaj)

art_ind_sh <- bind_rows(norsk, ISI, Scopus, Doaj)


# Arkivering --------------------------------------------------------------
inst_pub <- select(inst_pub, -(list_of_orgs))
sh_archive_df(art_alla, "Artiklar_inst.csv")
sh_archive_df(ahead, "Artiklar_inst_ahead.csv")
sh_archive_df(art_author, "Artiklar_författare.csv")
sh_archive_df(inst_pub, "Publikationer_inst.csv")
sh_archive_df(art_ind, "Artiklar_index.csv")
sh_archive_df(inst_konf, "Konferens_inst.csv")
sh_archive_df(diva, "Publikationer_sh.csv")
sh_archive_df(art_ind_sh, "Artiklar_index_sh")
sh_archive_end()

