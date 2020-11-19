#
#
#artiklar_inst
#180925 GL / 200518 GL / 201119 GL
#Sammanställning av artiklar för en institution. WoS, Scopus, Norska listan och DOAJ.
#
#

library(tidyverse)

source('/home/shub/src/common/lib/sh_parameters.R')
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')

sh_archive_start("Institution_pub")

doaj_listan <- read_csv("/home/shub/assets/doaj.csv")

n_issn <- read.csv(file="/home/shub/assets/nsd.issn.csv",
                   header=TRUE,
                   sep=";",
                   na.strings = c("", "NA"),
                   stringsAsFactors = FALSE,
                   encoding = "latin1")

diva <- read_csv("/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>% filter(between(Year, 2016, 2020))

diva$JournalISSN[is.na(diva$JournalISSN)] <- 0L
diva$JournalEISSN[is.na(diva$JournalEISSN)] <- 0L
diva$FreeFulltext[diva$FreeFulltext == "true"] <- TRUE

#Samtliga pulikationer begränsat till institution och status
inst_pub <- diva %>% filter_orgs(sam)

#Urval vetenskapliga artiklar , ej submitted
art <- inst_pub %>%
  filter(PublicationType %in% c("Artikel, forskningsöversikt", "Artikel i tidskrift")) %>%
  filter(ContentType != "Övrig (populärvetenskap, debatt, mm)") %>%
  filter(Status != "Submitted") %>%
  mutate(doaj = ((JournalISSN %in% doaj_listan$`Journal ISSN (print version)`)|
                   (JournalEISSN %in% doaj_listan$`Journal EISSN (online version)`)))%>%
  mutate(nsd = ((JournalISSN %in% n_issn$`Print.ISSN`)|
                  (JournalEISSN %in% n_issn$`Online.ISSN`)))%>%
  select(PID, Name, Title, Journal, JournalISSN, JournalEISSN, Year, ContentType, PublicationSubtype, Status, 
         ISI, ScopusId, nsd, FullTextLink, FreeFulltext, doaj)

# Index -------------------------------------------------------------------
# Matchning norska listan.
# nsd_kol = vektor som bestämmer vilken kolumn nivåvärdet hämtas från ur norska filerna
# Omatchade publikationer måste tas bort innan nivån hämtas. Läggs tillbaka efteråt.

year1 <- 2016
year2 <- 2017
year3 <- 2018
year4 <- 2019
year5 <- 2020


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
art_alla$ScopusId[!is.na(art_alla$ScopusId)] <- 1L
art_alla$doaj[art_alla$doaj == TRUE] <- 1L
art_alla$doaj[art_alla$doaj == FALSE] <- 0L
art_alla[is.na(art_alla)] <- 0L

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

# Arkivering --------------------------------------------------------------
inst_pub <- select(inst_pub, -(list_of_orgs))
sh_archive_df(art_alla, "Artiklar_inst.csv")
sh_archive_df(inst_pub, "Publikationer_inst.csv")
sh_archive_df(art_ind, "Artiklar_index.csv")
sh_archive_df(inst_konf, "Konferens_inst.csv")
sh_archive_df(diva, "Diva_rådata.csv")
sh_archive_end()
