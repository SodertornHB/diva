#
# subject_unit
# GL 210208
# Uppföljning publikationer i DiVA för ett ämne.
# Källa DiVA, kompletterat med nivå norska listan.
#
#

library(tidyverse)

source('/home/shub/src/common/lib/sh_parameters.R')
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')

n_issn <- read.csv(file="/home/shub/assets/nsd.issn.csv",
                   header=TRUE,
                   sep=";",
                   na.strings = c("", "NA"),
                   stringsAsFactors = FALSE,
                   encoding = "latin1")

n_forlag <- read.csv(file="/home/shub/assets/nsd.forlag.csv",
         header=TRUE,
         sep=";",
         na.strings = c("", "NA"),
         stringsAsFactors = FALSE,
         encoding = "latin1")
n_forlag$Original.tittel <- recode(n_forlag$Original.tittel, "Södertörns Högskola" = "Södertörns högskola")
n_forlag$Original.tittel <- recode(n_forlag$Original.tittel, "De Gruyter Mouton" = "Mouton de Gruyter")

sh_archive_start("Uppföljning ämne")

diva <- read_csv(file="/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva_author <- read_csv(file="/home/shub/assets/diva/diva_author_sh_latest.csv")

diva$JournalISSN[is.na(diva$JournalISSN)] <- 0L
diva$JournalEISSN[is.na(diva$JournalEISSN)] <- 0L
diva$SeriesISSN[is.na(diva$SeriesISSN)] <- 0L
diva$SeriesEISSN[is.na(diva$SeriesEISSN)] <- 0L


# Avgränsningar -----------------------------------------------------------
#Se även section Författare

year1 <- 2016
year2 <- 2017
year3 <- 2018
year4 <- 2019
year5 <- 2020

diva <- diva %>% filter(between(Year, year1, year5))
diva_subject <- diva %>% filter_orgs(foretagsekonomi)


# Publikationer -----------------------------------------------------------

publ_vet <- diva_subject %>%
  filter(PublicationType=="Artikel i tidskrift"|PublicationType=="Artikel, forskningsöversikt"
         |PublicationType=="Kapitel i bok, del av antologi"|PublicationType=="Bok"|PublicationType=="Konferensbidrag")%>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter((is.na(Status))|Status=="published"|Status=="epub ahead of print")%>%
  filter(is.na(PublicationSubtype)|PublicationSubtype == "publishedPaper")%>%
  mutate(nsd = ((JournalISSN %in% n_issn$`Print.ISSN`)|(JournalEISSN %in% n_issn$`Online.ISSN`)))

publ_vet$PublicationType <- recode(publ_vet$PublicationType,
                                   "Artikel i tidskrift" = "Artikel",
                                   "Artikel, forskningsöversikt" = "Artikel",
                                   "Kapitel i bok, del av antologi" = "Kapitel",
                                   "Bok" = "Monografi",
                                   "Konferensbidrag" = "Publicerat konferensbidrag")

publ_tabell <- publ_vet %>%
  group_by(PublicationType, ContentType) %>% 
  count(Year) %>%
  spread(Year, n) %>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

publ_tabell[is.na(publ_tabell)] <- 0L

write_csv(publ_tabell, "Tabell publikationer.csv")

# Norska listan -----------------------------------------------------------
# Matchning norska listan.
# nsd_kol = vektor som bestämmer vilken kolumn nivåvärdet hämtas från ur norska filerna
# Omatchade publikationer måste tas bort innan nivån hämtas. Läggs tillbaka efteråt.

#year1 <- 2016 #Anges i inledningen
#year2 <- 2017
#year3 <- 2018
#year4 <- 2019
#year5 <- 2020


art <- publ_vet %>%
  filter(PublicationType=="Artikel") %>%
  select(PID, Name, Title, Journal, JournalISSN, JournalEISSN, Year, Publisher, PublicationType, ContentType, ISI, ScopusId)

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
art_alla[is.na(art_alla)] <- 0L


# Tabell indexerade artiklar

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

index_art <- bind_rows(norsk, ISI, Scopus)
index_art[is.na(index_art)] <- 0L

write_csv(art_alla, "Artiklar_norska.csv")
write_csv(index_art, "Artiklar_index.csv")

# Norska listan böcker ----------------------------------------------------
# Viktigt att förlagsnamnen stämmer mot Norska listan. Samma år som definerats under artiklar.
# Dubbla serier behöver kollas manuellt i filen.


bok <- publ_vet %>%
  filter(PublicationType == "Monografi"| PublicationType == "Kapitel"|PublicationType == "Publicerat konferensbidrag") %>% 
  select(PID, Name, Title, Publisher, Year, Series, SeriesISSN, SeriesEISSN, PublicationType, ContentType)

bok_1 <- bok %>%
  filter(Year == year1) %>%
  mutate(nsd_row = match(Publisher, n_forlag$Original.tittel, nomatch = 0))

nsd_kol <- str_c("Nivå.", year1)   

bok_forlag_norsk <- bok_1 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_forlag[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_forlag_ej_norsk <- bok_1 %>%
  filter(nsd_row == 0)

forlag <- bind_rows(bok_forlag_norsk, bok_forlag_ej_norsk)
forlag[is.na(forlag)] <- 0L

bok_serie <- bok_1 %>%
  mutate(nsd_index_print = match(SeriesISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(SeriesEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

bok_serie_norsk <- bok_serie %>%
  filter(nsd_row > 0)

bok_serie_norsk <- bok_serie_norsk %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0)%>%
  mutate(nivå = 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

byear_1 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))

#Bok year2

bok_2 <- bok %>%
  filter(Year == year2) %>%
  mutate(nsd_row = match(Publisher, n_forlag$Original.tittel, nomatch = 0))

nsd_kol <- str_c("Nivå.", year2)

bok_forlag_norsk <- bok_2 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_forlag[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_forlag_ej_norsk <- bok_2 %>%
  filter(nsd_row == 0)

forlag <- bind_rows(bok_forlag_norsk, bok_forlag_ej_norsk)
forlag[is.na(forlag)] <- 0L

bok_serie <- bok_2 %>%
  mutate(nsd_index_print = match(SeriesISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(SeriesEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

bok_serie_norsk <- bok_serie %>%
  filter(nsd_row > 0)

bok_serie_norsk <- bok_serie_norsk %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0)%>%
  mutate(nivå = 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

byear_2 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))

#Bok year3

bok_3 <- bok %>%
  filter(Year == year3) %>%
  mutate(nsd_row = match(Publisher, n_forlag$Original.tittel, nomatch = 0))

nsd_kol <- str_c("Nivå.", year3)

bok_forlag_norsk <- bok_3 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_forlag[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_forlag_ej_norsk <- bok_3 %>%
  filter(nsd_row == 0)

forlag <- bind_rows(bok_forlag_norsk, bok_forlag_ej_norsk)
forlag[is.na(forlag)] <- 0L

bok_serie <- bok_3 %>%
  mutate(nsd_index_print = match(SeriesISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(SeriesEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

bok_serie_norsk <- bok_serie %>%
  filter(nsd_row > 0)

bok_serie_norsk <- bok_serie_norsk %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0)%>%
  mutate(nivå = 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

byear_3 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))

#Bok year4

bok_4 <- bok %>%
  filter(Year == year4) %>%
  mutate(nsd_row = match(Publisher, n_forlag$Original.tittel, nomatch = 0))

nsd_kol <- str_c("Nivå.", year4)

bok_forlag_norsk <- bok_4 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_forlag[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_forlag_ej_norsk <- bok_4 %>%
  filter(nsd_row == 0)

forlag <- bind_rows(bok_forlag_norsk, bok_forlag_ej_norsk)
forlag[is.na(forlag)] <- 0L

bok_serie <- bok_4 %>%
  mutate(nsd_index_print = match(SeriesISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(SeriesEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

bok_serie_norsk <- bok_serie %>%
  filter(nsd_row > 0)

bok_serie_norsk <- bok_serie_norsk %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0)%>%
  mutate(nivå = 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

byear_4 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))

#Bok year5

bok_5 <- bok %>%
  filter(Year == year5) %>% 
  mutate(nsd_row = match(Publisher, n_forlag$Original.tittel, nomatch = 0))

nsd_kol <- str_c("Nivå.", year5)

bok_forlag_norsk <- bok_5 %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_forlag[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_forlag_ej_norsk <- bok_5 %>%
  filter(nsd_row == 0)

forlag <- bind_rows(bok_forlag_norsk, bok_forlag_ej_norsk)
forlag[is.na(forlag)] <- 0L

bok_serie <- bok_5 %>%
  mutate(nsd_index_print = match(SeriesISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(SeriesEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

bok_serie_norsk <- bok_serie %>%
  filter(nsd_row > 0)

bok_serie_norsk <- bok_serie_norsk %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0) %>%
  mutate(nivå = 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

byear_5 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))

bok_alla <- bind_rows(byear_1, byear_2, byear_3, byear_4, byear_5)

index_bok <- bok_alla %>%
  group_by(nivåMax) %>% 
  count(Year) %>%
  spread(Year, n)
index_bok ["Indikator"] <- "Nivå norska listan"
index_bok <- index_bok %>%
  unite(nivåMax, Indikator, nivåMax, sep=": ") %>%
  rename(Index = nivåMax)

index_bok[is.na(index_bok)] <- 0L

write_csv(bok_alla, "Övriga_norska.csv")
write_csv(index_bok, "Övriga_index.csv")

art_alla <- art_alla %>% rename(nivåMax = nivå)
publ_norska <- bind_rows(art_alla, bok_alla)
write_csv(publ_norska, "Medräknade publikationer.csv")


# Författare --------------------------------------------------------------

author <- diva_author %>% 
  filter(between(Year, year1, year5)) %>%
  filter(PublicationType=="Artikel i tidskrift"|PublicationType=="Artikel, forskningsöversikt"
         |PublicationType=="Kapitel i bok, del av antologi"|PublicationType=="Bok"|PublicationType=="Konferensbidrag")%>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter((is.na(Status))|Status=="published"|Status=="epub ahead of print")%>%
  filter(is.na(PublicationSubtype)|PublicationSubtype == "publishedPaper")%>%
  select(PID, Name.x, Id, OrganisationIds, Title, PublicationType, ContentType, Year)

author$PublicationType <- recode(author$PublicationType,
                                   "Artikel i tidskrift" = "Artikel",
                                   "Artikel, forskningsöversikt" = "Artikel",
                                   "Kapitel i bok, del av antologi" = "Kapitel",
                                   "Bok" = "Monografi",
                                   "Konferensbidrag" = "Publicerat konferensbidrag")

author_subject <- author %>% filter_orgs_author(foretagsekonomi)

#author_subject_2 <- author %>% filter(between(Year, year1, year5)) %>% filter_orgs_author(subject_unit_2)

#author_subject <- bind_rows(author_subject, author_subject_2)
author_subject <- select(author_subject, -(list_of_orgs))
write_csv(author_subject, "Publikationer per föfattare.csv")


# Avslut ------------------------------------------------------------------

sh_archive_resource("Tabell publikationer.csv")
sh_archive_resource("Artiklar_index.csv")
sh_archive_resource("Övriga_index.csv")
sh_archive_resource("Medräknade publikationer.csv")
sh_archive_resource("Publikationer per föfattare.csv")
sh_archive_df(diva, "Diva_rådata.csv")
sh_archive_end()

