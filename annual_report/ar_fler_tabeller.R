#
# ar_fler_tabeller
# GL 211012
# 
# Statistik med DiVA som källa. Samma urval som till ÅR.
# Tabeller med antal publikationer, indexerade artiklar och OA.
# Behöver förbättras där NA ersätts med OL.
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
                   encoding = "utf8")

n_forlag <- read.csv(file="/home/shub/assets/nsd.forlag.csv",
         header=TRUE,
         sep=";",
         na.strings = c("", "NA"),
         stringsAsFactors = FALSE,
         encoding = "utf8")
n_forlag$Original.tittel <- recode(n_forlag$Original.tittel, "Södertörns Högskola" = "Södertörns högskola")

sh_archive_start("SH_underlag")

diva <- read_csv(file="/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>% filter(between(Year, 2016, 2020))

diva$JournalISSN[is.na(diva$JournalISSN)] <- 0L
diva$JournalEISSN[is.na(diva$JournalEISSN)] <- 0L
diva$SeriesISSN[is.na(diva$SeriesISSN)] <- 0L
diva$SeriesEISSN[is.na(diva$SeriesEISSN)] <- 0L


# Publikationer -----------------------------------------------------------

publ_vet <- diva %>%
  filter(!(PublicationType == "Samlingsverk (redaktörskap)"|PublicationType == "Proceedings (redaktörskap)" |PublicationType == "Övrigt")) %>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter((is.na(Status))|Status=="published"|Status=="inPress") %>%
  filter(is.na(PublicationSubtype)|PublicationSubtype == "publishedPaper" |PublicationSubtype == "meetingAbstract" 
         |PublicationSubtype == "editorialMaterial") %>%
  mutate(nsd = ((JournalISSN %in% n_issn$`Print.ISSN`)|(JournalEISSN %in% n_issn$`Online.ISSN`)))

publ_vet$PublicationType <- recode(publ_vet$PublicationType,
                                   "Artikel i tidskrift" = "Artiklar i tidskrift",
                                   "Artikel, forskningsöversikt" = "Artiklar i tidskrift",
                                   "Artikel, recension" = "Artiklar i tidskrift",
                                   "Kapitel i bok, del av antologi" = "Artiklar i antologi",
                                   "Doktorsavhandling, monografi" = "Doktorsavhandlingar",
                                   "Doktorsavhandling, sammanläggning" = "Doktorsavhandlingar",
                                   "Licentiatavhandling, monografi" = "Licentiatavhandlingar",
                                   "Licentiatavhandling, sammanläggning" = "Licentiatavhandlingar",
                                   "Bok" = "Monografier",
                                   "Konferensbidrag" = "Publicerade konferensbidrag",
                                   "Rapport" = "Rapporter")

publ_vet$PublicationType <- factor(publ_vet$PublicationType, ordered = TRUE, 
                                 levels = c("Artiklar i tidskrift", "Artiklar i antologi", "Monografier", "Publicerade konferensbidrag", 
                                            "Doktorsavhandlingar", "Licentiatavhandlingar", "Rapporter"))
publ_vet$ContentType <- factor(publ_vet$ContentType, ordered = TRUE,
                             levels = c("Refereegranskat", "Övrigt vetenskapligt"))

articles <- publ_vet %>%
  subset(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- publ_vet %>%
  subset(!(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi")) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

ar_table <- bind_rows(articles, other_publication)
ar_table[is.na(ar_table)] <- 0L

write_csv(ar_table, "Tabell årsredovisning.csv")

# DOI -----------------------------------------------------------
# Skapa en DOI-fil som går att kopiera till Scopus och WoS avancerade sökning.
doi <- publ_vet %>%
  filter(PublicationType == "Artiklar i tidskrift") %>%
  filter(!(is.na(DOI))) %>%
  transmute(scop = str_c("DOI(", DOI,") OR "))
  #transmute(wos =str_c("DO=", DOI, " OR "))

write_csv(doi, "DOI")

# Open Access kapitel -------------------------------------------------------------

options(OutDec= ",")
  
sh_kap <- publ_vet %>%
  filter(PublicationType == "Artiklar i antologi") %>%
  group_by (Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

oa_sh_kap <- publ_vet %>%
  filter(PublicationType == "Artiklar i antologi") %>%
  filter(FreeFulltext == TRUE | (!(is.na(FullTextLink)))) %>%
  count(Year)

oa_stats_kap <- full_join(sh_kap, oa_sh_kap, by = "Year")
names(oa_stats_kap) <- c("Year", "sh_kap", "sh_kap_oa")
oa_stats_kap[is.na(oa_stats_kap)] <- 0L

oa_andel_kap <- oa_stats_kap %>%
  mutate(sh_kap_andel_oa = sh_kap_oa/sh_kap) %>%
  select(Year, sh_kap_andel_oa) %>%
  arrange(Year)

write_csv(oa_stats_kap, "Antal_oa_kapitel.csv")
write_excel_csv2(oa_andel_kap, "Andel_oa_kapitel.csv")

# Open Access artikel -----------------------------------------------------

sh_art <- publ_vet %>%
  filter(PublicationType == "Artiklar i tidskrift") %>%
  group_by (Year) %>%
  count(Year) %>%
  select(Year, n) %>%
  arrange(Year)

oa_sh_art <- publ_vet %>%
  filter(PublicationType == "Artiklar i tidskrift") %>%
  filter(FreeFulltext == TRUE | (!(is.na(FullTextLink)))) %>%
  count(Year)

oa_stats_art <- full_join(sh_art, oa_sh_art, by = "Year")
names(oa_stats_art) <- c("Year", "sh_art", "sh_art_oa")

oa_andel_art <- oa_stats_art %>%
  mutate(sh_art_andel_oa = sh_art_oa/sh_art) %>%
  select(Year, sh_art_andel_oa) %>%
  arrange(Year)
  
write_csv(oa_stats_art, "Antal_oa_artiklar.csv")
write_excel_csv2(oa_andel_art, "Andel_oa_artiklar.csv")


# Norska listan -----------------------------------------------------------
# Matchning norska listan.
# nsd_kol = vektor som bestämmer vilken kolumn nivåvärdet hämtas från ur norska filerna
# Omatchade publikationer måste tas bort innan nivån hämtas. Läggs tillbaka efteråt.

year1 <- 2016
year2 <- 2017
year3 <- 2018
year4 <- 2019
year5 <- 2020


art <- publ_vet %>%
  filter(PublicationType == "Artiklar i tidskrift") %>%
  select(PID, Name, Title, Journal, JournalISSN, JournalEISSN, Year, Publisher, ContentType, ISI, ScopusId)

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
write_csv(art_alla, "Artiklar_norska.csv")
write_csv(index_art, "SH_artiklar_index.csv")

# Norska listan böcker ----------------------------------------------------
# Viktigt att förlagsnamnen stämmer mot Norska listan. Samma år som definerats under artiklar.
# Dubbla serier behöver kollas manuellt i filen.

bok <- publ_vet %>%
  filter(PublicationType == "Monografi"| PublicationType == "Artiklar i antologi"|PublicationType == "Publicerat konferensbidrag") %>% 
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
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

year_1 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))

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
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

year_2 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))

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
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

year_3 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))


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
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

year_4 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))

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
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_serie_ej_norsk <- bok_serie %>%
  filter(nsd_row == 0)

serie <- bind_rows(bok_serie_norsk, bok_serie_ej_norsk)
serie[is.na(serie)] <- 0L

year_5 <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))

bok <- bind_rows(year_1, year_2, year_3, year_4, year_5)

index_bok <- bok %>%
  group_by(nivåMax) %>% 
  count(Year) %>%
  spread(Year, n)
index_bok ["Indikator"] <- "Nivå norska listan"
index_bok <- index_bok %>%
  unite(nivåMax, Indikator, nivåMax, sep=": ") %>%
  rename(Index = nivåMax)

write_csv(bok, "Övriga_norska.csv")
write_csv(index_bok, "SH_övriga_index.csv")

# Avslut ------------------------------------------------------------------

sh_archive_resource("Tabell årsredovisning.csv")
sh_archive_resource("Artiklar_norska.csv")
sh_archive_resource("SH_artiklar_index.csv")
sh_archive_resource("Övriga_norska.csv")
sh_archive_resource("SH_övriga_index.csv")
sh_archive_resource("Antal_oa_artiklar.csv")
sh_archive_resource("Andel_oa_artiklar.csv")
sh_archive_resource("Antal_oa_kapitel.csv")
sh_archive_resource("Andel_oa_kapitel.csv")
sh_archive_df(diva, "Diva_rådata.csv")
sh_archive_df(publ_vet, "Medräknade publikationer.csv")

sh_archive_end()

