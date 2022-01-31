#
# oss_annualreport
# 220121 GL
# Återrapportering till ÖSS 
#


library(tidyverse)
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')
sh_archive_start("ÖSS")

#Välj år även rad 142
y <- 2021

#Läs in data från DiVA. Vi använder en csvall2-fil
diva <- read_csv(file="/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>% filter(Year == y)

diva <- subject_baltic(diva)
diva <- funder_oss(diva)

diva$JournalISSN[is.na(diva$JournalISSN)] <- 0L
diva$JournalEISSN[is.na(diva$JournalEISSN)] <- 0L
diva$SeriesISSN[is.na(diva$SeriesISSN)] <- 0L
diva$SeriesEISSN[is.na(diva$SeriesEISSN)] <- 0L

#Norska listan
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

#Filtrerar till Östersjöforskning
ossforsk <- diva %>% filter(baltic == TRUE)

#De publikationer som skall ingå i sammanställningen

ossforskAR <- ossforsk %>%
  filter(!(PublicationType == "Övrigt")) %>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter((is.na(Status))|Status=="published"|Status=="inPress") %>%
  filter(is.na(PublicationSubtype)|PublicationSubtype == "publishedPaper" |PublicationSubtype == "meetingAbstract" 
         |PublicationSubtype == "editorialMaterial")%>%
  mutate(nsd = ((JournalISSN %in% n_issn$`Print.ISSN`)|(JournalEISSN %in% n_issn$`Online.ISSN`)))

ahead <- nrow(subset(ossforsk, Status == "aheadofprint")) 

#Slå ihop publikationstyper genom att byta namn på värden
ossforskAR$PublicationType <- recode(ossforskAR$PublicationType,
                                     "Artikel i tidskrift" = "Artikel",
                                     "Artikel, forskningsöversikt" = "Artikel",
                                     "Artikel, recension" = "Artikel, recension",
                                     "Kapitel i bok, del av antologi" = "Kapitel",
                                     "Bok" = "Monografi",
                                     "Doktorsavhandling, monografi" = "Doktorsavhandling",
                                     "Doktorsavhandling, sammanläggning" = "Doktorsavhandling",
                                     "Konferensbidrag" = "Publicerat konferensbidrag",
                                     "Licentiatavhandling, monografi" = "Licentiatavhandling",
                                     "Licentiatavhandling, sammanläggning" = "Licentiatavhandling",
                                     "Rapport" = "Rapport",
                                     "Samlingsverk (redaktörskap)" = "Samlingsverk/proceedings, redaktörskap",
                                     "Proceedings (redaktörskap)" = "Samlingsverk/proceedings, redaktörskap")

ossforskAR$PublicationType <- factor(ossforskAR$PublicationType, ordered = TRUE, 
                                     levels = c("Artikel", "Artikel, recension", "Monografi", "Kapitel", 
                                                "Publicerat konferensbidrag", "Doktorsavhandling", "Licentiatavhandling",
                                                "Rapport", "Samlingsverk/proceedings, redaktörskap"))
ossforskAR$ContentType <- factor(ossforskAR$ContentType, ordered = TRUE,
                                 levels = c("Refereegranskat", "Övrigt vetenskapligt"))

oss_table <- ossforskAR %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

oss_ref <- ossforskAR %>%
  filter(ContentType == "Refereegranskat") %>%
  group_by(PublicationType, ContentType) %>%
  count(Year) %>%
  spread(Year, n)

oss_ref <- oss_ref %>%
  spread(ContentType, '2021')

oss_table <- left_join(oss_table, oss_ref, "PublicationType")

oss_table[is.na(oss_table)] <- 0L


# OSS funding -------------------------------------------------------------

oss_fund <- ossforskAR %>% filter(oss == TRUE)

oss_fund_table <- oss_fund %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

oss_fund_ref <- oss_fund %>%
  filter(ContentType == "Refereegranskat") %>%
  group_by(PublicationType, ContentType) %>%
  count(Year) %>%
  spread(Year, n)

oss_fund_ref <- oss_fund_ref %>%
  spread(ContentType, '2021')

oss_fund_table <- left_join(oss_fund_table, oss_fund_ref, "PublicationType")

oss_fund_table[is.na(oss_fund_table)] <- 0L


# Norska listan -----------------------------------------------------------

#Artiklar norska listan
art <- ossforskAR %>%
  filter(PublicationType == "Artikel" |PublicationType == "Artikel, recension") %>%
  select(PID, Name, Title, Journal, JournalISSN, JournalEISSN, Year, Publisher, ContentType, oss, baltic)

art <- art %>%
  mutate(nsd_index_print = match(JournalISSN, n_issn$Print.ISSN, nomatch = 0)) %>%
  mutate(nsd_index_e = match(JournalEISSN, n_issn$Online.ISSN, nomatch = 0)) %>%
  mutate(nsd_row = pmax(nsd_index_print, nsd_index_e))

nsd_kol <- str_c("Nivå.", y) 

art_norsk <- art %>%
  filter(nsd_row > 0) %>%
  #rowwise behövs för att indexeringen i nästa rad ska fungera:
  rowwise() %>%
  mutate(nivå = n_issn[[nsd_kol]][[nsd_row]]) %>%
  #för att ta bort rowwise:
  ungroup()

art_ej_norsk <- art %>%
  filter(nsd_row == 0)

art_alla <- bind_rows(art_norsk, art_ej_norsk)

#Övriga publikationer norska listan
bok <- ossforskAR %>%
  filter(PublicationType == "Monografi"| PublicationType == "Kapitel"|PublicationType == "Publicerat konferensbidrag") %>% 
  select(PID, Name, Title, Publisher, Year, Series, SeriesISSN, SeriesEISSN, PublicationType, ContentType, oss, baltic)

bok <- bok %>%
  mutate(nsd_row = match(Publisher, n_forlag$Original.tittel, nomatch = 0))

nsd_kol <- str_c("Nivå.", y)   


bok_forlag_norsk <- bok %>%
  filter(nsd_row > 0) %>%
  rowwise() %>%
  mutate(nivå = n_forlag[[nsd_kol]][[nsd_row]]) %>%
  ungroup()

bok_forlag_ej_norsk <- bok %>%
  filter(nsd_row == 0)

forlag <- bind_rows(bok_forlag_norsk, bok_forlag_ej_norsk)
#forlag[is.na(forlag)] <- 0L

bok_serie <- bok %>%
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
#serie[is.na(serie)] <- 0L

bok_alla <- forlag %>%
  mutate(rowSerie = serie$nsd_row[match(PID, serie$PID)]) %>%
  mutate(nivåSerie = serie$nivå[match(PID, serie$PID)])%>%
  mutate(nivåMax = (pmax(nivå, nivåSerie)))


# Documentation -----------------------------------------------------------

write_csv(oss_table, "Östersjöforskning.csv")
write_csv(oss_fund_table, "Östersjöstiftelsen.csv")
sh_archive_resource("Östersjöforskning.csv")
sh_archive_resource("Östersjöstiftelsen.csv")
sh_archive_df(art_alla, "Artiklar_norska")
sh_archive_df(bok_alla, "Övriga_norska")
sh_archive_df(ossforskAR, "Medräknade_publikationer")
sh_archive_df(diva, "Diva_rådata")
sh_archive_end()
