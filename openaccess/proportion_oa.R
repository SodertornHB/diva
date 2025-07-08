#
# Andel OA
# 220506 GL / 240520GL
# Underlag för att se hur mycket av SH-affilierade publikationer som publiceras OA.
# 
#
#


library(tidyverse)
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')

diva <- read_csv(file="/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>% filter(between(Year, 2024,2024))

doaj_listan <- read_csv("/home/shub/assets/doaj.csv")

diva$JournalISSN[is.na(diva$JournalISSN)] <- 0L
diva$JournalEISSN[is.na(diva$JournalEISSN)] <- 0L
diva$FreeFulltext[diva$FreeFulltext == "true"] <- TRUE

diva_urval <- diva %>% 
  filter((PublicationType == "Artikel i tidskrift"|PublicationType =="Artikel, forskningsöversikt"|
            PublicationType == "Kapitel i bok, del av antologi"|PublicationType == "Bok"|
            PublicationType == "Samlingsverk (redaktörskap)")) %>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter((is.na(Status))|Status=="published"|Status=="inPress"|Status=="aheadofprint")

diva_confpaper <- diva %>%
  filter(PublicationType == "Konferensbidrag")%>%
  filter(ContentType != "Övrig (populärvetenskap, debatt, mm)")%>%
  filter(PublicationSubtype == "publishedPaper")

diva_urval <- bind_rows(diva_urval, diva_confpaper)

# DOI ---------------------------------------------------------------------
# Ta ut lista från Unpaywall för att matcha i excel

doi_unpay_alla <- diva_urval %>%
  filter(!(is.na(DOI))) %>%
  select(DOI)

write_csv(doi_unpay_alla, "doi.csv")

# Urval kolumner --------------------------------------------------------------------

underlag <- diva_urval %>%
  mutate(doaj = ((JournalISSN %in% doaj_listan$`Journal ISSN (print version)`)|
                 (JournalEISSN %in% doaj_listan$`Journal EISSN (online version)`)))%>%
  select(PID, Year, Status, DOI, Publisher, PublicationType, ContentType, PublicationSubtype, FreeFulltext,
         Urls, FullTextLink, doaj, Title, Journal, JournalISSN, JournalEISSN, HostPublication, ISBN, Funder)

write_csv(underlag, "underlag.csv")

