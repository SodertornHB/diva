#
# Underlag Open APC Sweden
# 200710 GL
# Artiklar där författare affilierade till vårt lärosäte är första eller sista författare.
# Hjälp för sökning efter felkonterade APC i ekonomisystemet. 

library(tidyverse)

source('/home/shub/src/common/lib/sh_parameters.R')
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')

#Läs in data från DiVA. Vi använder en bearbetad csv2-fil.
diva <- read_csv(file = "/home/shub/assets/diva/diva_author_latest.csv")
#Tidskriftslista nerladdad från doaj.org
doaj_listan <- read_csv("/home/shub/assets/doaj.csv")

diva$JournalISSN[is.na(diva$JournalISSN)] <- 0L
diva$JournalEISSN[is.na(diva$JournalEISSN)] <- 0L
diva$FreeFulltext[diva$FreeFulltext == "true"] <- TRUE

diva_art <- diva %>%
  filter(between(Year, 2017, 2018)) %>%
  filter(PublicationType %in% c("Artikel, forskningsöversikt", "Artikel i tidskrift")) %>%
  filter(ContentType != "Övrig (populärvetenskap, debatt, mm)") %>%
  mutate(doaj = ((JournalISSN %in% doaj_listan$`Journal ISSN (print version)`)|
                   (JournalEISSN %in% doaj_listan$`Journal EISSN (online version)`)))%>%
  select(PID, Name.x, OrganisationIds, Position, NumberOfAuthors, Title, Publisher, Journal, JournalISSN, JournalEISSN,
         Year, ContentType, PublicationSubtype, Status, FullTextLink, DOI, Urls, FreeFulltext, doaj)

diva_corresponding <- diva_art %>%
  filter(Position == "1"| Position == NumberOfAuthors) %>%
  filter(!(is.na(OrganisationIds)))

write_csv(diva_corresponding, "Artiklar_corresponding.csv")
