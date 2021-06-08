#
#
#
#cleanup_diva
#170921 GL
#Olika skript som sammanställer poster som behöver åtgärdas i DiVA. 
#Innehåller: Ahead of print, Ej granskningsmärkta, Manuskript i sammanläggningsavhandlingar, Sidor saknas i kapitel, 
#Orcid saknas för enstaka poster
#
#
#

library(tidyverse)
library(stringr)

#Läs in data från DiVA. Vi använder en csvall2-fil.
diva <- read_csv(file = "/home/shub/assets/diva/diva_researchpubl_latest.csv")

#Läs in en csv2
author <- read_csv("/home/shub/assets/diva/diva_author_sh_latest.csv")

#PID blir felaktigt format vid inläsning, därför:
colnames(author)[1] <- "PID"


# Ahead of print ----------------------------------------------------------
#För att använda Rmarkdown behöver även filinläsning och biblioteken klistras in.

#Uraval av publikationer efter status
ahead_of_print <- diva %>%
  subset(Status == "aheadofprint") %>%
  select(PID, DOI) %>%
  transmute(PID, link = str_c("https://doi.org/", DOI))

sort(ahead_of_print$PID)

# Ej granskningsmärkta ----------------------------------------------------
#Poster som är markerade som ej granskade, trots att de inte ligger i granskningen.
#Ger en csv-fil med PID och titel.
not_reviewed <- diva %>% filter(Reviewed == "false")
not_reviewed <- not_reviewed %>% select(PID, Title)
write_csv(not_reviewed, "Ej granskningsmärkta")


# Manuskript i sammanläggningsavhandlingar --------------------------------
#Ger en csv-fil

#Välj första publikationsår för avhandlingarna
thesis_year <- 2015

manuscript <- diva %>%
  filter(PublicationType == "Manuskript (preprint)") %>%
  select(PID, Name, Title, PublicationType, PartOfThesis)
thesis <- diva %>%
  filter(PublicationType == "Doktorsavhandling, sammanläggning", Year >= thesis_year) %>%
  select(Year, NBN)

manus_to_check <- manuscript %>%
  inner_join(thesis, c("PartOfThesis" = "NBN")) %>%
  arrange(desc(Year)) %>%
  write_csv("Manuskript i avhandlingar")


# Sidor saknas i kapitel --------------------------------------------------
#Ger en csv.fil med kapitel som saknar både start- och slutsida.

kapitel <- diva %>%
  filter(PublicationType == "Kapitel i bok, del av antologi") %>%
  filter((is.na(StartPage) & (is.na(EndPage)))) %>%
  select(PID, Title, HostPublication, Year, Publisher, ISBN) %>%
  write_csv("Kapitel utan sidor")


# Orcid saknas för enstaka poster ------------------------------
#Enstaka poster som saknar orcid, där lokaltID annars har det. Ger en csv.fil.

id_all <- author %>%
  arrange(Id) %>%
  select(Id, Orcid, PID)

id_master <- id_all %>%
  filter(!(is.na(Orcid))) %>%
  distinct(Id, Orcid)

missing_orcid <- id_all %>%
  filter(!(is.na(Id))) %>%
  filter(is.na(Orcid)) %>%
  inner_join(id_master, "Id") %>%
  write_csv("Uppdatera orcid")

