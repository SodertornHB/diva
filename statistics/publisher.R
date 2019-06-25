#
#Publisher
#190315 GL
#Publikationer från SHforskare från ett specifikt förlag.
#
#


library(tidyverse)

diva <- read_csv("/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")

#Ladda fin med ISSN för förlaget
f_issn <- read_excel("Frontiers.xlsx")

artiklar <- diva %>%
  filter(PublicationType == "Artikel i tidskrift"|PublicationType == "Artikel, forskningsöversikt") %>%
  select(PID, Name, Title, Journal, JournalISSN, JournalEISSN, Year)

f_artiklar <- artiklar %>%
  mutate(forlaget = match(JournalISSN, f_issn$ISSN, nomatch = 0))%>%
  filter(forlaget != 0)

write_csv(f_artiklar, "Förlagets artiklar")


#Böcker och kapitel
diva <- mutate(diva, Pbl = ifelse(grepl("Gruyter", diva$Publisher), T, F))

f_bocker <- diva %>%
  filter(PublicationType == "Samlingsverk (redaktörskap)"|PublicationType == "Bok") %>%
  select(PID, Name, Title, Publisher, Year, ISBN, DOI, Pbl) %>%
  filter(Pbl==T)

f_kapitel <- diva %>%
  filter(PublicationType == "Kapitel i bok, del av antologi") %>%
  select(PID, Name, Title, Publisher, Year, ISBN, DOI, Pbl) %>%
  filter(Pbl==T)

write_csv(f_bocker, "Förlagets böcker")
write_csv(f_kapitel, "Förlagets kapitel")
