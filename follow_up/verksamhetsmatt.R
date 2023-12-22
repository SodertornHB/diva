#
# Verksamhetsmått till högskolans ledning
# 220317 GL / 231222 GL
# Uppföljning av publikationer INTE samma urval som ÅR. 
# Går det att få in instnamn genom att hämta värde?
#
#


library(tidyverse)
source('/home/shub/src/common/lib/sh_parameters.R')
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')

sh_archive_start("Verksamhetsmått")

diva <- read_csv(file="/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>% filter(between(Year, 2019, 2023))

diva_urval <- diva %>% 
  filter(!(PublicationType == "Övrigt" |PublicationType == "Manuskript")) %>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter((is.na(Status))|Status=="published"|Status=="inPress") %>%
  filter(is.na(PublicationSubtype)|PublicationSubtype == "publishedPaper" |PublicationSubtype == "meetingAbstract" 
         |PublicationSubtype == "editorialMaterial"|PublicationSubtype == "letter")

diva_urval$PublicationType <- recode(diva_urval$PublicationType,
                                 "Artikel i tidskrift" = "Artikel",
                                 "Artikel, forskningsöversikt" = "Artikel",
                                 "Artikel, recension" = "Artikel",
                                 "Kapitel i bok, del av antologi" = "Kapitel",
                                 "Doktorsavhandling, monografi" = "Doktorsavhandlingar",
                                 "Doktorsavhandling, sammanläggning" = "Doktorsavhandlingar",
                                 "Licentiatavhandling, monografi" = "Licentiatavhandlingar",
                                 "Licentiatavhandling, sammanläggning" = "Licentiatavhandlingar",
                                 "Konferensbidrag" = "Publicerade konferenspaper")


articles <- diva_urval %>%
  subset(PublicationType=="Artikel"|PublicationType=="Kapitel") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- diva_urval %>%
  subset(!(PublicationType=="Artikel"|PublicationType=="Kapitel"))%>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

tabell_urval <- bind_rows(articles, other_publication)
tabell_urval[is.na(tabell_urval)] <- 0L

ahead <- diva %>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter(Status=="aheadofprint")

tabell_ahead <- ahead %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

tabell_ahead[is.na(tabell_ahead)] <- 0L

write_csv(diva_urval, "sh_publikationer.csv")
write_csv(tabell_urval, "sh_publikationer_tabell.csv")
write_csv(tabell_ahead, "sh_artiklar_ahead.csv")

# ISV ---------------------------------------------------------------------
inst <- diva_urval %>% filter_orgs(sam)

articles <- inst %>%
  subset(PublicationType=="Artikel"|PublicationType=="Kapitel") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- inst %>%
  subset(!(PublicationType=="Artikel"|PublicationType=="Kapitel"))%>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

tabell_inst <- bind_rows(articles, other_publication)%>%
  rename(ISV = PublicationType)
tabell_inst[is.na(tabell_inst)] <- 0L

tabell_inst_ahead <- ahead %>%
  filter_orgs(sam) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  rename("ISV Ahead of print" = PublicationType)

tabell_inst_ahead[is.na(tabell_inst_ahead)] <- 0L

#Spara filer
inst <- select(inst, -(list_of_orgs))
write_csv(inst, "isv_publikationer.csv" )
write_csv(tabell_inst, "isv_publikationer_tabell.csv")
write_csv(tabell_inst_ahead, "isv_artiklar_ahead.csv")

# IKL ---------------------------------------------------------------------
inst <- diva_urval %>% filter_orgs(ikl)

articles <- inst %>%
  subset(PublicationType=="Artikel"|PublicationType=="Kapitel") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- inst %>%
  subset(!(PublicationType=="Artikel"|PublicationType=="Kapitel"))%>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

tabell_inst <- bind_rows(articles, other_publication)%>%
  rename(IKL = PublicationType)
tabell_inst[is.na(tabell_inst)] <- 0L

tabell_inst_ahead <- ahead %>%
  filter_orgs(ikl) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  rename("IKL Ahead of print" = PublicationType)

tabell_inst_ahead[is.na(tabell_inst_ahead)] <- 0L

#Spara filer
inst <- select(inst, -(list_of_orgs))
write_csv(inst, "ikl_publikationer.csv" )
write_csv(tabell_inst, "ikl_publikationer_tabell.csv")
write_csv(tabell_inst_ahead, "ikl_artiklar_ahead.csv")

# IHS ---------------------------------------------------------------------
inst <- diva_urval %>% filter_orgs(hs)

articles <- inst %>%
  subset(PublicationType=="Artikel"|PublicationType=="Kapitel") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- inst %>%
  subset(!(PublicationType=="Artikel"|PublicationType=="Kapitel"))%>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

tabell_inst <- bind_rows(articles, other_publication)%>%
  rename(IHS = PublicationType)
tabell_inst[is.na(tabell_inst)] <- 0L

tabell_inst_ahead <- ahead %>%
  filter_orgs(hs) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  rename("IHS Ahead of print" = PublicationType)

tabell_inst_ahead[is.na(tabell_inst_ahead)] <- 0L

#Spara filer
inst <- select(inst, -(list_of_orgs))
write_csv(inst, "ihs_publikationer.csv" )
write_csv(tabell_inst, "ihs_publikationer_tabell.csv")
write_csv(tabell_inst_ahead, "ihs_artiklar_ahead.csv")


# NMT ---------------------------------------------------------------------

inst <- diva_urval %>% filter_orgs(nmt)

articles <- inst %>%
  subset(PublicationType=="Artikel"|PublicationType=="Kapitel") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- inst %>%
  subset(!(PublicationType=="Artikel"|PublicationType=="Kapitel"))%>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

tabell_inst <- bind_rows(articles, other_publication)%>%
  rename(NMT = PublicationType)
tabell_inst[is.na(tabell_inst)] <- 0L

tabell_inst_ahead <- ahead %>%
  filter_orgs(nmt) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  rename("NMT Ahead of print" = PublicationType)

tabell_inst_ahead[is.na(tabell_inst_ahead)] <- 0L

#Spara filer
inst <- select(inst, -(list_of_orgs))
write_csv(inst, "nmt_publikationer.csv" )
write_csv(tabell_inst, "nmt_publikationer_tabell.csv")
write_csv(tabell_inst_ahead, "nmt_artiklar_ahead.csv")


# IPA -------------------------------------------------------------------

inst <- diva_urval %>% filter_orgs(polis)

articles <- inst %>%
  subset(PublicationType=="Artikel"|PublicationType=="Kapitel") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- inst %>%
  subset(!(PublicationType=="Artikel"|PublicationType=="Kapitel"))%>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

tabell_inst <- bind_rows(articles, other_publication)%>%
  rename(IPA = PublicationType)
tabell_inst[is.na(tabell_inst)] <- 0L

tabell_inst_ahead <- ahead %>%
  filter_orgs(polis) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  rename("IPA Ahead of print"= PublicationType)

tabell_inst_ahead[is.na(tabell_inst_ahead)] <- 0L

#Spara filer
inst <- select(inst, -(list_of_orgs))
write_csv(inst, "ipa_publikationer.csv" )
write_csv(tabell_inst, "ipa_publikationer_tabell.csv")
write_csv(tabell_inst_ahead, "ipa_artiklar_ahead.csv")

# Lararinst ---------------------------------------------------------------

inst <- diva_urval %>% filter_orgs(lararinst)

articles <- inst %>%
  subset(PublicationType=="Artikel"|PublicationType=="Kapitel") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- inst %>%
  subset(!(PublicationType=="Artikel"|PublicationType=="Kapitel"))%>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

tabell_inst <- bind_rows(articles, other_publication)%>%
  rename("Lärarinst" = PublicationType)
tabell_inst[is.na(tabell_inst)] <- 0L

tabell_inst_ahead <- ahead %>%
  filter_orgs(lararinst) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  rename("Lärarinst Ahead of print" = PublicationType)

tabell_inst_ahead[is.na(tabell_inst_ahead)] <- 0L

#Spara filer
inst <- select(inst, -(list_of_orgs))
write_csv(inst, "lararinst_publikationer.csv" )
write_csv(tabell_inst, "lararinst_publikationer_tabell.csv")
write_csv(tabell_inst_ahead, "lararinst_artiklar_ahead.csv")


# Avslut ------------------------------------------------------------------
sh_archive_df(diva,"diva_rådata")
sh_archive_resource("sh_publikationer.csv")
sh_archive_resource("sh_publikationer_tabell.csv")
sh_archive_resource("sh_artiklar_ahead.csv")

sh_archive_resource("isv_publikationer.csv")
sh_archive_resource("isv_publikationer_tabell.csv")
sh_archive_resource("isv_artiklar_ahead.csv")

sh_archive_resource("ikl_publikationer.csv")
sh_archive_resource("ikl_publikationer_tabell.csv")
sh_archive_resource("ikl_artiklar_ahead.csv")

sh_archive_resource("ihs_publikationer.csv")
sh_archive_resource("ihs_publikationer_tabell.csv")
sh_archive_resource("ihs_artiklar_ahead.csv")

sh_archive_resource("nmt_publikationer.csv")
sh_archive_resource("nmt_publikationer_tabell.csv")
sh_archive_resource("nmt_artiklar_ahead.csv")

sh_archive_resource("ipa_publikationer.csv")
sh_archive_resource("ipa_publikationer_tabell.csv")
sh_archive_resource("ipa_artiklar_ahead.csv")

sh_archive_resource("lararinst_publikationer.csv" )
sh_archive_resource("lararinst_publikationer_tabell.csv")
sh_archive_resource("lararinst_artiklar_ahead.csv")

sh_archive_end()
