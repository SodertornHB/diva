#
#
#
#doktorandernas publikationer
#171221 CHL
#vi fick en doktorandlista av AVM. den används som indata och 
#kombineras med diva_author_latest-filen för att få fram doktoranderns publikationer.
#Observera att det leder till dubbletter om doktoranderna samförfattat.
#
#
#
library(tidyverse)
library(ggplot2)

#Läs in data:
phds <- read.csv("/lägg_filen_från_AVM_på_lämpligt_ställe.csv", encoding = "latin1")
#i det här fallet var jag tvungen att byta namn på kolumnen för att kunna göra en left_join nedan:
names(phds)[names(phds) == "ID"] <- "Id"
divapubls <- read_csv("/home/shub/assets/diva/diva_author_latest.csv")
phds_publs <- left_join(phds, divapubls, by = "Id")

#skriv ut data för vidare bearbetning i Excel:
write_csv(phds_publs, "phds_publs.csv")
