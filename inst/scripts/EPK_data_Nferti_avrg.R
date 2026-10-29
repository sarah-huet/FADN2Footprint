# Set up ----

library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringi)
library(stringr)
library(tidyverse)

# EPK 2017 ----

#tmp_epk_2017 <- read_csv2("data_CASD/PC_PKGC_2017/PKGC2017_Gen_20200128.csv")

#tmp_epk_FumOrg <- read_csv2("data_CASD/PC_PKGC_2017/PKGC2017_FumOrg_20190708.csv") %>%
  #select(NOM_DOSSIER,COEF2,BIO,SAU,LIB_ESPECE,LIB_ESPECE_CODE,FUMOPROV,QNORGCALC) %>%
 # mutate(COEF2 = as.numeric(str_replace(COEF2,",",".")) )

#tmp_epk_Pr_Expl <- read_csv2("data_CASD/PC_PKGC_2017/PKGC2017_Prairies_Exploitation_20201103.csv") %>%
 # mutate(NOM_DOSSIER = as.character(NOM_DOSSIER))
# in this table : NOM_DOSSIER = id farm

load("~/BiodivLabel/data_CASD/EPK_2017.RData")

# mean by crop & production mode

tmp_EPK_Nferti <- tmp_epk_2017 %>%
  select(NOM_DOSSIER,COEF2,BIO,LIB_ESPECE,FUMO,QNORGTOT,FUMQNTOT) %>%
  # remove missing values
  filter(!is.na(COEF2)) %>%
  # remove incoherent values
  ## manure without N
  filter(!(FUMO == 1 & QNORGTOT == 0)) %>%
  ## organic with mineral fertilizers
  filter(!(BIO == 1 & FUMQNTOT >0)) %>%
  ## wrangle crop names to remove accented characters, as their is a lot of those in french, and spaces
  mutate(
    EPK_ferti_crop_name = stri_replace_all_regex(
      LIB_ESPECE,
      pattern = c("é","è","ê","à","ï"," "),
      replacement = c("e","e","e","a","i","_"),
      vectorize = F)) %>%
  # summarize N fertilizer inputs
  group_by(EPK_ferti_crop_name,BIO) %>%
  summarise(
    n = length(unique(NOM_DOSSIER)),

    n_max_p100 = max((QNORGTOT*COEF2)/sum(COEF2))/weighted.mean(QNORGTOT,COEF2),

    N_org = weighted.mean(QNORGTOT,COEF2,na.rm = T),
    N_min = weighted.mean(FUMQNTOT,COEF2,na.rm = T),
    .groups = "keep") %>%
  ungroup()

write.csv(tmp_EPK_Nferti %>% filter(n >= 3 & n_max_p100 <= 0.8) %>% select(!n_max_p100),paste0("~/BiodivLabel/data_out/PKGC_N_ferti_",Sys.Date(),".csv"),quote = F,row.names = T)
