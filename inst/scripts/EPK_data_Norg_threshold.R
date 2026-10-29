# Set up ----

library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringi)
library(stringr)
library(tidyverse)

# EPK 2017 FumOrg ----

#tmp_epk_2017 <- read_csv2("data_CASD/PC_PKGC_2017/PKGC2017_Gen_20200128.csv")

#tmp_epk_FumOrg <- read_csv2("data_CASD/PC_PKGC_2017/PKGC2017_FumOrg_20190708.csv") %>%
  #select(NOM_DOSSIER,COEF2,BIO,SAU,LIB_ESPECE,LIB_ESPECE_CODE,FUMOPROV,QNORGCALC) %>%
 # mutate(COEF2 = as.numeric(str_replace(COEF2,",",".")) )

#tmp_epk_Pr_Expl <- read_csv2("data_CASD/PC_PKGC_2017/PKGC2017_Prairies_Exploitation_20201103.csv") %>%
 # mutate(NOM_DOSSIER = as.character(NOM_DOSSIER))
# in this table : NOM_DOSSIER = id farm

load("~/BiodivLabel/data_CASD/EPK_2017.RData")

# threshold per crop

## which farm use only manure produced on-site
tmp_FumOrg <- tmp_epk_FumOrg %>%
  group_by(NOM_DOSSIER) %>%
  summarise(
    FUMOPROV = str_c(unique(sort(FUMOPROV)),collapse = ";"),
    QNORGCALC = sum(QNORGCALC)
  )

## mean & threshold by crop

tmp_EPK_Norg_all <- tmp_epk_2017 %>%
  select(NOM_DOSSIER,COEF2,BIO,LIB_ESPECE,FUMO,QNORGTOT,FUMQNTOT) %>%
  # add manure origin
  mutate(
    Norg_int = case_when(
      NOM_DOSSIER %in% tmp_FumOrg$NOM_DOSSIER[tmp_FumOrg$FUMOPROV == 1] ~ T,
      .default = F
    )
  ) %>%
  # remove missing values
  filter(!is.na(COEF2)) %>%
  # remove incoherent values
  ## manure without N
  filter(!(FUMO == 1 & QNORGTOT == 0)) %>%
  ## organic with mineral fertilizers
  filter(!(BIO == 1 & FUMQNTOT >0)) %>%
  # match categories
  mutate(
    EPK_ferti_crop_name = stri_replace_all_regex(
      LIB_ESPECE,
      pattern = c("é","è","ê","à","ï"," "),
      replacement = c("e","e","e","a","i","_"),
      vectorize = F)
  ) %>%
  # summarize N fertilizer inputs
  group_by(EPK_ferti_crop_name,BIO,Norg_int) %>%
  summarise(
    n = length(unique(NOM_DOSSIER)),
    n_max_p100 = max((QNORGTOT*COEF2)/sum(COEF2))/weighted.mean(QNORGTOT,COEF2),

    N_org = weighted.mean(QNORGTOT,COEF2),
    Norg_sd_w = sqrt(sum(COEF2 * ((QNORGTOT - N_org)^2), na.rm = T)/
                        (sum(COEF2, na.rm = T)-1)),
    Norg_se_w =
      sqrt(
        ( ( (sum(COEF2*QNORGTOT^2,na.rm = T)) / (sum(COEF2,na.rm = T)) ) -  (N_org^2) )
        *
          ( (sum(COEF2^2,na.rm = T)) / ( (sum(COEF2,na.rm = T)^2) - (sum(COEF2^2,na.rm = T)) ) )),
    Norg_CI95 = Norg_se_w * 1.96,
    Norg_thrhld_min = N_org - Norg_CI95,
    Norg_thrhld_max = N_org + Norg_CI95,
    .groups = "keep"
    ) %>%
  # quality check : pour ici, quand Norg_thrhld_min < 0, je mets zero
  mutate(
    Norg_thrhld_min = case_when(
      Norg_thrhld_min < 0 ~ 0,
      .default = Norg_thrhld_min
    )
  )

write.csv(tmp_EPK_Norg_all %>% filter(n >= 3 & n_max_p100 <= 0.8) %>% select(!n_max_p100),
          paste0("~/BiodivLabel/data_out/PKGC_N_ferti_org_",Sys.Date(),".csv"),quote = F,row.names = T)


tmp_EPK_Norg <- tmp_EPK_Norg_all %>%
  # define min by crop and production mode as threshold
  group_by(EPK_ferti_crop_name,BIO) %>%
  summarise(
    n = sum(n),
    n_max_p100 = max(n_max_p100),

    Norg_thrhld_min = min(Norg_thrhld_min,na.rm = T),
    Norg_thrhld_max = max(Norg_thrhld_max,na.rm = T),
    .groups = "keep") %>% ungroup()

write.csv(tmp_EPK_Norg %>% filter(n >= 3 & n_max_p100 <= 0.8) %>% select(!n_max_p100),
          paste0("~/BiodivLabel/data_out/PKGC_N_ferti_org_threshold_",Sys.Date(),".csv"),quote = F,row.names = T)

