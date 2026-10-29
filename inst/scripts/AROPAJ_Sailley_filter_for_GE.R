# AROPAJ total dry matter per livestock category -------------------------------------------------------------------------------------------------------
## AROPAJ
AROPAJ_ref_ration <- data_extra$AROPAJ_France


# Sailley total dry matter per crop -------------------------------------------------------------------------------------------------------
## From Sailley et al. 2021, INRAe Prod. Anim.
# TODO: how to generalize this to all FADN countries?
sailley_livestock <- intersect(
    colnames(data_extra$Sailley_2021_feed_flows),
    unique(data_extra$livestock$Sailley_livestock)
)

Sailley_ref_feed_allocation <- data_extra$Sailley_2021_feed_flows |>
    dplyr::select(
        Sailley_feed,
        FADN_code_feed,
        dplyr::all_of(sailley_livestock)
    ) |>
    tidyr::pivot_longer(
        cols = !c(Sailley_feed, FADN_code_feed),
        names_to = "Sailley_livestock",
        values_to = "raw_flow"
    ) |>
    # estimate relative weights for each feed sub category (see data_extra$Sailley_2021_feed_flows)
    dplyr::left_join(
        data_extra$Sailley_2021_feed_flows |>
            dplyr::select(Sailley_feed, dplyr::all_of(sailley_livestock)) |>
            tidyr::pivot_longer(
                cols = !Sailley_feed,
                names_to = "Sailley_livestock",
                values_to = "raw_flow"
            ) |>
            tidyr::pivot_wider(
                id_cols = Sailley_livestock,
                names_from = Sailley_feed,
                values_from = raw_flow
            ) |>
            dplyr::mutate(
                Total_matieres_premieres_concentrees = 0,
                Grains = 0,
                Cereales = Cereales -
                    (Dont_ble_tendre + Dont_mais_grain + Dont_orge),
                Coproduits = 0,
                Tourteaux = Tourteaux - Dont_tourteau_de_soja,
                Total_Fourrages = 0
            ) |>
            tidyr::pivot_longer(
                cols = !Sailley_livestock,
                names_to = "Sailley_feed",
                values_to = "rel_weight"
            ),
        by = c('Sailley_feed', 'Sailley_livestock')
    ) |>
    # estimate relative weight percent per feed category
    dplyr::left_join(
        data_extra$Sailley_2021_feed_flows |>
            dplyr::select(Sailley_feed, dplyr::all_of(sailley_livestock)) |>
            dplyr::filter(
                Sailley_feed %in%
                    c("Total_matieres_premieres_concentrees", "Total_Fourrages")
            ) |>
            tidyr::pivot_longer(
                cols = !Sailley_feed,
                names_to = "Sailley_livestock",
                values_to = "raw_flow"
            ) |>
            tidyr::pivot_wider(
                id_cols = Sailley_livestock,
                names_from = Sailley_feed,
                values_from = raw_flow
            ),
        by = c('Sailley_livestock')
    ) |>
    dplyr::left_join(
        data_extra$Sailley_2021_feed_flows |>
            dplyr::select(Sailley_feed, feed_type),
        by = c('Sailley_feed')
    ) |>
    dplyr::mutate(
        rel_weight_p100 = case_when(
            feed_type == "feed_concent" ~ rel_weight /
                Total_matieres_premieres_concentrees,
            feed_type == "feed_rough" ~ rel_weight / Total_Fourrages
        )
    ) |>
    # filter feed >= 10% to keep only main feed type
    #filter(rel_weight_p100 >= 0.1) |>
    # filter feed >= 5% to keep only main feed type
    #filter(rel_weight_p100 >= 0.05) |>
    # recalculate overall feed percent
    dplyr::mutate(
        relweight_feed = sum(rel_weight, na.rm = T), # in tDM
        ref_p100_feed = rel_weight / relweight_feed, # in %
        .by = Sailley_livestock
    )
# Estimate farm theoretical ration -------------------------------------------------------------------------------------------------------
## Steps:
## 1. Estimate total feed DM per livestock category by multiplying AROPAJ ref by nb of livestock units => th_total_DMI_t_livcat_y
## 2. Estimate ton of DM per crop per livestock category using the proportion based on Sailley et al., 2021 => th_DMI_t_livcat_y

th_feed_ration <- AROPAJ_ref_ration |>
    # add theoretical feed allocation
    dplyr::left_join(
        data_extra$livestock |>
            dplyr::select(
                FADN_code_letter,
                AROPAJ,
                Sailley_livestock,
                livestock_unit_coef
            ) |>
            tidyr::separate_longer_delim(AROPAJ, "|"),
        by = c('AROPAJ')
    ) |>
    dplyr::left_join(
        Sailley_ref_feed_allocation |>
            dplyr::select(
                Sailley_livestock,
                Sailley_feed,
                feed_type,
                ref_p100_feed
            ),
        by = c('Sailley_livestock'),
        relationship = "many-to-many"
    ) |>
    # estimate theoretical ration per livestock category per crop
    dplyr::mutate(
        th_DMI_t_LU_y = DMI_t_LU_y * ref_p100_feed #, # in tDM
        #th_DMI_t_LU_y = th_DMI_t_livcat_y / Qobs_LU # in tDM/LU
    )

#th_feed_ration |> view()

# Estimate Feed quality ------------------------------------------------------
## For each feed, we assign an average dry matter (DM), crude protein content (CP) in DM, gross energy (GE) in DM and ash (Ash) in DM based on the INRA-CIRAD-AFZ feed tables

# average crop quality
## average GE, DM & CP (DM is not estimated as it is always 100%)
Sailley_crop_qlty <- data_extra$Sailley_2021_feed_flows |>
    dplyr::select(Sailley_feed, feed_tables, NZ_feed_DE) |>
    tidyr::separate_longer_delim(feed_tables, ";") |>
    tidyr::separate_longer_delim(NZ_feed_DE, ";") |>
    # add feed nutritional values
    dplyr::left_join(
        data_extra$feed_table_all_as_DM,
        by = 'feed_tables'
    ) |>
    # add feed digestibility
    dplyr::left_join(
        data_extra$NZ_feed_DE,
        by = dplyr::join_by(NZ_feed_DE == Feed_Description)
    ) |>
    # summarise by Sailley crop name
    dplyr::summarise(
        GE_MJ_kg = mean(`GE MJ/kg`, na.rm = T),
        GE_kcal_kg = mean(`GE kcal/kg`, na.rm = T),
        CP_pc = mean(`CP %`, na.rm = T),
        Ash_pc = mean(`Ash %`, na.rm = T),
        DE_pc = mean(Dry_Matter_Digestibility_pct, na.rm = T),
        .by = 'Sailley_feed'
    )

## Total GE, DM & CP
th_feed_ration_qlty <- th_feed_ration |>
    # add average feed quality
    dplyr::left_join(
        Sailley_crop_qlty,
        by = c('Sailley_feed')
    ) |>
    # convert
    dplyr::mutate(
        th_DM_t_LU_y = th_DMI_t_LU_y,
        th_GE_MJ_LU_y = th_DMI_t_LU_y * (GE_MJ_kg * 10^3),
        th_GE_kcal_LU_y = th_DMI_t_LU_y * (GE_kcal_kg * 10^3),
        th_CP_t_LU_y = th_DMI_t_LU_y * (CP_pc / 100),
        th_Ash_t_LU_y = th_DMI_t_LU_y * (Ash_pc / 100)
    )

#th_feed_ration_qlty |> view()

# Summarise theoretical feed ration quality per livestock category
th_feed_ration_qlty_summary <- th_feed_ration_qlty |>
    dplyr::summarise(
        th_DM_t_LU_y = sum(th_DM_t_LU_y, na.rm = T),
        th_GE_MJ_LU_y = sum(th_GE_MJ_LU_y, na.rm = T),
        th_GE_kcal_LU_y = sum(th_GE_kcal_LU_y, na.rm = T),
        th_CP_t_LU_y = sum(th_CP_t_LU_y, na.rm = T),
        th_Ash_t_LU_y = sum(th_Ash_t_LU_y, na.rm = T),
        .by = c('AROPAJ', 'FADN_code_letter', 'Sailley_livestock')
    ) |>
    dplyr::summarise(
        th_DM_t_LU_y = mean(th_DM_t_LU_y, na.rm = T),
        th_GE_MJ_LU_y = mean(th_GE_MJ_LU_y, na.rm = T),
        th_GE_kcal_LU_y = mean(th_GE_kcal_LU_y, na.rm = T),
        th_CP_t_LU_y = mean(th_CP_t_LU_y, na.rm = T),
        th_Ash_t_LU_y = mean(th_Ash_t_LU_y, na.rm = T),
        .by = c('FADN_code_letter')
    )

#th_feed_ration_qlty_summary |> view()

# export the summary table when script is sourced
return(th_feed_ration_qlty_summary)
