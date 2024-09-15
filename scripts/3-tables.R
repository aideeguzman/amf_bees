## *****************************************************************************
## tables ######################################################################
## *****************************************************************************



## *****************************************************************************
## model significance ##########################################################
## *****************************************************************************




all_models <- bind_rows(model.df(shoots) %>% rename("statistic" = 4),
                        model.df(roots) %>% rename("statistic" = 4),
                        model.df(root_shoot) %>% rename("statistic" = 4),
                        model.df(hyphae) %>% rename("statistic" = 4),
                        model.df(col) %>% rename("statistic" = 4),
                        model.df(col_hyp) %>% rename("statistic" = 4),
                        model.df(spores) %>% rename("statistic" = 4),
                        model.df(flower_depth) %>% rename("statistic" = 4),
                        model.df(flower_tot) %>% rename("statistic" = 4),
                        model.df(volume) %>% rename("statistic" = 4),
                        model.df(brix) %>% rename("statistic" = 4),
                        model.df(poll_vol) %>% rename("statistic" = 4),
                        model.df(protein) %>% rename("statistic" = 4),
                        model.df(beevisits) %>% rename("statistic" = 4),
                        model.df(visittime) %>% rename("statistic" = 4)) %>%
  dplyr::select(model, variable, covariate, statistic, P, sig) %>%
  pivot_longer(cols = statistic:sig) %>%
  filter(!is.na(value), ! model %in% c("amf_richness", "amf_pa") | covariate != "P supply") %>%
  mutate(stat = paste(covariate, name, sep = "_")) %>%
  dplyr::select(-covariate, -name, -model) %>%
  pivot_wider(id_cols = variable, names_from = stat, values_from = value) %>%
  dplyr::select(1,5:7,2:4,8:22)

write.xlsx(all_models, "tables/Table 1.xlsx")



## *****************************************************************************
## mean se +/- #################################################################
## *****************************************************************************


final_for_mean <- df_all %>%
  full_join(df_bee_visits %>% ungroup() %>% 
              dplyr::select(field_measure_date:visits, p_supply:amf_richness, amf_csr)) %>%
  group_by(plant_id) %>%
  mutate(prop_col = prop_col *100,
         rank = row_number()) %>% ungroup() %>%
  mutate(across(c(shoot_g, root_g, root_shoot, hyphae_m_g, prop_col, col_hyp, spore_count), ~ifelse( rank> 1, NA,.x)) )


amf_pa_summary <- final_for_mean %>%
  pivot_longer(shoot_g:visits, names_to = "measurement", values_to = "value") %>%
  group_by(amf_pa, measurement) %>%
  summarise_at(vars(value), list(mean=mean, se=std.error), na.rm=TRUE) %>%
  mutate(mean = round(mean, 3), se = round(se, 3), p_supply = "-", combination = "amf_pa") %>%
  dplyr::select(combination, amf_pa, p_supply, measurement, mean, se) %>%
  rename("amf" = amf_pa)

amf_richness_summary <- final_for_mean %>%
  pivot_longer(shoot_g:visits, names_to = "measurement", values_to = "value") %>%
  group_by(amf_richness, measurement) %>%
  summarise_at(vars(value), list(mean=mean, se=std.error), na.rm=TRUE) %>%
  mutate(mean = round(mean, 3), se = round(se, 3), p_supply = "-", combination = "amf_richness") %>%
  dplyr::select(combination, amf_richness, p_supply, measurement, mean, se) %>%
  rename("amf" = amf_richness)

amf_csr_summary <- final_for_mean %>%
  unique() %>%
  pivot_longer(shoot_g:visits, names_to = "measurement", values_to = "value") %>%
  group_by(amf_csr, measurement) %>%
  summarise_at(vars(value), list(mean=mean, se=std.error), na.rm=TRUE) %>%
  mutate(mean = round(mean, 3), se = round(se, 3), p_supply = "-", combination = "amf_csr") %>%
  dplyr::select(combination, amf_csr, p_supply, measurement, mean, se)  %>%
  rename("amf" = amf_csr)



p_summary <- final_for_mean %>%
  pivot_longer(shoot_g:visits, names_to = "measurement", values_to = "value") %>%
  group_by(p_supply, measurement) %>%
  summarise_at(vars(value), list(mean=mean, se=std.error), na.rm=TRUE) %>%
  mutate(mean = round(mean, 3), se = round(se, 3), amf = "-", combination = "p") %>%
  dplyr::select(combination, amf, p_supply, measurement, mean, se)





amf_pa_p_summary <- final_for_mean %>%
  pivot_longer(shoot_g:visits, names_to = "measurement", values_to = "value") %>%
  group_by(amf_pa, p_supply, measurement) %>%
  summarise_at(vars(value), list(mean=mean, se=std.error), na.rm=TRUE) %>%
  mutate(mean = round(mean, 3), se = round(se, 3), combination = "amf_pa_p") %>%
  dplyr::select(combination, amf_pa, p_supply, measurement, mean, se) %>%
  rename("amf" = amf_pa)


amf_richness_p_summary <- final_for_mean %>%
  pivot_longer(shoot_g:visits, names_to = "measurement", values_to = "value") %>%
  group_by(amf_richness, p_supply, measurement) %>%
  summarise_at(vars(value), list(mean=mean, se=std.error), na.rm=TRUE) %>%
  mutate(mean = round(mean, 3), se = round(se, 3), combination = "amf_richness_p") %>%
  dplyr::select(combination,amf_richness, p_supply, measurement, mean, se) %>%
  rename("amf" = amf_richness)

amf_csr_p_summary <- final_for_mean %>%
  pivot_longer(shoot_g:visits, names_to = "measurement", values_to = "value") %>%
  group_by(amf_csr, p_supply, measurement) %>%
  summarise_at(vars(value), list(mean=mean, se=std.error), na.rm=TRUE) %>%
  mutate(mean = round(mean, 3), se = round(se, 3), combination = "amf_csr_p") %>%
  dplyr::select(combination, amf_csr, p_supply, measurement, mean, se)  %>%
  rename("amf" = amf_csr)


mean_se <- bind_rows(amf_pa_summary, amf_richness_summary, amf_csr_summary,  
                     p_summary,  
                     amf_pa_p_summary, amf_richness_p_summary, amf_csr_p_summary)  %>%
  mutate(measurement = case_when(measurement == "shoot_g" ~ "shoot (g)",
                                 measurement == "root_g" ~ "root (g)",
                                 measurement == "root_shoot" ~ "root:shoot",
                                 measurement == "spore_count" ~ "spore count (grains/ml)",
                                 measurement == "hyphae_m_g" ~ "hyphal length (m/g)",
                                 measurement == "prop_col" ~ "% root colonization",
                                 measurement == "col_hyp" ~ "root colonization:hyphae",
                                 measurement == "nectar_brix" ~ "nectar sugar (%brix)",
                                 measurement == "nectar_mm" ~ "nectar volume (µl)",
                                 measurement == "flower_d_cm" ~ "flower size (cm)",
                                 measurement == "pollen_density" ~ "pollen density (grains/µl)",
                                 measurement == "protein_mg_l" ~ "pollen protein (mg/l)",
                                 measurement == "tot_flower" ~ "number of flowers",
                                 measurement == "visit_time" ~ "bee visitation time (s)",
                                 measurement == "visits" ~ "number of bee visits"),
         measurement = factor(
           measurement, levels=c("shoot (g)", "root (g)", "root:shoot",
                                 "hyphal length (m/g)", "% root colonization",
                                 "root colonization:hyphae", "spore count (grains/ml)",
                                 "flower size (cm)", "number of flowers", 
                                 "nectar volume (µl)", "nectar sugar (%brix)",
                                 "pollen density (grains/µl)", "pollen protein (mg/l)",
                                 "number of bee visits", "bee visitation time (s)")),
         amf = factor(amf, levels = c("none", "competitor","stress tolerant","ruderal","csr",
                                      "0 sp","2 sp","6 sp",
                                      "absent", "present",
                                      "-")),
         # amf = recode(amf, csr = "CSR", competitor = "C", `stress tolerant` = "S", ruderal = "R"), 
         p_supply = factor(p_supply, levels = c("low","high", "-"))) 





cld_df = bind_rows(shoots_cont$cld_df,
                   roots_cont$cld_df,
                   root_shoot_cont$cld_df,
                   hyphae_cont$cld_df,
                   col_cont$cld_df,
                   col_hyp_cont$cld_df,
                   spores_cont$cld_df,
                   flower_depth_cont$cld_df,
                   flower_tot_cont$cld_df,
                   brix_cont$cld_df,
                   volume_cont$cld_df,
                   poll_vol_cont$cld_df,
                   protein_cont$cld_df,
                   beevisits_cont$cld_df,
                   visittime_cont$cld_df) %>%
  mutate(p_supply = ifelse(is.na(p_supply), "-", p_supply),
         amf = ifelse(is.na(amf), "-", amf)) %>%
  dplyr::select(-combination)



mean_se_cld = mean_se %>%
  left_join(cld_df)



mean_se_wide <- mean_se_cld %>%
  mutate(mean_se = paste(format(round(mean,1), nsmall=1), "±",
                         format(round(se,1), nsmall=1), "  ", pairwise_group, sep=" ")) %>%
  dplyr::select(-mean, -se) %>%
  pivot_wider(id_cols = amf:p_supply, names_from = "measurement", values_from = "mean_se") %>%
  dplyr::select(amf, p_supply, c("shoot (g)", "root (g)", "root:shoot",
                                 "hyphal length (m/g)", "% root colonization",
                                 "root colonization:hyphae", "spore count (grains/ml)",
                                 "flower size (cm)", "number of flowers", 
                                 "nectar volume (µl)", "nectar sugar (%brix)",
                                 "pollen density (grains/µl)", "pollen protein (mg/l)",
                                 "number of bee visits", "bee visitation time (s)")) %>%
  ungroup() %>%
  mutate(type = case_when(amf %in% c("-") ~ "p_supply",
                          amf %in% c("none", "competitor","stress tolerant",
                                     "ruderal","csr", "-") ~ "amf_csr",
                          (amf %in% c("none", "competitor","stress tolerant",
                                      "ruderal","csr", "-") & p_supply == "-") ~ "amf_csr",
                          (amf %in% c("none", "competitor","stress tolerant",
                                      "ruderal","csr", "-") & p_supply != "-") ~ "amf_csr x p_supply",
                          (amf %in% c("0 sp","2 sp","6 sp") & p_supply == "-") ~ "amf_richness",
                          (amf %in% c("0 sp","2 sp","6 sp") & p_supply != "-") ~ "amf_richness x p_supply",
                          (amf %in% c("absent", "present") & p_supply == "-") ~ "amf_pa",
                          (amf %in% c("absent", "present") & p_supply != "-") ~ "amf_pa x p_supply")) %>%
  mutate(type = factor(type, levels = c("p_supply", 
                                        "amf_csr", "amf_csr x p_supply",
                                        "amf_richness","amf_richness x p_supply",
                                        "amf_pa","amf_pa x p_supply"))) %>%
  arrange(type) %>%
  add_row(.after = 2) %>%
  add_row(.after = 8) %>%
  add_row(.after = 19) %>%
  add_row(.after = 23) %>%
  add_row(.after = 30) %>%
  add_row(.after = 33) %>%
  rename("AMF treatment" = amf, "P supply" = p_supply)


write.xlsx(mean_se_wide %>% 
             dplyr::select(type, `AMF treatment`, `P supply`,
                           any_of(c("shoot (g)", "root (g)", "root:shoot"))), 
           "tables/Table S2.xlsx", rowNames = FALSE)



write.xlsx(mean_se_wide %>% 
             dplyr::select(type, `AMF treatment`, `P supply`,
                           any_of(c("hyphal length (m/g)", "% root colonization",
                                    "root colonization:hyphae", "spore count (grains/ml)"))), 
           "tables/Table S3.xlsx", rowNames = FALSE)


write.xlsx(mean_se_wide %>% 
             dplyr::select(type, `AMF treatment`, `P supply`,
                           any_of(c("flower size (cm)", "number of flowers", 
                                    "nectar volume (µl)", "nectar sugar (%brix)",
                                    "pollen density (grains/µl)", "pollen protein (mg/l)"))), 
           "tables/Table S4.xlsx", rowNames = FALSE)


write.xlsx(mean_se_wide %>% 
             dplyr::select(type, `AMF treatment`, `P supply`,
                           any_of(c("number of bee visits", "bee visitation time (s)"))), 
           "tables/Table S5.xlsx", rowNames = FALSE)
