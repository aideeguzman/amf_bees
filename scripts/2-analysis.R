## analysis ####################################################################
## *****************************************************************************

# 1. plant traits
# 2. AMF traits
# 3. floral traits
# 4. bee visitation
# 5. SEM

## *****************************************************************************
## 1. plant traits #############################################################
## *****************************************************************************

## 1a. shoots
shoots <- lm.model(y="shoot_g", data= df_plant_amf_traits)
model.df(shoots)
shoots_cont = model_contrasts(shoots)

## 1b. roots
roots <- lm.model(y="root_g", data= df_plant_amf_traits)
model.df(roots)
roots_cont = model_contrasts(roots)


## 1c. roots:shoots
root_shoot <- lm.model(y="root_shoot", data= df_plant_amf_traits)
model.df(root_shoot)
root_shoot_cont = model_contrasts(root_shoot)


## *****************************************************************************
## 2. AMF traits ###############################################################
## *****************************************************************************

## 2a. hyphae
hyphae <- lm.model(y="hyphae_m_g", data= df_plant_amf_traits)
model.df(hyphae)
hyphae_cont = model_contrasts(hyphae)

## 2b. colonization
col <- glm.model(y="prop_col", data= df_plant_amf_traits, family="binomial", weights="tot_amf_counts")
model.df(col)
col_cont = model_contrasts(col)

## 2c. colonization:hyphae
col_hyp <- lm.model(y="col_hyp", data= df_plant_amf_traits)
model.df(col_hyp)
col_hyp_cont = model_contrasts(col_hyp)

## 2d. spores
spores <- glm.nb.model(y="spore_count", data = df_plant_amf_traits)
model.df(spores)
spores_cont = model_contrasts(spores)



## *****************************************************************************
## 3. floral traits ############################################################
## *****************************************************************************

## 3a. flower size
flower_depth <- lmer.model(y="flower_d_cm", data=df_flower_traits)
model.df(flower_depth)
flower_depth_cont = model_contrasts(flower_depth)

## 3b. flower number
flower_tot <- glmer.model(y="tot_flower", data=df_flower_traits)
model.df(flower_tot)
flower_tot_cont = model_contrasts(flower_tot)

## 3c. nectar sugar
brix <- lmer.model(y="nectar_brix", data = df_flower_traits)
model.df(brix)
brix_cont = model_contrasts(brix)

## 3c. nectar volume
volume <- lmer.model(y="nectar_mm", data = df_flower_traits)
model.df(volume)
volume_cont = model_contrasts(volume)

## 3d. pollen density
poll_vol <- glmer.model(y="pollen_density", data=df_flower_traits)
model.df(poll_vol)
poll_vol_cont = model_contrasts(poll_vol)

## 3e. pollen protein
protein <- lmer.model(y="protein_mg_l", data=df_flower_traits)
model.df(protein)
protein_cont = model_contrasts(protein)





## *****************************************************************************
## 4. bee visitation ###########################################################
## *****************************************************************************

## 4a. number of bee visits
beevisits <- glmer.model(y="visits", data=df_bee_visits)
model.df(beevisits)
beevisits_cont = model_contrasts(beevisits)

## 4b. visitation time
visittime <- lmer.model(y="visit_time", data=df_bee_visits)
model.df(visittime)
visittime_cont = model_contrasts(visittime)




## *****************************************************************************
## 5. SEM ######################################################################
## *****************************************************************************


## number of visits - 1
sem_visits_01 <- psem(
  glm(total_visits ~ hyphae_m_g + prop_col + col_hyp + nectar_mm + pollen_density + 
        protein_mg_l + nectar_brix + flower_d_cm + tot_flower + 
        offset(log(total_daily_obs)), data = df_sem, family="poisson"),
  lm(protein_mg_l ~ hyphae_m_g + prop_col + col_hyp , data=df_sem),
  lm(nectar_mm ~ hyphae_m_g + prop_col + col_hyp, data=df_sem),
  lm(pollen_density ~ hyphae_m_g + prop_col + col_hyp, data=df_sem),
  lm(flower_d_cm ~ hyphae_m_g + prop_col + col_hyp, data=df_sem),
  lm(nectar_brix ~ hyphae_m_g + prop_col + col_hyp, data=df_sem),
  lm(tot_flower ~ hyphae_m_g + prop_col + col_hyp, data=df_sem),
  data = df_sem)
summary(sem_visits_01)


## number of visits - 2
sem_visits_02 <- psem(
  glm(total_visits ~ flower_d_cm +
        offset(log(total_daily_obs)), data = df_sem, family="poisson"),
  lm(flower_d_cm ~ hyphae_m_g + prop_col, data=df_sem),
  data = df_sem)
summary(sem_visits_02)



## visitation time - 1
sem_time_01 <-psem(
  lm(total_visit_time ~ hyphae_m_g + prop_col + col_hyp + nectar_mm + pollen_density + protein_mg_l + nectar_brix + flower_d_cm + tot_flower + offset(log(total_daily_obs)), data = df_sem),
  lm(protein_mg_l ~ hyphae_m_g + prop_col + col_hyp , data=df_sem),
  lm(nectar_mm ~ hyphae_m_g + prop_col + col_hyp, data=df_sem),
  lm(pollen_density ~ hyphae_m_g + prop_col + col_hyp, data=df_sem),
  lm(flower_d_cm ~ hyphae_m_g + prop_col + col_hyp, data=df_sem),
  lm(nectar_brix ~ hyphae_m_g + prop_col + col_hyp, data=df_sem),
  lm(tot_flower ~ hyphae_m_g + prop_col + col_hyp, data=df_sem),
  data = df_sem)
summary(sem_time_01)


## visitation time - 2
sem_time_02 <-psem(
  lm(flower_d_cm ~ hyphae_m_g + prop_col, data=df_sem),
  data = df_sem)
summary(sem_time_02)

