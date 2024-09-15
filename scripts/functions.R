## *****************************************************************************
## functions ###################################################################
## *****************************************************************************

## *****************************************************************************
## model outputs ###############################################################
## *****************************************************************************
# 
# 
# Linear mixed effects model
lmer.model <- function(y, data){
  df <- data %>%
    mutate(y = scale(eval(as.symbol(y))))
  
  # (1) y ~ amf_csr*p_supply
  mod1 <- lmer(y ~ amf_csr*p_supply + (1|plant_id) + (1|field_measure_date), data = df)
  mod1_summary <- model.summary(mod1)
  
  # (2) y ~ amf_pa*p_supply
  mod2 <- lmer(y ~ amf_pa*p_supply + (1|plant_id)  + (1|field_measure_date), data = df)
  mod2_summary <- model.summary(mod2)
  
  # (3) y ~ amf_richness*p_supply
  mod3 <- lmer(y ~ amf_richness*p_supply + (1|plant_id)  + (1|field_measure_date), data = df)
  mod3_summary <- model.summary(mod3)
  
  models <- list(amf_csr=mod1, amf_richness=mod3, amf_pa = mod2)
  model_summary <- list(amf_csr=mod1_summary, amf_richness=mod3_summary, amf_pa=mod2_summary)
  
  AICc <- AICc( mod2, mod3, mod1)
  row.names(AICc) <- c("amf_pa","amf_richness", "amf_csr")
  AICc <- AICc %>% rownames_to_column("model")
  
  return(list(model_summary = model_summary, models=models, AICc=AICc))
  
  
}

# Linear model
lm.model <- function(y, data){
  df <- data %>%
    mutate(y = scale(eval(as.symbol(y))))
  
  # (1) y ~ amf_csr*p_supply
  mod1 <- lm(y ~ amf_csr*p_supply, data = df)
  mod1_summary <- model.summary(mod1)
  
  # (2) y ~ amf_pa*p_supply
  mod2 <- lm(y ~ amf_pa*p_supply, data = df)
  mod2_summary <- model.summary(mod2)
  
  # (3) y ~ amf_richness*p_supply
  mod3 <- lm(y ~ amf_richness*p_supply, data = df)
  mod3_summary <- model.summary(mod3)
  
  models <- list(amf_csr=mod1, amf_richness=mod3, amf_pa = mod2)
  model_summary <- list(amf_csr=mod1_summary, amf_richness=mod3_summary, amf_pa=mod2_summary)
  
  AICc <- AICc(mod1, mod2, mod3)
  row.names(AICc) <- c("amf_csr","amf_pa","amf_richness")
  AICc <- AICc %>% rownames_to_column("model")
  
  return(list(model_summary = model_summary, models=models, AICc=AICc))
  
  
}

# Generalized linear mixed effects model
glmer.model <- function(y, data){
  df <- data %>%
    mutate(y = eval(as.symbol(y)))
  
  # (1) y ~ amf_csr*p_supply
  mod1 <- glmer(y ~ amf_csr*p_supply + (1|plant_id)  + (1|field_measure_date), data = df, 
                family = "poisson")
  mod1_summary <- model.summary(mod1)
  
  # (2) y ~ amf_pa*p_supply
  mod2 <- glmer(y ~ amf_pa*p_supply + (1|plant_id)  + (1|field_measure_date), data = df, family = "poisson")
  mod2_summary <- model.summary(mod2)
  
  # (3) y ~ amf_richness*p_supply
  mod3 <- glmer(y ~ amf_richness*p_supply + (1|plant_id)  + (1|field_measure_date), data = df, family = "poisson")
  mod3_summary <- model.summary(mod3)
  
  models <- list(amf_csr=mod1, amf_richness=mod3, amf_pa = mod2)
  model_summary <- list(amf_csr=mod1_summary, amf_richness=mod3_summary, amf_pa=mod2_summary)
  
  AICc <- AICc(mod1, mod2, mod3)
  row.names(AICc) <- c("amf_csr","amf_pa","amf_richness")
  AICc <- AICc %>% rownames_to_column("model")
  
  return(list(model_summary = model_summary, models=models, AICc=AICc))
  
  
}



# Generalized linear model
glm.model <- function(y, data, family="poisson", weights=NULL){
  df <- data %>%
    mutate(y = eval(as.symbol(y)))
  
  if(!is.null(weights)){
    df <- df %>%
      mutate(weights = eval(as.symbol(weights)))
  } else {
    weights=NULL
  }
  
  # (1) y ~ amf_csr*p_supply
  mod1 <- glm(y ~ amf_csr*p_supply, data = df, weights = weights, family = family)
  mod1_summary <- model.summary(mod1)
  
  # (2) y ~ amf_pa*p_supply
  mod2 <- glm(y ~ amf_pa*p_supply, data = df,weights = weights,  family = family)
  mod2_summary <- model.summary(mod2)
  
  # (3) y ~ amf_richness*p_supply
  mod3 <- glm(y ~ amf_richness*p_supply, data = df, weights = weights, family = family)
  mod3_summary <- model.summary(mod3)
  
  models <- list(amf_csr=mod1, amf_richness=mod3, amf_pa = mod2)
  model_summary <- list(amf_csr=mod1_summary, amf_richness=mod3_summary, amf_pa=mod2_summary)
  
  AICc <- AICc(mod1, mod2, mod3)
  row.names(AICc) <- c("amf_csr","amf_pa","amf_richness")
  AICc <- AICc %>% rownames_to_column("model")
  
  return(list(model_summary = model_summary, models=models, AICc=AICc))
  
  
}



glm.nb.model <- function(y, data,  weights=NULL){
  df <- data %>%
    mutate(y = round(eval(as.symbol(y)),0))
  
  if(!is.null(weights)){
    df <- df %>%
      mutate(weights = eval(as.symbol(weights)))
  } else {
    weights=NULL
  }
  
  # (1) y ~ amf_csr*p_supply
  mod1 <- glm.nb(y ~ amf_csr*p_supply, data = df)
  mod1_summary <- model.summary(mod1)
  
  # (2) y ~ amf_pa*p_supply
  mod2 <- glm.nb(y ~ amf_pa*p_supply, data = df)
  mod2_summary <- model.summary(mod2)
  
  # (3) y ~ amf_richness*p_supply
  mod3 <- glm.nb(y ~ amf_richness*p_supply, data = df)
  mod3_summary <- model.summary(mod3)
  
  models <- list(amf_csr=mod1, amf_richness=mod3, amf_pa = mod2)
  model_summary <- list(amf_csr=mod1_summary, amf_richness=mod3_summary, amf_pa=mod2_summary)
  
  AICc <- AICc(mod1, mod2, mod3)
  row.names(AICc) <- c("amf_csr","amf_pa","amf_richness")
  AICc <- AICc %>% rownames_to_column("model")
  
  return(list(model_summary = model_summary, models=models, AICc=AICc))
  
  
}


## *****************************************************************************
## model summary table #########################################################
## *****************************************************************************


# get response variable
get.response <- function(formula) {
  tt <- terms(formula)
  vars <- as.character(attr(tt, "variables"))[-1] ## [1] is the list call
  response <- attr(tt, "response") # index of response var
  vars[response] 
}

# make table of the model summary and Anova
model.summary <- function(model){
  summary <- summary(model)
  coef <- as.data.frame(summary$coefficients)
  type <- class(model)[1]
  
  if(type == "glmerMod"){
    
    summary_table <- coef %>%
      rownames_to_column("covariate") %>% 
      filter(!str_detect(covariate, 'Intercept')) %>%
      mutate_if(is.numeric, ~format(round(.,3), nsmall=3)) %>%
      rename(`P` = `Pr(>|z|)`, `SE` = `Std. Error`) %>%
      mutate(sig = case_when(
        P <= 0.001 ~ "***",
        P <= 0.01 &  P > 0.001 ~ "**",
        P <= 0.05 &  P > 0.01 ~ "*",
        P > 0.05  ~ "ns"),
        response = get.response(model)) %>%
      mutate(Estimate = as.numeric(Estimate),
             SE = as.numeric(SE)) %>%
      dplyr::select(covariate, Estimate, SE, P, sig)
    
    anova_table <- as.data.frame(car::Anova(model, type = 2, singular.ok = T)) %>%
      rownames_to_column("covariate") %>% 
      filter(!str_detect(covariate, 'Intercept')) %>%
      mutate_if(is.numeric, ~format(round(.,3), nsmall=3)) %>%
      rename(`P` = `Pr(>Chisq)`) %>%
      mutate(sig = case_when(
        P <= 0.001 ~ "***",
        P <= 0.01 &  P > 0.001 ~ "**",
        P <= 0.05 &  P > 0.01 ~ "*",
        P > 0.05  ~ "ns"),
        response = get.response(model)) %>%
      dplyr::select(covariate, Chisq, P, sig)
    
  }  else if(type == "glmer"){
    
    summary_table <- coef %>%
      rownames_to_column("covariate") %>% 
      filter(!str_detect(covariate, 'Intercept')) %>%
      mutate_if(is.numeric, ~format(round(.,3), nsmall=3)) %>%
      rename(`P` = `Pr(>|z|)`, `SE` = `Std. Error`) %>%
      mutate(sig = case_when(
        P <= 0.001 ~ "***",
        P <= 0.01 &  P > 0.001 ~ "**",
        P <= 0.05 &  P > 0.01 ~ "*",
        P > 0.05  ~ "ns"),
        response = get.response(model)) %>%
      mutate(Estimate = as.numeric(Estimate),
             SE = as.numeric(SE)) %>%
      dplyr::select(covariate, Estimate, SE, P, sig)
    
    anova_table <- as.data.frame(car::Anova(model, type = 2, singular.ok = T)) %>%
      rownames_to_column("covariate") %>% 
      filter(!str_detect(covariate, 'Intercept')) %>%
      mutate_if(is.numeric, ~format(round(.,3), nsmall=3)) %>%
      rename(`P` = `Pr(>Chisq)`) %>%
      mutate(sig = case_when(
        P <= 0.001 ~ "***",
        P <= 0.01 &  P > 0.001 ~ "**",
        P <= 0.05 &  P > 0.01 ~ "*",
        P > 0.05  ~ "ns"),
        response = get.response(model)) %>%
      dplyr::select(covariate, Chisq, P, sig)
    
  } else if(type == "glm"){
    
    summary_table <- coef %>%
      rownames_to_column("covariate") %>% 
      filter(!str_detect(covariate, 'Intercept')) %>%
      mutate_if(is.numeric, ~format(round(.,3), nsmall=3)) %>%
      rename(`P` = `Pr(>|z|)`, `SE` = `Std. Error`) %>%
      mutate(sig = case_when(
        P <= 0.001 ~ "***",
        P <= 0.01 &  P > 0.001 ~ "**",
        P <= 0.05 &  P > 0.01 ~ "*",
        P > 0.05  ~ "ns"),
        response = get.response(model)) %>%
      dplyr::select(covariate, Estimate, SE, P, sig)
    
    anova_table <- as.data.frame(car::Anova(model, type = 2, test = "Wald", singular.ok = T)) %>%
      rownames_to_column("covariate") %>% 
      filter(!str_detect(covariate, 'Intercept')) %>%
      mutate_if(is.numeric, ~format(round(.,3), nsmall=3)) %>%
      rename(`P` = `Pr(>Chisq)`) %>%
      mutate(sig = case_when(
        P <= 0.001 ~ "***",
        P <= 0.01 &  P > 0.001 ~ "**",
        P <= 0.05 &  P > 0.01 ~ "*",
        P > 0.05  ~ "ns"),
        response = get.response(model)) %>%
      dplyr::select(covariate, Chisq, P, sig)
    
  } else if(type == "negbin"){
    
    summary_table <- coef %>%
      rownames_to_column("covariate") %>% 
      filter(!str_detect(covariate, 'Intercept')) %>%
      mutate_if(is.numeric, ~format(round(.,3), nsmall=3)) %>%
      rename(`P` = `Pr(>|z|)`, `SE` = `Std. Error`) %>%
      mutate(sig = case_when(
        P <= 0.001 ~ "***",
        P <= 0.01 &  P > 0.001 ~ "**",
        P <= 0.05 &  P > 0.01 ~ "*",
        P > 0.05  ~ "ns"),
        response = get.response(model)) %>%
      dplyr::select(covariate, Estimate, SE, P, sig)
    
    anova_table <- as.data.frame(car::Anova(model, type = 2, test = "F", singular.ok = T)) %>%
      rownames_to_column("covariate") %>% 
      filter(!str_detect(covariate, 'Intercept')) %>%
      mutate_if(is.numeric, ~format(round(.,3), nsmall=3)) %>%
      rename(`P` = `Pr(>F)`,
             `F` = `F value`) %>%
      mutate(sig = case_when(
        P <= 0.001 ~ "***",
        P <= 0.01 &  P > 0.001 ~ "**",
        P <= 0.05 &  P > 0.01 ~ "*",
        P > 0.05  ~ "ns"),
        response = get.response(model)) %>%
      dplyr::select(covariate, `F`, P, sig)
    
  } else if(type == "lmerModLmerTest"){
    
    summary_table <- coef %>%
      rownames_to_column("covariate") %>% 
      filter(!str_detect(covariate, 'Intercept')) %>%
      mutate_if(is.numeric, ~format(round(.,3), nsmall=3)) %>%
      rename(`P` = `Pr(>|t|)`, `SE` = `Std. Error`) %>%
      mutate(sig = case_when(
        P <= 0.001 ~ "***",
        P <= 0.01 &  P > 0.001 ~ "**",
        P <= 0.05 &  P > 0.01 ~ "*",
        P > 0.05  ~ "ns"),
        response = get.response(model)) %>%
      mutate(Estimate = as.numeric(Estimate),
             SE = as.numeric(SE)) %>%
      dplyr::select(covariate, Estimate, SE, P, sig)
    
    
    anova_table <- as.data.frame(car::Anova(model, type = 2, test = "F", singular.ok = T)) %>%
      rownames_to_column("covariate") %>% 
      filter(!str_detect(covariate, 'Intercept')) %>%
      mutate_if(is.numeric, ~format(round(.,3), nsmall=3)) %>%
      rename(`P` = `Pr(>F)`) %>%
      mutate(sig = case_when(
        P <= 0.001 ~ "***",
        P <= 0.01 &  P > 0.001 ~ "**",
        P <= 0.05 &  P > 0.01 ~ "*",
        P > 0.05  ~ "ns"),
        response = get.response(model)) %>%
      dplyr::select(covariate, `F`, P, sig)
    
  } else if(type == "lm"){
    
    summary_table <- coef %>%
      rownames_to_column("covariate") %>% 
      filter(!str_detect(covariate, 'Intercept')) %>%
      mutate_if(is.numeric, ~format(round(.,3), nsmall=3)) %>%
      rename(`P` = `Pr(>|t|)`, `SE` = `Std. Error`) %>%
      mutate(sig = case_when(
        P <= 0.001 ~ "***",
        P <= 0.01 &  P > 0.001 ~ "**",
        P <= 0.05 &  P > 0.01 ~ "*",
        P > 0.05  ~ "ns"),
        response = get.response(model)) %>%
      mutate(Estimate = as.numeric(Estimate),
             SE = as.numeric(SE)) %>%
      dplyr::select(covariate, Estimate, SE, P, sig)
    
   
    anova_table <- as.data.frame(car::Anova(model, type = 2, test = "F", singular.ok = T)) %>%
      rownames_to_column("covariate") %>% 
      filter(!str_detect(covariate, 'Intercept')) %>%
      mutate_if(is.numeric, ~format(round(.,3), nsmall=3)) %>%
      rename(`P` = `Pr(>F)`,
             `F` = `F value`) %>%
      mutate(sig = case_when(
        P <= 0.001 ~ "***",
        P <= 0.01 &  P > 0.001 ~ "**",
        P <= 0.05 &  P > 0.01 ~ "*",
        P > 0.05  ~ "ns"),
        response = get.response(model)) %>%
      dplyr::select(covariate, `F`, P, sig)
  } 
  
  
  list(model = summary$call, AIC = AIC(model), summary = summary_table, Anova = anova_table) 
}


object_to_character <- function(object) {
  deparse(substitute(object))
}


model.df <- function(model){
  
  # oject_to_character
  var.tmp <-   deparse(substitute(model))
  var <-  case_when(var.tmp == "shoots" ~ "shoot (g)",
                    var.tmp == "roots" ~ "root (g)",
                    var.tmp == "root_shoot" ~ "root:shoot",
                    var.tmp == "hyphae" ~ "hyphal length (m/g)",
                    var.tmp == "col" ~ "% root colonization",
                    var.tmp == "col_hyp" ~ "root colonization:hyphae",
                    var.tmp == "spores" ~ "spore count (grains/ml)",
                    var.tmp == "flower_depth" ~ "flower size (cm)",
                    var.tmp == "flower_tot" ~ "number of flowers",
                    var.tmp == "volume" ~ "nectar volume (µl)",
                    var.tmp == "brix" ~ "nectar sugar (%brix)",
                    var.tmp == "poll_vol" ~ "pollen density (grains/µl)",
                    var.tmp == "protein" ~ "pollen protein (mg/l)",
                    var.tmp == "visittime" ~ "bee visitation time (s)",
                    var.tmp == "beevisits" ~ "number of bee visits")
  
  # arrange data.frames
  model_df <- rbind(model$model_summary$amf_csr$Anova %>% mutate(model = names(model$model_summary[1])),
                    model$model_summary$amf_richness$Anova %>% mutate(model = names(model$model_summary)[2]),
                    model$model_summary$amf_pa$Anova  %>% mutate(model = names(model$model_summary)[3])) %>%
    filter(covariate != "Residuals") %>%
    mutate(covariate = c( "AMF_CSR", "P supply", "AMF_CSR x P supply",
                          "AMF_richness", "P supply", "AMF_richness x P supply",
                          "AMF_pa", "P supply", "AMF_pa x P supply"),
           variable = var) %>%
    # column_to_rownames("covariate") %>%
    # dplyr::select(model, variable, covariate, `F value`, P, sig)  #%>%
    dplyr::select(5, 6, 1, 2, 3, 4) 
  
  return(model_df)
  
}



## *****************************************************************************
## multiple contrasts ###################################################################
## *****************************************************************************


model_contrasts = function(model){
  
  # variable
  var.tmp <-   deparse(substitute(model))
  var <-  case_when(var.tmp == "shoots" ~ "shoot (g)",
                    var.tmp == "roots" ~ "root (g)",
                    var.tmp == "root_shoot" ~ "root:shoot",
                    var.tmp == "hyphae" ~ "hyphal length (m/g)",
                    var.tmp == "col" ~ "% root colonization",
                    var.tmp == "col_hyp" ~ "root colonization:hyphae",
                    var.tmp == "spores" ~ "spore count (grains/ml)",
                    var.tmp == "flower_depth" ~ "flower size (cm)",
                    var.tmp == "flower_tot" ~ "number of flowers",
                    var.tmp == "volume" ~ "nectar volume (µl)",
                    var.tmp == "brix" ~ "nectar sugar (%brix)",
                    var.tmp == "poll_vol" ~ "pollen density (grains/µl)",
                    var.tmp == "protein" ~ "pollen protein (mg/l)",
                    var.tmp == "beevisits" ~ "bee visitation time (s)",
                    var.tmp == "visittime" ~ "number of bee visits")
  
  # estimated means
  p_em = emmeans(model$models$amf_csr, pairwise~p_supply, adjustSigma = TRUE, 
                 adjust = "tukey", type = "response")
  
  csr_em = emmeans(model$models$amf_csr, pairwise~amf_csr, adjustSigma = TRUE, 
                   adjust = "tukey", type = "response")
  
  csr_p_em = emmeans(model$models$amf_csr, pairwise~amf_csr*p_supply, adjustSigma = TRUE, 
                     adjust = "tukey", type = "response")
  
  richness_em = emmeans(model$models$amf_richness, pairwise~amf_richness, adjustSigma = TRUE, 
                        adjust = "tukey", type = "response")
  
  richness_p_em = emmeans(model$models$amf_richness, pairwise~amf_richness*p_supply, adjustSigma = TRUE, 
                          adjust = "tukey", type = "response")
  
  pa_em = emmeans(model$models$amf_pa, pairwise~amf_pa, adjustSigma = TRUE, 
                  adjust = "tukey", type = "response")
  
  pa_p_em = emmeans(model$models$amf_pa, pairwise~amf_pa*p_supply, adjustSigma = TRUE, 
                    adjust = "tukey", type = "response")
  
  
  
  
  
  # combine all contrasts
  contrast_table = bind_rows(p_em$contrasts %>% tidy() %>%
                               rename(adj.p.value = p.value),
                             csr_em$contrasts %>% tidy(),
                             csr_p_em$contrasts %>% tidy(),
                             richness_em$contrasts %>% tidy(),
                             richness_p_em$contrasts %>% tidy(),
                             pa_em$contrasts %>% tidy(),
                             pa_p_em$contrasts %>% tidy()) %>%
    separate(col = contrast, into= c("contrast_a", "contrast_b"), sep =" - | / ") %>%
    mutate(adj.p.value = round(adj.p.value, 3),
           contrast_a = gsub(" l", " - l", contrast_a),
           contrast_a = gsub(" h", " - h", contrast_a),
           contrast_b = gsub(" l", " - l", contrast_b),
           contrast_b = gsub(" h", " - h", contrast_b)) %>%
    mutate(contrast_a = factor(contrast_a, 
                               levels = 
                                 rev(c("high","low",
                                       "none","competitor","stress tolerant", "ruderal", "csr",
                                       "none - low","competitor - low","stress tolerant - low", "ruderal - low", "csr - low",
                                       "none - high","competitor - high","stress tolerant - high", "ruderal - high", "csr - high",
                                       "0 sp", "2 sp", "6 sp",
                                       "0 sp - low", "2 sp - low", "6 sp - low",
                                       "0 sp - high", "2 sp - high", "6 sp - high",
                                       "absent", "present",
                                       "absent - low", "present - low",
                                       "absent - high", "present - high"))),
           contrast_b = factor(contrast_b, 
                               levels = 
                                 (c("high","low",
                                    "none","competitor","stress tolerant", "ruderal", "csr",
                                    "none - low","competitor - low","stress tolerant - low", "ruderal - low", "csr - low",
                                    "none - high","competitor - high","stress tolerant - high", "ruderal - high", "csr - high",
                                    "0 sp", "2 sp", "6 sp",
                                    "0 sp - low", "2 sp - low", "6 sp - low",
                                    "0 sp - high", "2 sp - high", "6 sp - high",
                                    "absent", "present",
                                    "absent - low", "present - low",
                                    "absent - high", "present - high")))) %>%
    mutate(sig = case_when(
      adj.p.value <= 0.001 ~ "***",
      adj.p.value <= 0.01 &  adj.p.value > 0.001 ~ "**",
      adj.p.value <= 0.05 &  adj.p.value > 0.01 ~ "*",
      adj.p.value <= 0.1 &  adj.p.value > 0.05 ~ "",
      adj.p.value > 0.1  ~ ""),
      p = case_when(
        # adj.p.value <= 0.001 ~ "<0.001",
        adj.p.value > 0.05  ~ "",
        TRUE ~ as.character(format(round(adj.p.value, 2), digits = 2))),
      p_sig = paste(p,sig,sep ="")) %>%
    complete(contrast_b, contrast_a) %>%
    mutate(var = var) %>%
    rename("estimate" = 5) %>%
    mutate(estimate = rescale(estimate, to = c(-2,2)))
  
  
  # cld of pairwise comparisons
  
  
  cld_df = lapply(list(p_em = p_em, csr_em = csr_em, csr_p_em = csr_p_em, 
                       richness_em = richness_em, richness_p_em = richness_p_em, 
                       pa_em = pa_em, pa_p_em = pa_p_em), multiple.cld) %>%
    bind_rows(.id = "combination") %>%
    mutate(measurement = unique(contrast_table$var)) %>%
    dplyr::select(combination, amf, p_supply, measurement, pairwise_group)
  
  

  return(list(emmeans = list(p_em = p_em, csr_em = csr_em, csr_p_em = csr_p_em, 
                             richness_em = richness_em, richness_p_em = richness_p_em, 
                             pa_em = pa_em, pa_p_em = pa_p_em),
              cld_df = cld_df,
              
              # contrast_plot = contrast_plot,
              contrast_table = contrast_table
  ))
  
}



multiple.cld = function(x){
  
  amf_col = c(amf = "amf_csr",  amf = "amf_richness", amf = "amf_pa")
  
  cld(x, Letters = letters) %>%
    tidy() %>%
    rename(any_of(amf_col), pairwise_group = .group)
}

## *****************************************************************************
## plot grid ###################################################################
## *****************************************************************************

plot_grid_function <- function(variables=measurement, subset=combination, colors, shapes = c(21, 19), data, rows = 1){
  
  # df.tmp <- data
  
  # subset data frame
  plot_df <- data %>%
    dplyr::filter(measurement %in% variables,
                  combination %in% subset) 
  
  levels.tmp = levels(plot_df$measurement)
  levels = levels.tmp[levels.tmp %in% variables]
  
  labels <- data.frame(measurement=levels,
                       label=LETTERS[1:length(levels)]) %>%
    mutate(measurement = factor(measurement, levels = levels ))
  
  # plot
  plot_grid.tmp <- ggplot(plot_df, 
                          aes(x=amf, y=mean, fill=amf, 
                              color=amf, shape=p_supply)) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.0, linewidth =0.85,
                  position=position_dodge(width=0.5)) +
    geom_point(size=3, position=position_dodge(width=0.5), fill="white", stroke=1) +
    scale_color_manual(values= colors)+
    scale_shape_manual(values= shapes) +
    scale_y_continuous(expand = expansion(mult = c(0.15, .15)),
                       breaks = scales::breaks_extended(6)) +
    # ylab("") +
    xlab("AMF treatment") +
    facet_wrap(~measurement, scales="free_y", nrow=rows, 
               strip.position = "left")  +
    theme_bw()  +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(angle = 40, vjust = 1, hjust=1),
          strip.text = element_text(face="bold", size = 12),
          axis.title.x = element_text(face="bold"),
          axis.title.y=element_blank())   + 
    geom_text(data = labels, 
              aes(x = -Inf, y = Inf, label = label, 
                  group = measurement),
              size = 5,
              # face = "bold",
              hjust = -0.5,
              vjust = 1.4,
              inherit.aes = FALSE)#+ 
  
  
  if(length(subset) > 1){
    plot_grid <- plot_grid.tmp + labs(shape="P supply", colour="AMF")   
  } else if(subset == "amf_csr_p"){
    plot_grid <- plot_grid.tmp + labs(shape="P supply", colour=expression(AMF["CSR"]))   
  } else if(subset == "amf_pa_p"){
    plot_grid <- plot_grid.tmp + labs(shape="P supply", colour=expression(AMF["pa"]))   
  } else if(subset == "amf_richness_p"){
    plot_grid <- plot_grid.tmp + labs(shape="P supply", colour=expression(AMF["richness"]))   
  } else if(subset == "amf_csr"){
    plot_grid <- plot_grid.tmp + labs(shape="P supply", colour=expression(AMF["CSR"]))   
  } 
  

  return(plot_grid)
}



## *****************************************************************************
## fill empty cells with NAs ###################################################
## *****************************************************************************

NA_as_0_or_1 <- function(x){
  ifelse(is.na(x), 0, ifelse(x == 0, 0, 1))}
