library(plyr)
library(tidymv)
library(Hmisc)
library(ggplot2)
library(knitr)
library(kableExtra)
library(data.table)
library(psycho)
library(lme4)
library(sjPlot)
library(afex)
library(wesanderson)
library(viridis)
library(showtext)
library(tidyverse)
library(mgcv)
library(itsadug)
library(RColorBrewer)
library(ggcorrplot)
library(devtools)
library(factoextra)
library(tidyjson)
library(purrr)
library(broom)
library(zoo)
library(psych)
library(patchwork)
library(emmeans)
library(ggpubr)
library(stats)
library(broom.mixed)
library(pracma)
library(afex)

set.seed(478)
load("power_frontal-theta_betas.RData")
frontal.theta.betas <- power.betas
frontal.theta.results.betas <- results.betas
frontal.theta.rt.betas <- power.rt.betas
frontal.theta.results.rt.betas <- results.rt.betas
rm(power.betas, results.betas, power.rt.betas, results.rt.betas)
load("power_posterior-alpha_betas.RData")
posterior.alpha.betas <- power.betas
posterior.alpha.results.betas <- results.betas
posterior.alpha.rt.betas <- power.rt.betas
posterior.alpha.results.rt.betas <- results.rt.betas
rm(power.betas, results.betas, power.rt.betas, results.rt.betas)
load("../pupil/pupil_betas.RData")
pupil.results.betas <- results.betas
pupil.results.rt.betas <- results.rt.betas
rm(results.betas, results.rt.betas)

get_pupil_clusters <- function(results.betas) {
  sfreq <- 0.01
  clusters <- results.betas %>% 
    group_by(term) %>% 
    mutate(time_diff = c(NA, diff(time))) %>% 
    mutate(time_diff_back = lead(time_diff)) %>% 
    mutate(time_diff = round(time_diff, 2),
            time_diff_back = round(time_diff_back, 2)) %>% 
    dplyr::filter(time_diff > sfreq | time_diff_back > sfreq | is.na(time_diff) | is.na(time_diff_back)) %>% 
    ungroup() %>% 
    select(term, time, time_diff, time_diff_back) %>% 
    arrange(term, time) %>% 
    mutate(is_solo_point = ifelse(is.na(time_diff) & time_diff_back > sfreq, T, F)) %>% 
    group_by(term, is_solo_point) %>% 
    mutate(index_parity = if_else(row_number() %% 2 == 1, "start", "end")) %>% 
    mutate(time_period = round(row_number()+1/2) / 2) %>% 
    ungroup() %>% 
    select(-time_diff, -time_diff_back) %>% 
    pivot_wider(names_from=c("index_parity"),
                values_from = "time") %>% 
    select(-time_period, -is_solo_point) %>% 
    mutate(cluster_size = end-start) %>% 
    mutate(cluster_id = row_number())
  return(clusters)
}

get_power_clusters <- function(results.betas) {
  sfreq <- 0.008
  clusters <- results.betas %>% 
    group_by(term) %>% 
    mutate(time_diff = c(NA, diff(time))) %>% 
    mutate(time_diff_back = lead(time_diff)) %>% 
    mutate(time_diff = round(time_diff, 3),
            time_diff_back = round(time_diff_back, 3)) %>% 
    dplyr::filter(time_diff > sfreq | time_diff_back > sfreq | is.na(time_diff) | is.na(time_diff_back)) %>% 
    ungroup() %>% 
    select(term, time, time_diff, time_diff_back) %>% 
    arrange(term, time) %>% 
    dplyr::filter(time_diff_back == sfreq | time_diff == sfreq) %>% 
    mutate(is_solo_point = ifelse(is.na(time_diff) & time_diff_back > sfreq, T, F)) %>% 
    group_by(term, is_solo_point) %>% 
    mutate(index_parity = if_else(row_number() %% 2 == 1, "start", "end")) %>% 
    mutate(time_period = round(row_number()+1/2) / 2) %>% 
    ungroup() %>% 
    select(-time_diff, -time_diff_back) %>% 
    pivot_wider(names_from=c("index_parity"),
                values_from = "time") %>% 
    select(-time_period, -is_solo_point) %>% 
    mutate(cluster_size = end-start) %>% 
    mutate(cluster_id = row_number())
  return(clusters)
}

### 

frontal.theta.clusters <- get_power_clusters(frontal.theta.results.betas)
posterior.alpha.clusters <- get_power_clusters(posterior.alpha.results.betas)
pupil.clusters <- get_pupil_clusters(pupil.results.betas)

### 

clusters.all <- pupil.clusters %>% 
  left_join(frontal.theta.clusters, 
            by=c("term"), 
            suffix=c(".pupil", ".frontal_theta"), 
            relationship = "many-to-many",
            multiple="all") %>% 
  left_join(posterior.alpha.clusters %>% 
              rename_with(~ paste(., "posterior_alpha", sep = ".")) %>% 
              rename(term = term.posterior_alpha), 
            by=c("term"), 
            relationship = "many-to-many",
            multiple="all") %>% 
  mutate(analysis_id = row_number()) %>% 
  mutate(comparison = paste(term,
                            "\npupil", cluster_id.pupil,
                            "v. theta", cluster_id.frontal_theta,
                            "v. alpha", cluster_id.posterior_alpha, sep=" "))

###

pupil.cluster.sub <- pupil.betas %>% 
  left_join(pupil.clusters %>% 
                mutate(term = case_when(term == "Memory strength (Weak v. Strong)" ~ "association_strengthWeak (1x)",
                          term == "Mismatch effect x Memory strength" ~ "association_strengthWeak (1x):is_mismatch",
                          term == "Mismatch v. Match" ~ "is_mismatch",
                          .default="")), 
            by="term",
            relationship = "many-to-many",
            suffix=c(".sub", ".cluster")) %>% 
  dplyr::filter(time >= start & time <= end) %>% 
  group_by(subjectId, cluster_id, term, start, end) %>% 
  summarize(estimate = mean(estimate)) %>% 
  ungroup() %>% 
  mutate(term = case_when(term == "association_strengthWeak (1x)" ~ "Memory strength (Weak v. Strong)",
                          term == "association_strengthWeak (1x):is_mismatch" ~ "Mismatch effect x Memory strength",
                          term == "is_mismatch" ~ "Mismatch v. Match",
                          .default=""))

frontal.theta.cluster.sub <- frontal.theta.betas %>% 
  left_join(frontal.theta.clusters %>% 
                mutate(term = case_when(term == "Memory strength (Weak v. Strong)" ~ "association_strengthWeak (1x)",
                          term == "Mismatch effect x Memory strength" ~ "association_strengthWeak (1x):is_mismatch",
                          term == "Mismatch v. Match" ~ "is_mismatch",
                          .default="")), 
            by="term",
            relationship = "many-to-many",
            suffix=c(".sub", ".cluster")) %>% 
  dplyr::filter(time >= start & time <= end) %>% 
  group_by(subjectId, cluster_id, term, start, end) %>% 
  summarize(estimate = mean(estimate)) %>% 
  ungroup() %>% 
  mutate(term = case_when(term == "association_strengthWeak (1x)" ~ "Memory strength (Weak v. Strong)",
                          term == "association_strengthWeak (1x):is_mismatch" ~ "Mismatch effect x Memory strength",
                          term == "is_mismatch" ~ "Mismatch v. Match",
                          .default=""))

posterior.alpha.cluster.sub <- posterior.alpha.betas %>% 
  left_join(posterior.alpha.clusters %>% 
                mutate(term = case_when(term == "Memory strength (Weak v. Strong)" ~ "association_strengthWeak (1x)",
                          term == "Mismatch effect x Memory strength" ~ "association_strengthWeak (1x):is_mismatch",
                          term == "Mismatch v. Match" ~ "is_mismatch",
                          .default="")), 
            relationship = "many-to-many",
            by="term",
            suffix=c(".sub", ".cluster")) %>% 
  dplyr::filter(time >= start & time <= end) %>% 
  group_by(subjectId, cluster_id, term, start, end) %>% 
  summarize(estimate = mean(estimate)) %>% 
  ungroup() %>% 
  mutate(term = case_when(term == "association_strengthWeak (1x)" ~ "Memory strength (Weak v. Strong)",
                          term == "association_strengthWeak (1x):is_mismatch" ~ "Mismatch effect x Memory strength",
                          term == "is_mismatch" ~ "Mismatch v. Match",
                          .default=""))
###

clusters.all.sub <- clusters.all %>% 
  left_join(pupil.cluster.sub %>% 
              rename_with(~ paste(., "pupil", sep = ".")) %>% 
              rename(term = term.pupil,
                     subjectId = subjectId.pupil), 
            relationship = "many-to-many",
            by=c("term", "cluster_id.pupil", "start.pupil", "end.pupil"),
            multiple="all") %>% 
  left_join(frontal.theta.cluster.sub %>% 
              rename_with(~ paste(., "frontal_theta", sep = ".")) %>% 
              rename(term = term.frontal_theta,
                     subjectId = subjectId.frontal_theta), 
            relationship = "many-to-many",
            by=c("subjectId", "term", "cluster_id.frontal_theta", "start.frontal_theta", "end.frontal_theta"),
            multiple="all") %>% 
  left_join(posterior.alpha.cluster.sub %>% 
              rename_with(~ paste(., "posterior_alpha", sep = ".")) %>% 
              rename(term = term.posterior_alpha,
                     subjectId = subjectId.posterior_alpha), 
            relationship = "many-to-many",
            by=c("subjectId", "term", "cluster_id.posterior_alpha", "start.posterior_alpha", "end.posterior_alpha"),
            multiple="all")

### 
save(clusters.all, file="spectral_pupil_clusters.RData")