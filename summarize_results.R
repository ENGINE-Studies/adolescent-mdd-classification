require(ggplot2)
require(reshape2)
require(bootstrap)
require(tidymodels)
require(vip)

## set paths
path_out <- '/path/to/results/'

# load output
load(paste0(path_out, 'results.Rdata'))

## Boxplot of performance metrics across CV folds
performance_df <- do.call(rbind, rf_fit$.metrics)

performance_df$.metric <- factor(performance_df$.metric, 
                                 levels = c("roc_auc", "pr_auc", "sens", "spec", "ppv", "npv"))

ggplot(performance_df, aes(.metric, .estimate)) + 
  #geom_violin() + 
  geom_boxplot(fill='lightgrey', colour = 'black') + 
  theme_bw()


## mean and sd of performance metrics
performance_df %>%
  group_by(.metric) %>%
  summarise(mean = mean(.estimate, na.rm=T),
            sd = sd(.estimate, na.rm=T))


## Permutation test results

# ROC
mean_roc_observed <- collect_metrics(rf_fit) %>%
  filter(.metric=='roc_auc') %>%
  select(mean)

sum(permuted_performance_by_metric$roc_auc$mean >= mean_roc_observed$mean) / n_perm


# PR
mean_pr_observed <- collect_metrics(rf_fit) %>%
  filter(.metric=='pr_auc') %>%
  select(mean)

sum(permuted_performance_by_metric$pr_auc$mean >= mean_pr_observed$mean) / n_perm


# Sensitivity
mean_sens_observed <- collect_metrics(rf_fit) %>%
  filter(.metric=='sens') %>%
  select(mean)

sum(permuted_performance_by_metric$sens$mean >= mean_sens_observed$mean) / n_perm


# Specificity
mean_spec_observed <- collect_metrics(rf_fit) %>%
  filter(.metric=='spec') %>%
  select(mean)

sum(permuted_performance_by_metric$spec$mean >= mean_spec_observed$mean) / n_perm

# PPV
mean_ppv_observed <- collect_metrics(rf_fit) %>%
  filter(.metric=='ppv') %>%
  select(mean)

sum(permuted_performance_by_metric$ppv$mean >= mean_ppv_observed$mean) / n_perm

# NPV
mean_npv_observed <- collect_metrics(rf_fit) %>%
  filter(.metric=='npv') %>%
  select(mean)

sum(permuted_performance_by_metric$npv$mean >= mean_npv_observed$mean) / n_perm

## variable importance: export as 5x7
set.seed(123)
rf_workflow %>% 
  fit(data_clf) %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 10)

# top 6 for simplicity
m <- melt(data_clf[, c('MDD', 'T7_P7_Coherence_Beta', 'P4_O2_Coherence_Beta',
                       'Cz_Pz_Coherence_Beta', 'Fz_Cz_Coherence_Delta', 
                       'Fp2_F8_Coherence_Alpha', 'P3_O1_Coherence_Theta')])

# top 10 for completeness: export as 7x10
m <- melt(data_clf[, c('MDD', 'T7_P7_Coherence_Beta', 'P4_O2_Coherence_Beta',
                       'Cz_Pz_Coherence_Beta', 'Fz_Cz_Coherence_Delta', 
                       'Fp2_F8_Coherence_Alpha', 'P3_O1_Coherence_Theta',
                       'T7_P7_Coherence_Theta', 'F7_T7_Coherence_Beta',
                       'C4_P4_Coherence_Beta', 'Fp1_F7_Coherence_Alpha')])

# Important variables by diagnosis
ggplot(m, aes(MDD, value, fill=MDD)) + 
  geom_boxplot() + 
  ylim(0,1) + 
  facet_wrap(~variable, nrow=2) + 
  theme_bw() + 
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))


# ROC curve
collect_predictions(rf_fit) %>%
  roc_curve(MDD, .pred_0) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()





