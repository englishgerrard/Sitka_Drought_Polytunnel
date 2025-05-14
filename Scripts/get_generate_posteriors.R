source('./Scripts/_PACKAGES.R')
source('./Scripts/_FUNCTIONS.R')

lapply(1:17, function(x, lwr_ci = 0.05, upr_ci = 0.95){
VI <- dependent_vars[x]
mod <- readRDS(paste0('./models/',VI,'_poly2.rds'))
# draw from rthe posterior
posterior_samples <- as_draws_df(mod)


## calculate relavent clone 6 coefficents by combining pos distrubutions using
# c6 = -(c1,c2,c3,c4,c5)

# Calculate the posterior for clone 6
posterior_samples$b_clone6 <- -(
  posterior_samples$b_clone1 + 
    posterior_samples$b_clone2 + 
    posterior_samples$b_clone3 + 
    posterior_samples$b_clone4 + 
    posterior_samples$b_clone5
)

# calculate posterior for clone effect over time poly1

posterior_samples$`b_polydays_scaled21:clone6` <- -(
  posterior_samples$`b_polydays_scaled21:clone1` + 
    posterior_samples$`b_polydays_scaled21:clone2` + 
    posterior_samples$`b_polydays_scaled21:clone3` + 
    posterior_samples$`b_polydays_scaled21:clone4` + 
    posterior_samples$`b_polydays_scaled21:clone5`
)

# calculate posterior for clone effect over time poly2
#######################################################
posterior_samples$`b_polydays_scaled22:clone6` <- -(
  posterior_samples$`b_polydays_scaled22:clone1` + 
    posterior_samples$`b_polydays_scaled22:clone2` + 
    posterior_samples$`b_polydays_scaled22:clone3` + 
    posterior_samples$`b_polydays_scaled22:clone4` + 
    posterior_samples$`b_polydays_scaled22:clone5`
)

# calculate posterior for treatment effect over time poly1
#######################################################
posterior_samples$`b_treatmentD:polydays_scaled21:clone6` <- -(
  posterior_samples$`b_treatmentD:polydays_scaled21:clone1` + 
    posterior_samples$`b_treatmentD:polydays_scaled21:clone2` + 
    posterior_samples$`b_treatmentD:polydays_scaled21:clone3` + 
    posterior_samples$`b_treatmentD:polydays_scaled21:clone4` + 
    posterior_samples$`b_treatmentD:polydays_scaled21:clone5`
)

# calculate posterior for treatment effect over time poly2
#######################################################
posterior_samples$`b_treatmentD:polydays_scaled22:clone6` <- -(
  posterior_samples$`b_treatmentD:polydays_scaled22:clone1` + 
    posterior_samples$`b_treatmentD:polydays_scaled22:clone2` + 
    posterior_samples$`b_treatmentD:polydays_scaled22:clone3` + 
    posterior_samples$`b_treatmentD:polydays_scaled22:clone4` + 
    posterior_samples$`b_treatmentD:polydays_scaled22:clone5`
)


fixed_effects <-  posterior_samples %>%
  select(starts_with("b")) %>%
  pivot_longer(cols = everything(), names_to = "model_term", values_to = "estimate") %>%
  group_by(model_term) %>%
  summarise(
    lower = quantile(estimate, lwr_ci),
    upper = quantile(estimate, upr_ci),
    estimate_mean = mean(estimate)) 

p1 <- ggplot(fixed_effects, aes(x = estimate_mean, y = model_term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper))

print(p1)


saveRDS(posterior_samples, paste0('./Data/VI/',VI,'_posterior_samples2.rds'))
write.csv(fixed_effects, paste0('./Data/VI/',VI,'_fixed_effects2.csv'))




# ### archive code for absolute values 
# posterior_samples$b_clone1_abs <- posterior_samples$b_Intercept + posterior_samples$b_clone1 
# posterior_samples$b_clone2_abs <- posterior_samples$b_Intercept + posterior_samples$b_clone2 
# posterior_samples$b_clone3_abs <- posterior_samples$b_Intercept + posterior_samples$b_clone3 
# posterior_samples$b_clone4_abs <- posterior_samples$b_Intercept + posterior_samples$b_clone4 
# posterior_samples$b_clone5_abs <- posterior_samples$b_Intercept + posterior_samples$b_clone5  
# posterior_samples$b_clone6_abs <- posterior_samples$b_Intercept + posterior_samples$b_clone6 
# 
# posterior_samples$b_ave_clone <- (
#   posterior_samples$b_clone1 + 
#   posterior_samples$b_clone2 + 
#   posterior_samples$b_clone3 + 
#   posterior_samples$b_clone4 + 
#   posterior_samples$b_clone5 +
#   posterior_samples$b_clone6)/6
# 
# 
# 
# 
# 
# posterior_samples$`b_polydays_scaled21:clone1_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_polydays_scaled21:clone1` 
# posterior_samples$`b_polydays_scaled21:clone2_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_polydays_scaled21:clone2` 
# posterior_samples$`b_polydays_scaled21:clone3_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_polydays_scaled21:clone3` 
# posterior_samples$`b_polydays_scaled21:clone4_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_polydays_scaled21:clone4` 
# posterior_samples$`b_polydays_scaled21:clone5_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_polydays_scaled21:clone5` 
# posterior_samples$`b_polydays_scaled21:clone6_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_polydays_scaled21:clone6` 
# 
# posterior_samples$`b_polydays_scaled21:ave_clone` <- (
#   posterior_samples$`b_polydays_scaled21:clone1` + 
#     posterior_samples$`b_polydays_scaled21:clone2` + 
#     posterior_samples$`b_polydays_scaled21:clone3` + 
#     posterior_samples$`b_polydays_scaled21:clone4` + 
#     posterior_samples$`b_polydays_scaled21:clone5` +
#     posterior_samples$`b_polydays_scaled21:clone6`)/6
# 
# 
# 
# 
# 
# 
# posterior_samples$`b_polydays_scaled22:clone1_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_polydays_scaled22:clone1` 
# posterior_samples$`b_polydays_scaled22:clone2_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_polydays_scaled22:clone2` 
# posterior_samples$`b_polydays_scaled22:clone3_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_polydays_scaled22:clone3` 
# posterior_samples$`b_polydays_scaled22:clone4_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_polydays_scaled22:clone4` 
# posterior_samples$`b_polydays_scaled22:clone5_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_polydays_scaled22:clone5` 
# posterior_samples$`b_polydays_scaled22:clone6_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_polydays_scaled22:clone6` 
# 
# posterior_samples$`b_polydays_scaled22:ave_clone` <- (
#   posterior_samples$`b_polydays_scaled22:clone1` + 
#     posterior_samples$`b_polydays_scaled22:clone2` + 
#     posterior_samples$`b_polydays_scaled22:clone3` + 
#     posterior_samples$`b_polydays_scaled22:clone4` + 
#     posterior_samples$`b_polydays_scaled22:clone5` +
#     posterior_samples$`b_polydays_scaled22:clone6`)/6
# #######################################################
# 
# 
# 
# 
# 
# 
# posterior_samples$`b_treatmentD:polydays_scaled21:clone1_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_treatmentD:polydays_scaled21:clone1` 
# posterior_samples$`b_treatmentD:polydays_scaled21:clone2_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_treatmentD:polydays_scaled21:clone2` 
# posterior_samples$`b_treatmentD:polydays_scaled21:clone3_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_treatmentD:polydays_scaled21:clone3` 
# posterior_samples$`b_treatmentD:polydays_scaled21:clone4_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_treatmentD:polydays_scaled21:clone4` 
# posterior_samples$`b_treatmentD:polydays_scaled21:clone5_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_treatmentD:polydays_scaled21:clone5` 
# posterior_samples$`b_treatmentD:polydays_scaled21:clone6_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_treatmentD:polydays_scaled21:clone6` 
# 
# posterior_samples$`b_treatmentD:polydays_scaled21:ave_clone` <- (
#   posterior_samples$`b_treatmentD:polydays_scaled21:clone1` + 
#     posterior_samples$`b_treatmentD:polydays_scaled21:clone2` + 
#     posterior_samples$`b_treatmentD:polydays_scaled21:clone3` + 
#     posterior_samples$`b_treatmentD:polydays_scaled21:clone4` + 
#     posterior_samples$`b_treatmentD:polydays_scaled21:clone5` +
#     posterior_samples$`b_treatmentD:polydays_scaled21:clone6`)/6
# #######################################################
# 
# 
# posterior_samples$`b_treatmentD:polydays_scaled22:clone1_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_treatmentD:polydays_scaled22:clone1` 
# posterior_samples$`b_treatmentD:polydays_scaled22:clone2_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_treatmentD:polydays_scaled22:clone2` 
# posterior_samples$`b_treatmentD:polydays_scaled22:clone3_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_treatmentD:polydays_scaled22:clone3` 
# posterior_samples$`b_treatmentD:polydays_scaled22:clone4_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_treatmentD:polydays_scaled22:clone4` 
# posterior_samples$`b_treatmentD:polydays_scaled22:clone5_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_treatmentD:polydays_scaled22:clone5` 
# posterior_samples$`b_treatmentD:polydays_scaled22:clone6_abs` <- posterior_samples$b_Intercept + posterior_samples$`b_treatmentD:polydays_scaled22:clone6` 
# 
# posterior_samples$`b_treatmentD:polydays_scaled22:ave_clone` <- (
#   posterior_samples$`b_treatmentD:polydays_scaled22:clone1` + 
#     posterior_samples$`b_treatmentD:polydays_scaled22:clone2` + 
#     posterior_samples$`b_treatmentD:polydays_scaled22:clone3` + 
#     posterior_samples$`b_treatmentD:polydays_scaled22:clone4` + 
#     posterior_samples$`b_treatmentD:polydays_scaled22:clone5` +
#     posterior_samples$`b_treatmentD:polydays_scaled22:clone6`)/6
# #######################################################




# em_clone <- emmeans(
#   mod, 
#   specs = pairwise ~ treatment | days_scaled| clone,
#   at = list(days_scaled = seq(min(data$days_scaled), 
#                               max(data$days_scaled), 
#                               length = 8))  # Evaluates at original weeks
# )
# 
# contrasts_df <- as.data.frame(em_clone$contrasts)
# p2<- ggplot(contrasts_df, aes(x = days_scaled, y = estimate, color = contrast)) +
#   geom_point() +
#   geom_line() +
#   geom_errorbar(aes(ymin = lower.HPD, ymax = upper.HPD), width = 0.2) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(title = "Drought vs. Control: Weekly Contrasts by Clone",
#        y = "VI Difference (Drought - Control)",
#        x = "Days Scaled") +
#   facet_wrap(~ clone) +  # Facet by clone
#   theme_minimal() +
#   theme(legend.position = "bottom")
# p2
})
