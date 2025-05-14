# big plot 
get_em_treat_days <- function(x =1, num.contrast = 8 ){
VI <- dependent_vars[x]
mod <- readRDS(paste0('./models/',VI,'_poly2.rds'))
fe <- read.csv(paste0('./Data/VI/',VI,'_fixed_effects2.csv'))
ps <- readRDS(paste0('./Data/VI/',VI,'_posterior_samples2.rds'))


em_week <- emmeans(
  mod, 
  specs = pairwise ~ treatment | days_scaled | clone,
  at = list(days_scaled = seq(min(data$days_scaled), 
                              max(data$days_scaled), 
                              length = num.contrast))  
)

df <- as.data.frame(em_week$emmeans) %>%
  mutate(VI =dependent_vars[x] ) %>%
  mutate(VI_type = ifelse(x %in% 1:5, 'Green', ifelse(x %in% 6:11, 'Pigment', 'Water')))

return(df)}

ave_data_long<- data_long %>%
  group_by(treatment,clone, days_scaled, VI, VI_type) %>%
  summarise(ave_value = mean(value))

lines <- bind_rows(lapply(1:17, get_em_treat_days, num.contrast = 8))
green_lines <- lines %>% filter(VI_type == 'Green')
pigment_lines <- lines %>% filter(VI_type == 'Pigment')
water_lines <- lines %>% filter(VI_type == 'Water')

                 
ggplot(filter(ave_data_long, VI_type == 'Green'), aes(x = days_scaled,  colour = treatment))+
  geom_point(aes(y=ave_value)) +
  geom_line(data = green_lines, aes( y = emmean))+
  facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
  ncol(6) 

ggplot(filter(ave_data_long, VI_type == 'Pigment'), aes(x = days_scaled,  colour = treatment))+
  geom_point(aes(y=ave_value)) +
  geom_line(data = pigment_lines, aes( y = emmean))+
  facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
  ncol(6) 

ggplot(filter(ave_data_long, VI_type == 'Water'), aes(x = days_scaled,  colour = treatment))+
  geom_point(aes(y=ave_value)) +
  geom_line(data = water_lines, aes( y = emmean))+
  facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
  ncol(6) 



gp <- ggplot(filter(data_long, VI_type == 'Green'), aes(x = days_scaled,  colour = treatment))+
  geom_point(aes(y=value)) +
  geom_line(data = green_lines, aes( y = emmean))+
  facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
  ncol(6) 

pp <- ggplot(filter(data_long, VI_type == 'Pigment'), aes(x = days_scaled,  colour = treatment))+
  geom_point(aes(y=value)) +
  geom_line(data = pigment_lines, aes( y = emmean))+
  facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
  ncol(6) 

wp <- ggplot(filter(data_long, VI_type == 'Water'), aes(x = days_scaled,  colour = treatment))+
  geom_point(aes(y=value)) +
  geom_line(data = water_lines, aes( y = emmean))+
  facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
  ncol(6) 
