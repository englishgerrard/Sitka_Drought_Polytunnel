# big plot 
source('./Scripts/_PACKAGES.R')
source('./Scripts/_FUNCTIONS.R')

ave_data_long<- data_long %>%
  group_by(treatment, days_scaled, VI, VI_type) %>%
  summarise(ave_value = mean(value),
            se_value = sd(value, na.rm = TRUE) / sqrt(n())) 


lines <- bind_rows(lapply(1:dv_l, get_em_treat_days, num.contrast = 8, i = 1))
green_lines <- lines %>% filter(VI_type == 'Green')
pigment_lines <- lines %>% filter(VI_type == 'Pigment')
water_lines <- lines %>% filter(VI_type == 'Water')


VI_plot <- function(VI.type = 'Green' ){
ggplot(filter(ave_data_long, VI_type == VI.type), aes(x = days_scaled)) +
  geom_errorbar(data = filter(ave_data_long, VI_type == VI.type),
                aes(ymin = ave_value - se_value, ymax = ave_value + se_value, y = ave_value), 
                width = 0.1) +
  geom_ribbon(data = filter(lines, VI_type == VI.type), aes(fill = treatment, ymin = lower.HPD, ymax = upper.HPD), alpha = 0.1) +
  geom_point(aes(y = ave_value, colour = treatment)) +
  geom_line(data = filter(lines, VI_type == VI.type), aes(colour = treatment, y = emmean)) +
  facet_wrap(~VI, scales = 'free_y') +
  theme_bw()
}

VI_plot(VI.type = 'Green')  
VI_plot(VI.type = 'Pigment')  
VI_plot(VI.type = 'Water')  





 ##### OLD CODE ######
# lines <- bind_rows(lapply(1:dv_l, get_em_treat_days_clone, num.contrast = 8, i = 1))
# green_lines <- lines %>% filter(VI_type == 'Green')
# pigment_lines <- lines %>% filter(VI_type == 'Pigment')
# water_lines <- lines %>% filter(VI_type == 'Water')
# 
# 
# 
# ggplot(filter(ave_data_long, VI_type == 'Green'), aes(x = days_scaled,  colour = treatment))+
#   #geom_point(data = filter(data_long, VI_type == 'Green'), aes(y=value), alpha = 0.2) +
#   geom_errorbar(data = filter(ave_data_long, VI_type == 'Green'),
#                 aes(ymin= ave_value - se_value, ymax = ave_value + se_value)) +
#   geom_point(aes(y=ave_value)) +
#   geom_line(data = green_lines, aes( y = emmean))+
#   facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
#   ncol(6) 
# 
# ggplot(filter(ave_data_long, VI_type == 'Pigment'), aes(x = days_scaled,  colour = treatment))+
#   #geom_point(data = filter(data_long, VI_type == 'Pigment'), aes(y=value), alpha = 0.2) +
#   geom_errorbar(data = filter(ave_data_long, VI_type == 'Pigment'),
#                 aes(ymin= ave_value - se_value, ymax = ave_value + se_value)) +
#   geom_point(aes(y=ave_value)) +
#   geom_line(data = pigment_lines, aes( y = emmean))+
#   facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
#   ncol(6) 
# 
# ggplot(filter(ave_data_long, VI_type == 'Water'), aes(x = days_scaled,  colour = treatment))+
#   #geom_point(data = filter(data_long, VI_type == 'Water'), aes(y=value), alpha = 0.2) +
#   geom_errorbar(data = filter(ave_data_long, VI_type == 'Water'),
#                 aes(ymin= ave_value - se_value, ymax = ave_value + se_value)) +
#   geom_point(aes(y=ave_value)) +
#   geom_line(data = water_lines, aes( y = emmean))+
#   facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
#   ncol(6) 
# 
# 
# 
# gg <- ggplot(filter(ave_data_long, VI_type == 'Green'), aes(x = days_scaled,  colour = treatment))+
#   #geom_point(data = filter(data_long, VI_type == 'Green'), aes(y=value), alpha = 0.2) +
#   geom_errorbar(data = filter(ave_data_long, VI_type == 'Green'),
#                 aes(ymin= ave_value - se_value, ymax = ave_value + se_value)) +
#   geom_point(aes(y=ave_value)) +
#   geom_line(data = green_lines, aes( y = emmean))+
#   facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
#   ncol(6) 
# 
# gp <- ggplot(filter(ave_data_long, VI_type == 'Pigment'), aes(x = days_scaled,  colour = treatment))+
#   #geom_point(data = filter(data_long, VI_type == 'Pigment'), aes(y=value), alpha = 0.2) +
#   geom_errorbar(data = filter(ave_data_long, VI_type == 'Pigment'),
#                 aes(ymin= ave_value - se_value, ymax = ave_value + se_value)) +
#   geom_point(aes(y=ave_value)) +
#   geom_line(data = pigment_lines, aes( y = emmean))+
#   facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
#   ncol(6) 
# 
# gw <- ggplot(filter(ave_data_long, VI_type == 'Water'), aes(x = days_scaled,  colour = treatment))+
#   #geom_point(data = filter(data_long, VI_type == 'Water'), aes(y=value), alpha = 0.2) +
#   geom_errorbar(data = filter(ave_data_long, VI_type == 'Water'),
#                 aes(ymin= ave_value - se_value, ymax = ave_value + se_value)) +
#   geom_point(aes(y=ave_value)) +
#   geom_line(data = water_lines, aes( y = emmean))+
#   facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
#   ncol(6) 
# 
