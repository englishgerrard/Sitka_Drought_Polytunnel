source('./Scripts/_FUNCTIONS.R')

cont <- bind_rows(lapply(1:17, get_em_treat_days, num.contrast = 10, i = 2))


ggplot(filter(cont, VI_type == 'Green'), aes(x = days_scaled, y = estimate, group = VI)) +
  geom_ribbon(aes(ymin=lower.HPD.90, ymax = upper.HPD.90), alpha = 0.2, colour = 'green') +
  geom_ribbon(aes(ymin=lower.HPD, ymax = upper.HPD.95), alpha = 0.2, colour = 'steelblue') +
  #geom_ribbon(aes(ymin=lower.HPD, ymax = upper.HPD), alpha = 0.2, colour = 'blue') +
  geom_ribbon(aes(ymin=lower.HPD.99, ymax = upper.HPD.99),alpha = 0.2, colour = 'red') +
  #geom_point(aes(colour = effect))+
  geom_line() + 
  geom_hline(yintercept = 0) +
  facet_wrap(~VI, scales = 'free_y')

ggplot(filter(cont, VI_type == 'Pigment'), aes(x = days_scaled, y = estimate, group = VI)) +
  geom_ribbon(aes(ymin=lower.HPD.90, ymax = upper.HPD.90), alpha = 0.2, colour = 'green') +
  geom_ribbon(aes(ymin=lower.HPD, ymax = upper.HPD.95), alpha = 0.2, colour = 'steelblue') +
  #geom_ribbon(aes(ymin=lower.HPD, ymax = upper.HPD), alpha = 0.2, colour = 'blue') +
  geom_ribbon(aes(ymin=lower.HPD.99, ymax = upper.HPD.99),alpha = 0.2, colour = 'red') +
  #geom_point(aes(colour = effect))+
  geom_line() + 
  geom_hline(yintercept = 0) +
  facet_wrap(~VI, scales = 'free_y')


cont_clone <- bind_rows(lapply(1:17, get_em_treat_days_clone, num.contrast = 8, i = 2)) 




alpha = 0.1
ggplot(filter(cont_clone, VI_type == 'Green'), aes(x = days_scaled, y = estimate, group=clone, fill = clone)) +
  geom_ribbon(aes(ymin=lower.HPD.85, ymax = upper.HPD.85), alpha = alpha) +
  geom_ribbon(aes(ymin=lower.HPD.90, ymax = upper.HPD.90), alpha = alpha) +
  geom_ribbon(aes(ymin=lower.HPD, ymax = upper.HPD.95, colour = clone), alpha = alpha, linetype  = 2) +
  geom_ribbon(aes(ymin=lower.HPD.85, ymax = upper.HPD.85), alpha = alpha) +
  geom_ribbon(aes(ymin=lower.HPD.99, ymax = upper.HPD.99), alpha = alpha) +
  #geom_ribbon(aes(ymin=lower.HPD.99, ymax = upper.HPD.99),alpha = 0.02, fill = 'red') +
  #geom_point()+
  geom_line(aes(colour = clone )) + 
  geom_hline(yintercept = 0) +
  facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
  theme_bw()

ggplot(filter(cont_clone, VI_type == 'Water'), aes(x = days_scaled, y = estimate, group=clone, fill = clone)) +
  geom_ribbon(aes(ymin=lower.HPD.85, ymax = upper.HPD.85), alpha = alpha) +
  geom_ribbon(aes(ymin=lower.HPD.90, ymax = upper.HPD.90), alpha = alpha) +
  geom_ribbon(aes(ymin=lower.HPD, ymax = upper.HPD.95, colour = clone), alpha = alpha, linetype  = 2) +
  geom_ribbon(aes(ymin=lower.HPD.99, ymax = upper.HPD.99), alpha = alpha) +
  #geom_ribbon(aes(ymin=lower.HPD.99, ymax = upper.HPD.99),alpha = 0.02, fill = 'red') +
  #geom_point()+
  geom_line(aes(colour = clone )) + 
  geom_hline(yintercept = 0) +
  facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
  theme_bw()

ggplot(filter(cont_clone, VI_type == 'Pigment'), aes(x = days_scaled, y = estimate, group=clone, fill = clone)) +
  geom_ribbon(aes(ymin=lower.HPD.85, ymax = upper.HPD.85), alpha = alpha) +
  geom_ribbon(aes(ymin=lower.HPD.90, ymax = upper.HPD.90), alpha = alpha) +
  geom_ribbon(aes(ymin=lower.HPD, ymax = upper.HPD.95, colour = clone), alpha = alpha, linetype  = 2) +
  geom_ribbon(aes(ymin=lower.HPD.85, ymax = upper.HPD.85), alpha = alpha) +
  geom_ribbon(aes(ymin=lower.HPD.99, ymax = upper.HPD.99), alpha = alpha) +
  #geom_ribbon(aes(ymin=lower.HPD.99, ymax = upper.HPD.99),alpha = 0.02, fill = 'red') +
  #geom_point()+
  geom_line(aes(colour = clone )) + 
  geom_hline(yintercept = 0) +
  facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
  theme_bw()



ggplot(cont_clone, aes(x = days_scaled, y = estimate, group=clone, fill = clone)) +
  geom_ribbon(aes(ymin=lower.HPD.85, ymax = upper.HPD.85), alpha = alpha) +
  geom_ribbon(aes(ymin=lower.HPD.90, ymax = upper.HPD.90), alpha = alpha) +
  geom_ribbon(aes(ymin=lower.HPD, ymax = upper.HPD.95, colour = clone), alpha = alpha, linetype  = 2) +
  geom_ribbon(aes(ymin=lower.HPD.85, ymax = upper.HPD.85), alpha = alpha) +
  geom_ribbon(aes(ymin=lower.HPD.99, ymax = upper.HPD.99), alpha = alpha) +
  #geom_ribbon(aes(ymin=lower.HPD.99, ymax = upper.HPD.99),alpha = 0.02, fill = 'red') +
  #geom_point()+
  geom_line(aes(colour = clone )) + 
  geom_hline(yintercept = 0) +
  facet_grid(rows = vars(VI), cols = vars(clone), scales = "free_y") +
  theme_bw()
