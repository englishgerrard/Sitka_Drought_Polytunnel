source('./Scripts/.FUNCTIONS.R')
full_index <- read.csv('./Data/full_VI.csv')

ggplot(filter(full_index, branch == 'top' & days != 59), aes(x = as.factor(days), y = NDVI, colour = treatment)) +
  geom_boxplot() +
  stat_compare_means(aes(group = treatment), method = "t.test", label = "p.signif") +# +
  facet_wrap(~clone)

ggplot(filter(full_index, branch %in% c('top','new') & days != 59), aes(x = as.factor(days), y = ARI, colour = treatment)) +
  geom_boxplot()  +
  stat_compare_means(aes(group = treatment), method = "t.test", label = "p.signif") +
  facet_wrap(~branch)


ggplot(filter(full_index, days %in% c(0,367) & branch %in% c('new','top','old')), aes(x = branch, y = PRI, colour = as.factor(days))) +
  geom_boxplot() +
  facet_wrap(~as.factor(days))

ggplot(filter(full_index, days %in% c(367)), aes(x = treatment, y = ARI, colour = as.factor(branch) )) +
  geom_boxplot()+
  stat_compare_means(aes(group = branch), method = "t.test", label = "p.signif") +# +
  facet_wrap(~as.factor(days))




ggplot(filter(full_index, branch == 'top' & days != 59), aes(x = as.factor(days), y = PRI, colour = treatment)) +
  geom_boxplot() +
  stat_compare_means(aes(group = treatment), method = "t.test", label = "p.signif") +
  theme_minimal()

