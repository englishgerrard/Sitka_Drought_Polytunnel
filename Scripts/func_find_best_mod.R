evaluate_model <- function(x = 1, y = 5){
  
  index$clone <- as.factor(index$clone)
  
  m1 <- lmer(VI ~ days + treatment + clone +
               (1|id), data = index)
  
  m2 <- lmer(VI ~ days + treatment + clone + days:treatment +
               (1|id), data = index)
  
  m3 <- lmer(VI ~ days + treatment + clone + days:clone +
               (1|id), data = index)
  
  m4 <- lmer(VI ~ days + treatment + clone + days:treatment:clone +
               (1|id), data = index)
  
  m5 <- lmer(VI ~ days + treatment + clone + days:treatment +
               days:treatment:clone + (1|id), data = index)
  
  m6 <- lmer(VI ~ days + treatment + clone + days:clone +
               days:treatment:clone + (1|id), data = index)
  
  m7 <- lmer(VI ~ days + treatment + clone + days:treatment + days:clone +
               days:treatment:clone + (1|id), data = index)
  
  
  ####### ploynomials
  
  p1 <- lmer(VI ~ poly(days, 3) + treatment + clone +
               (1|id), data = index)
  
  p2 <- lmer(VI ~ poly(days, 3) + treatment + clone + poly(days, 3):treatment +
               (1|id), data = index)
  
  p3 <- lmer(VI ~ poly(days, 3) + treatment + clone + poly(days, 3):clone +
               (1|id), data = index)
  
  p4 <- lmer(VI ~ poly(days, 3) + treatment + clone + poly(days, 3):treatment:clone +
               (1|id), data = index)
  
  p5 <- lmer(VI ~ poly(days, 3) + treatment + clone + poly(days, 3):treatment +
               poly(days, 3):treatment:clone + (1|id), data = index)
  
  p6 <- lmer(VI ~ poly(days, 3) + treatment + clone + poly(days, 3):clone +
               poly(days, 3):treatment:clone + (1|id), data = index)
  
  p7 <- lmer(VI ~ poly(days, 3) + treatment + clone + poly(days, 3):treatment + poly(days, 3):clone +
               poly(days, 3):treatment:clone + (1|id), data = index)
  
  
  modlist <- c(m1, m2, m3, m4, m5, m6, m7,
               p1, p2, p3, p4, p5, p6, p7)
  
  names(modlist) <-  c('m1', 'm2', 'm3', 'm4', 'm5', 'm6', 'm7',
                       'p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7')
  
  performace_stats <-  compare_performance(m1, m2, m3, m4, m5, m6, m7,
                                           p1, p2, p3, p4, p5, p6, p7)
  
  
  top_mods <-  performace_stats %>%  slice_max(n = y, AIC_wt)
  bestMod <- modlist[[top_mods[x,1]]]
  print(top_mods)
  return(bestMod)}



## branch models used in chapter 4 of thesis are below
### two functions one with all combinations and one whihc excludes branch:treatment term
### all effects and interactions need to be in correct order 


# allfindmod <- function(x = 1){
# 
#   indexx$clone <- as.factor(indexx$clone)
# 
#   m1 <- lmer(VI ~ days*treatment*branch +
#                (1|id), data = indexx)
# 
#   m2 <- lmer(VI ~ days + treatment + branch +
#                   days:treatment + days:branch + treatment:branch +  (1|id), data = indexx)
# 
#   m3 <- lmer(VI ~ days + treatment + branch +
#                 days:treatment + days:branch +  (1|id), data = indexx)
#   m4 <- lmer(VI ~ days + treatment + branch +
#                 days:treatment + treatment:branch +  (1|id), data = indexx)
#   m5 <- lmer(VI ~ days + treatment + branch +
#                 days:branch + treatment:branch +  (1|id), data = indexx)
# 
#   m6 <- lmer(VI ~ days + treatment + branch + 
#                 days:treatment+ (1|id), data = indexx)
#   m7 <- lmer(VI ~ days + treatment + branch + 
#                 days:branch +   (1|id), data = indexx)
#   m8 <- lmer(VI ~ days + treatment + branch + 
#                 branch:treatment +   (1|id), data = indexx)
#   
#   m9 <- lmer(VI ~ days + treatment + branch + 
#                 (1|id), data = indexx)
#   
#   m10 <- lmer(VI ~ days + treatment + branch + 
#                 days:treatment + days:branch + 
#                 days:treatment:branch + (1|id), data = indexx)
#   
# ####### ploynomials 
#   
#   p1 <- lmer(VI ~ poly(days, 3)*treatment*branch + 
#                (1|id), data = indexx)
#   
#   p2 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#                poly(days, 3):treatment + poly(days, 3):branch + treatment:branch +  (1|id), data = indexx)
#   
#   p3 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#                poly(days, 3):treatment + poly(days, 3):branch +  (1|id), data = indexx)
#   p4 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#                poly(days, 3):treatment + treatment:branch +  (1|id), data = indexx)
#   p5 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#                poly(days, 3):branch + treatment:branch +  (1|id), data = indexx)
#   
#   p6 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#                poly(days, 3):treatment+ (1|id), data = indexx)
#   p7 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#                poly(days, 3):branch +   (1|id), data = indexx)
#   p8 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#                branch:treatment +   (1|id), data = indexx)
#   
#   p9 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#                (1|id), data = indexx)
#   
#   p10 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#                 poly(days, 3):treatment + poly(days, 3):branch + 
#                 poly(days, 3):treatment:branch + (1|id), data = indexx)
#   
#   modlist <- c(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
#                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
#   
#   names(modlist) <-  c('m1', 'm2', 'm3', 'm4', 'm5', 'm6', 'm7', 'm8', 'm9', 'm10',
#                        'p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10')
#   
#   d <-  compare_performance(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
#                            p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
#  
# 
# b4 <-  d %>%  slice_max(n = 5, AIC_wt)
# bestMod <- modlist[[b4[x,1]]]
# print(b4) 
# return(bestMod)}
# 
# findmod <- function(x = 1){
#   
#   indexx$clone <- as.factor(indexx$clone)
#   
# #  m1 <- lmer(VI ~ days*treatment*branch + 
# #               (1|id), data = indexx)
#   
# #  m2 <- lmer(VI ~ days + treatment + branch + 
# #               days:treatment + days:branch + treatment:branch +  (1|id), data = indexx)
#   
#   m3 <- lmer(VI ~ days + treatment + branch + 
#                days:treatment + days:branch +  (1|id), data = indexx)
# #    m4 <- lmer(VI ~ days + treatment + branch + 
# #                  days:treatment + treatment:branch +  (1|id), data = indexx)
# #  m5 <- lmer(VI ~ days + treatment + branch + 
# #               days:branch + treatment:branch +  (1|id), data = indexx)
#   
#   m6 <- lmer(VI ~ days + treatment + branch + 
#                days:treatment+ (1|id), data = indexx)
#   m7 <- lmer(VI ~ days + treatment + branch + 
#                days:branch +   (1|id), data = indexx)
# #  m8 <- lmer(VI ~ days + treatment + branch + 
# #               branch:treatment +   (1|id), data = indexx)
#   
#   m9 <- lmer(VI ~ days + treatment + branch + 
#                (1|id), data = indexx)
#   
#   m10 <- lmer(VI ~ days + treatment + branch + 
#                 days:treatment + days:branch + 
#                 days:treatment:branch + (1|id), data = indexx)
#   
#   ####### ploynomials 
#   
# #  p1 <- lmer(VI ~ poly(days, 3)*treatment*branch + 
# #               (1|id), data = indexx)
#   
# #  p2 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
# #               poly(days, 3):treatment + poly(days, 3):branch + treatment:branch +  (1|id), data = indexx)
#   
#   p3 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#                poly(days, 3):treatment + poly(days, 3):branch +  (1|id), data = indexx)
#   #  p4 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#   #               poly(days, 3):treatment + treatment:branch +  (1|id), data = indexx)
# #  p5 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
# #               poly(days, 3):branch + treatment:branch +  (1|id), data = indexx)
#   
#   p6 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#                poly(days, 3):treatment+ (1|id), data = indexx)
#   p7 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#                poly(days, 3):branch +   (1|id), data = indexx)
# #  p8 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
# #               branch:treatment +   (1|id), data = indexx)
#   
#   p9 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#                (1|id), data = indexx)
#   
#   p10 <- lmer(VI ~ poly(days, 3) + treatment + branch + 
#                 poly(days, 3):treatment + poly(days, 3):branch + 
#                 poly(days, 3):treatment:branch + (1|id), data = indexx)
#   
#   modlist <- c( m3,  m6, m7, m9, m10,
#                p3,  p6, p7, p9, p10)
#   
#   names(modlist) <-  c( 'm3',  'm6', 'm7',  'm9', 'm10',
#                         'p3',  'p6', 'p7',  'p9', 'p10')
#   
#   d <-  compare_performance( m3,  m6, m7, m9, m10,
#                              p3, p6, p7, p9, p10)
#   
#   
#   b4<-  d %>%  slice_max(n = 5, AIC_wt)
#   bestMod <- modlist[[b4[x,1]]]
#   print(b4) 
#   return(bestMod)}
# 
# findmodCLONE <- function(x = 1){
#   
#   indexx$clone <- as.factor(indexx$clone)
#   
#   
# # 1 way   
#  c1 <- lmer(VI ~ days + treatment + branch + clone +
#               (1|id), data = indexx)
# # 2 way 
#  c2 <- lmer(VI ~ days + treatment + branch + clone + 
#               days:treatment +
#               (1|id), data = indexx)
#  
#  c3 <- lmer(VI ~ days + treatment + branch + clone + 
#               days:branch +
#               (1|id), data = indexx)
#  
#  c4 <- lmer(VI ~ days + treatment + branch + clone + 
#               days:clone +
#               (1|id), data = indexx)
# # double 2 way
#  c5 <- lmer(VI ~ days + treatment + branch + clone + 
#               days:treatment + days:branch +
#               (1|id), data = indexx)
#  
#  c6 <- lmer(VI ~ days + treatment + branch + clone + 
#               days:treatment + days:clone +
#               (1|id), data = indexx)
#  
#  c7 <- lmer(VI ~ days + treatment + branch + clone + 
#               days:branch + days:clone +
#               (1|id), data = indexx)
# # triple 2 way
#  c8 <- lmer(VI ~ days + treatment + branch + clone + 
#               days:treatment + days:branch + days:clone +
#               (1|id), data = indexx)
# # double 2 way and 3 way 
#  c9 <- lmer(VI ~ days + treatment + branch + clone + 
#               days:treatment + days:branch + days:treatment:branch +
#               (1|id), data = indexx)
#  
#  c10 <- lmer(VI ~ days + treatment + branch + clone + 
#               days:treatment + days:clone + days:treatment:clone +
#               (1|id), data = indexx)
#  # triple 2 way and 3 way
#  c11 <- lmer(VI ~ days + treatment + branch + clone + 
#               days:treatment + days:branch + days:clone + days:treatment:branch +
#               (1|id), data = indexx)
#  
#  c12 <- lmer(VI ~ days + treatment + branch + clone + 
#               days:treatment + days:branch + days:clone + days:treatment:clone +
#               (1|id), data = indexx)
#  # triple 2 way and double 3 way 
#  c13 <- lmer(VI ~ days + treatment + branch + clone + 
#               days:treatment + days:branch + days:clone + days:treatment:branch + + days:treatment:clone +
#               (1|id), data = indexx)
#  
#   ####### ploynomials 
#   
#  # 1 way   
#  p1 <- lmer(VI ~ poly(days, 3) + treatment + branch + clone +
#               (1|id), data = indexx)
#  # 2 way 
#  p2 <- lmer(VI ~ poly(days, 3) + treatment + branch + clone + 
#               poly(days, 3):treatment +
#               (1|id), data = indexx)
#  
#  p3 <- lmer(VI ~ poly(days, 3) + treatment + branch + clone + 
#               poly(days, 3):branch +
#               (1|id), data = indexx)
#  
#  p4 <- lmer(VI ~ poly(days, 3) + treatment + branch + clone + 
#               poly(days, 3):clone +
#               (1|id), data = indexx)
#  # double 2 way
#  p5 <- lmer(VI ~ poly(days, 3) + treatment + branch + clone + 
#               poly(days, 3):treatment + poly(days, 3):branch +
#               (1|id), data = indexx)
#  
#  p6 <- lmer(VI ~ poly(days, 3) + treatment + branch + clone + 
#               poly(days, 3):treatment + poly(days, 3):clone +
#               (1|id), data = indexx)
#  
#  p7 <- lmer(VI ~ poly(days, 3) + treatment + branch + clone + 
#               poly(days, 3):branch + poly(days, 3):clone +
#               (1|id), data = indexx)
#  # triple 2 way
#  p8 <- lmer(VI ~ poly(days, 3) + treatment + branch + clone + 
#               poly(days, 3):treatment + poly(days, 3):branch + poly(days, 3):clone +
#               (1|id), data = indexx)
#  # double 2 way and 3 way 
#  p9 <- lmer(VI ~ poly(days, 3) + treatment + branch + clone + 
#               poly(days, 3):treatment + poly(days, 3):branch + poly(days, 3):treatment:branch +
#               (1|id), data = indexx)
#  
#  p10 <- lmer(VI ~ poly(days, 3) + treatment + branch + clone + 
#                poly(days, 3):treatment + poly(days, 3):clone + poly(days, 3):treatment:clone +
#                (1|id), data = indexx)
#  # triple 2 way and 3 way
#  p11 <- lmer(VI ~ poly(days, 3) + treatment + branch + clone + 
#                poly(days, 3):treatment + poly(days, 3):branch + poly(days, 3):clone + poly(days, 3):treatment:branch +
#                (1|id), data = indexx)
#  
#  p12 <- lmer(VI ~ poly(days, 3) + treatment + branch + clone + 
#                poly(days, 3):treatment + poly(days, 3):branch + poly(days, 3):clone + poly(days, 3):treatment:clone +
#                (1|id), data = indexx)
#  # triple 2 way and double 3 way 
#  p13 <- lmer(VI ~ poly(days, 3) + treatment + branch + clone + 
#                poly(days, 3):treatment + poly(days, 3):branch + poly(days, 3):clone + poly(days, 3):treatment:branch +  poly(days, 3):treatment:clone +
#                (1|id), data = indexx)
#   
#   modlist <- c( c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13,
#                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)
#   
#   names(modlist) <-  c( 'c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'c10', 'c11', 'c12', 'c13',
#                         'p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13')
#   
#   d <-  compare_performance( c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13,
#                              p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)
#   
#   
#   b4<-  d %>%  slice_max(n = 5, AIC_wt)
#   bestMod <- modlist[[b4[x,1]]]
#   print(b4) 
#   return(bestMod)}

