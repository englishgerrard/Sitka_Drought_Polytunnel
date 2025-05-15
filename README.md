# Sitka_Drought

reproducing work in thesis for publication. Updated/ new scripts in this respority along with analysis of recovery data


# script order
pre processing

`fun_load__priors` <- great a list of priors for models
`stat_makes_bayes_VI_model` <- runs models  
`get_generate_posteriors` <- save pos distrubutions (creates clone 6 coef)


## maybe i lose this observation in analysis
indexx <- index %>% filter(days != 49 | treatment != 'D' | clone != 1564 | clone_num != 'd')



# notes
bayes models run using fairy relaxed priors and polynomial,2  


checks 
at time 0 is there a differences between treatments
at time 0 is there a difference between clones 
is there a drought effect over time

for each VI
1. i want to know is there a drought effect over time
2. is there a drought effect over time that differs between clones
3. is the fit linear - poly1 the effect over time is constant?
                     - poly2 the effect over time occurs faster at different days
                     - just poly1 the effect is constant
                     - just poly2 the effect is non lin - increases/ decreases over time 
                     - both - 


## plots
- model checks - adjuts priors if needed
- t0 plots - treatment and clone at 0 
- VI effect over time

# results plots 
- drought effect over time (poly and lin)
- drought effect over time per clone

     


additional - at time 0 do the clones differ


# notes 
the clone 6 error is generally larger becuse 
Zero-Sum Constraints: In a zero-sum contrast, the effects of the different levels (in this case, clones) are constrained such that their sum equals zero. This means that if one clone's effect is estimated to be larger, the others must compensate by being smaller. If the estimates for the other clones are relatively stable, the estimate for clone 6 may have more variability as it is derived from the sum of the other clones' effects. This can lead to wider credible intervals.
Model Uncertainty: The estimation of the effect for clone 6 is dependent on the estimates of the other clones. If there is uncertainty in the estimates of clones 1 to 5, this uncertainty propagates to the estimate for clone 6, potentially leading to a larger CI.