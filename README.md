# Sitka_Drought

reproducing work in thesis for publication. Updated/ new scripts in this respority along with analysis of recovery data


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
- drought effect over time

# results plots 
- drought effect over time (poly and lin)
- drought effect over time per clone

     


additional - at time 0 do the clones differ