# EDI CUT CODE
cat_add <- cat_add %>%
  # remove places where the entire row or column consists of 'NA'
  # remove_empty(which = c('rows', 'cols')) %>%
  # make column names consistent with cat_edi data
  # make data taken column consistent
  # if the first letter is 'y', then 'yes'
  # if first letter is 'n', then 'no'
  mutate(Data_taken = case_when(
    tolower(substr(Data_taken,1,1)) == 'y' ~ 'yes',
    tolower(substr(Data_taken,1,1)) == 'n' ~ 'no')) %>%
  # reduce duplicate species columns
  mutate(Organism = case_when(
    # un-abbreviate semibalanus and botryllus
    substr(Organism, 1,9) == 'Botryllus' ~ 'Botryllus schlosseri',
    Organism == 'Sb. balanoides' ~ 'Semibalanus balanoides',
    # fix inconsistent capitalization
    tolower(Organism) == 'fucus base' ~ 'Fucus base',
    tolower(Organism) == 'bare rock' ~ 'bare rock',
    # if not included in above, keep original entry
    T ~ Organism
  ))