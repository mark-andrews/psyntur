## Code to prepare included datasets

# This code assumes packages `Stat2Data` and `HSAUR` are installed, but these are
# not listed as dependencies of this package.

library(tidyverse)

# Visual-verbal data ------------------------------------------------------

# Applying essentially superficial changes to the `VisualVerbal` data set from
# the `Stats2Data` data set. Drop one redundant variable, changes names and
# values to lower case, changes names.

data('VisualVerbal', package = 'Stat2Data')

vizverb <- as_tibble(VisualVerbal) %>% 
  select(-Group) %>% 
  rename_all(str_to_lower) %>% 
  rename(response = report) %>% 
  mutate_at(vars(-time), as.character) %>% 
  mutate_at(vars(task, response),
            str_to_lower)

usethis::use_data(vizverb, overwrite = TRUE)

# Faithful face data ------------------------------------------------------

# Mostly superficial changes applied to the `FaithfulFaces` data from the `Stat2Data` package.
# Names changed to lower.
# The `cheater` variable is recoded as logical.
# Values of sex in two variables changed from `F` and `M` to `female` and `male`.
# Factors changed to characters.
# And original CamelCase changes to snake_case.

data("FaithfulFaces", package = 'Stat2Data')

faithfulfaces <- as_tibble(FaithfulFaces) %>% 
  rename_all(str_to_lower) %>% 
  mutate(cheater = ifelse(cheater == 1, TRUE, FALSE)) %>% 
  mutate_at(vars(facesex, ratersex),
            ~recode(., F = 'female', M = 'male')
  ) %>% mutate_if(is.factor, as.character) %>% 
  rename(face_sex = facesex,
         rater_sex = ratersex,
         sex_dimorph = sexdimorph,
         trustworthy = trust,
         attractive = attract)

usethis::use_data(faithfulfaces, overwrite = TRUE)


# Schizophrenia data ------------------------------------------------------

# Very superficial change to the `schizophrenia` data frame
# from `HSAUR`. Primarily, covert to tibble as change 
# factor to character.
data("schizophrenia", package = 'HSAUR')

schizophrenia <- as_tibble(schizophrenia) %>% 
  mutate(gender = as.character(gender))

usethis::use_data(schizophrenia, overwrite = TRUE)


# Sleep repeated df -------------------------------------------------------

pairedsleep <- sleep %>%
  pivot_wider(id_cols = ID, names_from = group, values_from  = extra) %>%
  rename_with(~str_c('y', ., sep=''), c(`1`, `2`))

usethis::use_data(pairedsleep, overwrite = TRUE)


# Weight data -------------------------------------------------------------

# The original original is here:  https://www.openlab.psu.edu/ansur2/

ansur <- read_csv("http://data.ntupsychology.net/weight.csv") %>%
  mutate(height_tercile = ntile(height, 3), 
         age_tercile = ntile(age, 3))

usethis::use_data(ansur, overwrite = TRUE)
