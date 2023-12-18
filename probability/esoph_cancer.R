library(tidyverse)
library(dplyr)


num_control_groups <- nrow(esoph) # number of control groups
all_cases <- sum(esoph$ncases) # number of cases
all_controls <- sum(esoph$ncontrols) # number of controls


# probability of cancer case in highest alcohol consumption group
high_alc_group <- esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)


# probability of cancer case in lowest alcohol consumption group
lowest_alc_group <- esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)


# probability of over 10g smoked p/day on a cancer case
tob_cases <- esoph %>%
  filter(tobgp > "0-9g/day") %>%
  pull(ncases) %>%
  sum()
tob_cases/all_cases


# probability of over 10g smoked p/day on a control
tob_controls <- esoph %>%
  filter(tobgp > "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()
tob_controls/all_controls


# probability that a case is in the highest alcohol group
alc_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncases) %>%
  sum()
alc_cases/all_cases


# probability that a case is in the highest tobacco group
tob_cases <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncases) %>%
  sum()
tob_cases/all_cases


# probability that a case is in the highest tobacco group AND highest alcohol group
tob_and_alc_cases <- esoph %>%
  filter(tobgp == "30+" & alcgp == "120+") %>%
  pull(ncases) %>%
  sum()
tob_and_alc_cases/all_cases


# probability that a case is in the highest tobacco group OR highest alcohol group
tob_or_alc_cases <- esoph %>%
  filter(tobgp == "30+" | alcgp == "120+") %>%
  pull(ncases) %>%
  sum()
tob_or_alc_cases/all_cases


# probability that a control is in the highest alcohol group
alc_controls <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()
alc_controls/all_controls


# How many times more likely are cases than controls in the highest alcohol group
(sum(high_alc_group$ncases)/all_cases)/(sum(high_alc_group$ncontrols)/all_controls)


# probability that a control is in the highest tobacco group
tob_controls <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()
tob_controls/all_controls


# probability that a control is in the highest tobacco group AND highest alcohol group
tob_and_alc_controls <- esoph %>%
  filter(tobgp == "30+" & alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()
tob_and_alc_controls/all_controls


# probability that a control is in the highest tobacco group OR highest alcohol group
tob_or_alc_controls <- esoph %>%
  filter(tobgp == "30+" | alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()
tob_or_alc_controls/all_controls


# How many times more likely are cases than controls in the highest alcohol group or tobacco
(tob_or_alc_cases/all_cases)/(tob_or_alc_controls/all_controls)
