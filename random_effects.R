library(tidyverse)
library(glmmLasso)

results <- read_csv("ranks.csv")

results <-
  results %>%
  mutate(id = row_number())%>%
  group_by(Name) %>%
  mutate(id = min(id)) %>%
  ungroup()

id_map <-
  results %>%
  group_by(Name) %>%
  select(Name, id) %>%
  ungroup() %>%
  distinct()

defense <- 
  results %>%
  select(Group, id) %>%
  fastDummies::dummy_cols(select_columns = "id") %>%
  rename_all(~ paste0(., "_def"))

data <- 
  results %>%
  group_by(Group) %>%
  pivot_longer(cols = contains("Game"), names_to = "game", values_to = "vp") %>%
  fastDummies::dummy_cols(select_columns = "id") %>%
  unite(gameid, c(game, Group), sep = "_", remove = F) %>%
  left_join(defense, by = c("Group" = "Group_def")) %>%
  filter(id != id_def) %>%
  group_by(Group, Name, id, game, vp) %>%
  summarise_all(max) %>%
  ungroup() %>%
  select(-game, -Name, -id, -id_def, -Group) %>%
  drop_na(vp)

id_formulas <- 
  data %>%
  select(matches("^id")) %>%
  colnames() %>%
  paste(collapse = " + ")


fixed_formula <-
  paste0("vp ~ ", id_formulas) %>%
  formula()

data <- 
  data %>%
  mutate(gameid = factor(gameid))

model <-
  glmmLasso(fix = fixed_formula, 
            #rnd = NULL,
            rnd = list(gameid=~1),
            data = data,
            lambda = 0.1)
