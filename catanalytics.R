library(tidyverse)


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
  left_join(defense, by = c("Group" = "Group_def")) %>%
  filter(id != id_def) %>%
  group_by(Group, Name, id, game, vp) %>%
  summarise_all(max) %>%
  ungroup() %>%
  select(-Group, -Name, -id, -id_def, -game) %>%
  drop_na(vp)

X <-
  data %>%
  select(-vp) %>%
  data.matrix()

y <- 
  data %>%
  pull(vp)


model <- glmnet::glmnet(X, y, alpha = 0)

plot(model)

model <- glmnet::glmnet(X, y, lambda = 0.1, alpha = 0)

broom::tidy(model) %>%
  filter(str_detect(term, "^id")) %>%
  separate(term, c("idx", "id", "dir"), sep = "_") %>%
  mutate(id = as.numeric(id)) %>%
  left_join(id_map, by = "id") %>%
  select(Name, estimate, dir) %>%
  mutate(Name = replace_na(Name, "Intercept"),
         dir  = replace_na(dir, "off")) %>%
  pivot_wider(names_from = dir, values_from = estimate) %>%
  arrange(-off)
  DT::datatable()


round2 <- read_csv("round2.csv")

round2 <- 
  round2 %>%
  left_join(id_map, by = "Name")



defense <- 
  round2 %>%
  select(Group, id) %>%
  fastDummies::dummy_cols(select_columns = "id") %>%
  rename_all(~ paste0(., "_def"))


temp <- 
  round2 %>%
  group_by(Group) %>%
  fastDummies::dummy_cols(select_columns = "id") %>%
  left_join(defense, by = c("Group" = "Group_def")) %>%
  filter(id != id_def) %>%
  group_by(Group, Name, id) %>%
  summarise_all(max)

data_2 <- 
  temp %>%
  ungroup() %>%
  select(-Group, -Name, -id, -id_def)




model_terms <- colnames(X)

missing_columns <- 
  model_terms[(!(model_terms %in% colnames(data_2)))]

new_mat <- matrix(data = 0, nrow = nrow(data_2), ncol = length(missing_columns))
colnames(new_mat) <- missing_columns

new_df <- 
  as_tibble(new_mat)

new_test_df <-
  bind_cols(data_2, new_df) %>%
  select(all_of(model_terms))

X_2 <-
  new_test_df %>%
  data.matrix()



temp %>%
  ungroup() %>%
  select(Group, Name) %>%
  mutate(line = predict(model, newx = X_2, type = "response")  %>% as.vector()) %>%
  arrange(Group, -line) %>%
  mutate(line = ceiling(line) - 0.5)
  

temp %>%
  ungroup() %>%
  select(Group, Name) %>%
  mutate(line = predict(model, newx = X_2, type = "response")  %>% as.vector()) %>%
  arrange(Group, - line)

  

temp %>%
  group_by(Group) %>%
  select(Group, Name) %>%
  sample_n(4)


###############################
  
