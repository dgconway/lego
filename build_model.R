library(tidyverse)
library(nnet)
library(MASS)
library(pheatmap)


inventories = read_csv("inventories.csv")
inventory_parts = read_csv("inventory_parts.csv")
sets = read_csv("sets.csv")
inventory_sets = read_csv("inventory_sets.csv")
themes = read_csv("themes.csv")
parts = read_csv("parts.csv")
colors = read_csv("colors.csv")
part_categories = read_csv("part_categories.csv")


join_set_themes = inner_join(sets, themes, by=c("theme_id"="id"))


count_num_per_theme = join_set_themes |> count(name.y) |> slice_max(order_by=n, n=10)

ten_most_popular = join_set_themes |> inner_join(count_num_per_theme, by=c("name.y"="name.y"))


join_for_top_10 = ten_most_popular |> 
  left_join(inventories, by=c("set_num"="set_num")) |> 
  left_join(inventory_parts, by=c("id"="inventory_id")) |> 
  left_join(colors, by=c("color_id"="id"))

intermediate = join_for_top_10 |> 
  group_by(set_num, rgb, name.y) |> 
  summarize("n"=n()) |> 
  ungroup() |> 
  group_by(set_num, name.y) |> 
  mutate("total_num_pieces"=n(),
         "pct_pieces"=n/total_num_pieces) |> 
  dplyr::select(c("pct_pieces", "rgb", "set_num", "name.y")) |> 
  ungroup()

counting_colors = intermediate |> 
  pivot_wider(names_from=rgb, values_from=pct_pieces)

counting_colors[is.na(counting_colors)] <- 0


num_pieces = sets |> dplyr::select(c("num_parts", "set_num"))

add_n_pieces = counting_colors |> 
  left_join(num_pieces, by=c("set_num"="set_num")) |> 
  dplyr::select(-c("set_num"))

# adapted from ChatGPT
set.seed(42)  # For reproducibility
random_order <- sample(nrow(add_n_pieces))  # Create a random permutation of row indices
random_reordered_df <- add_n_pieces[random_order, ]

reorder = sample(add_n_pieces)

upper_lim = as.integer(nrow(add_n_pieces) * 0.8)
plus_one = upper_lim+1

train = random_reordered_df[0:upper_lim, ]
test = random_reordered_df[upper_lim:nrow(add_n_pieces), ]

g = capture.output(base_model <- multinom(name.y ~ 1, data = train))

g = capture.output(full_model <- multinom(name.y ~ ., data = train))

# adapted from ChatGPT
g = capture.output(forward_selected <- stepAIC(base_model, list(lower=base_model, upper=full_model), direction = "forward", trace = FALSE))

rm(g)

saveRDS(base_model, file="base_model.rds")

saveRDS(full_model, file="full_model.rds")

saveRDS(forward_selected, file = "multinom_classification.rds")
