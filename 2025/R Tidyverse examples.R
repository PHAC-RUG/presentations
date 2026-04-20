
pacman::p_load(tidyverse)


# To view the dataset
View(cars)


# Filter (select rows on condition)
cars_2 <- cars %>%
  filter(speed > 9 & dist <20)


# Select columns and rename (new = old)
cars_3 <- cars %>%
  select(speed, distance = dist)


# create new column with categories for speed
cars_4 <- cars_3 %>%
  mutate(speed_cat = case_when(speed <= 10 ~ "1-10 km/hr",
                               speed <= 20 ~ "11-20 km/hr",
                               speed  > 20 ~ "21+ km/hr"
                               ))%>%
  group_by(speed_cat) %>%
  summarize(distance_mean = mean(distance),
            distance_total = sum(distance))


cars_4.2 <- cars_3 %>%
  mutate(speed_cat = ifelse(speed < 15, "<15 km/hr", "15+ km/hr"))

    
### Pivot wider and longer

cars_long <- cars_4 %>%
  pivot_longer(cols = c(distance_mean, distance_total), 
               names_to = "calc", 
               values_to = "values")


cars_wide <- cars_long %>%
  pivot_wider(names_from = speed_cat, values_from = values)


### str_detect()

cars_5 <- cars_long %>%
  mutate(mean = str_detect(calc, "mean"))


### str_extract()

cars_6 <- cars_long %>%
  mutate(type = str_extract(calc, "mean|total"))


### str_pad()

cars_7 <- cars_3 %>%
  mutate(row = row_number()) %>%
  mutate(row2 = str_pad(row, 3, "left", pad = "0"))


### str_replace() & str_to_upper()

cars_8 <- cars_6 %>%
  mutate(speed_cat2 = str_replace(speed_cat, " km/hr", "")) %>%
  mutate(type2 = str_to_upper(type))


### str_glue()

cars_9 <- cars_7 %>%
  mutate(id = str_glue("{row2}_{speed}_speed"))


