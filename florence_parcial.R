library(HistData)
library(tidyverse)

data(Nightingale)
str(Nightingale)

df <- Nightingale

df <- df |>
  pivot_longer(cols = c("Disease", "Wounds", "Other"),
               names_to = "Causa",
               values_to = "Mortes")


df <- df |>
  mutate(Taxa = case_when(
    Causa == "Disease" ~ Disease.rate,
    Causa == "Wounds" ~ Wounds.rate,
    Causa == "Other" ~ Other.rate
  ))


meses <- c(month.abb[7:12], month.abb[1:6])

titulo_1 <- "1.\nAPRIL 1854 TO MARCH 1855"
titulo_2 <- "2.\nAPRIL 1855 TO MARCH 1856"

df <- df |>
  mutate(Month = factor(Month, levels = meses),
         Year_period = ifelse(Date < as.Date("1855-04-01"),titulo_1, titulo_2)) |>
  mutate(Year_period = factor(Year_period, levels = c(titulo_2, titulo_1)))

df |>
  filter(Year == 1854) |>
  ggplot(aes(x = Month, y = Taxa, fill = Causa)) +
  geom_col(position = "identity", alpha = 0.5)

df |>
  filter(Year == 1854) |>
  ggplot(aes(x = Month, y = Taxa, fill = Causa)) +
  geom_col(position = "dodge")

df |>
  filter(Year == 1854) |>
  ggplot(aes(x = Month, y = sqrt(Taxa), fill = Causa)) +
  geom_col(position = "identity", alpha = 0.5, width = 1, color = "white") +
  coord_polar(start = 0)

df |>
  ggplot(aes(x = Month, y = sqrt(Taxa), fill = Causa)) +
  geom_col(position = "identity", alpha = 0.7, width = 1, color = "white") +
  coord_polar(start = 0) +
  facet_wrap(~Year_period) + 
  scale_fill_manual(values = c(
    "Disease" = "#81b1ce",  # steel blue
    "Wounds"  = "#f2a69d",  # rosa/bege claro, se quiser manter
    "Other"   = "#5A2E0C"   # marrom bem escuro
  )) +
  theme_minimal() +
  labs(title = "DIAGRAM OF THE CAUSES OF MORTALITY\nIN THE ARMY IN THE EAST") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(family = "mono", size = 12, hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank())
