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
        legend.title = element_blank(),
        legend.key.size = unit(0.3, "cm"))

meses_longos <- c(month.name[7:12], month.name[1:6])

rotulos <- data.frame(
  Month = factor(meses, levels = meses, ordered = TRUE),
  Meses_longos = factor(meses_longos, levels = meses_longos),
  altura = 32,
  angulos = seq(from = 75, by = -30, length.out = 12)
)

df |>
  left_join(rotulos, join_by(Month)) |>
  ggplot() +
  geom_col(aes(x = Month, y = sqrt(Taxa), fill = Causa), position = "identity", alpha = 0.7, width = 1, color = "white") +
  geom_text(aes(x = Month, y = altura, label = Meses_longos, angle = angulos + 270)) +
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
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(family = "mono", size = 12, hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.size = unit(0.3, "cm"))
  
View(rotulos)


df |>
  left_join(rotulos, join_by(Month)) |>
  mutate(angulos = angulos + 270) |>
  mutate(angulos = ifelse(angulos >=  105 & angulos <= 255, angulos + 180, angulos)) |>
  ggplot() +
  geom_col(aes(x = Month, y = sqrt(Taxa), fill = Causa), position = "identity", alpha = 0.7, width = 1, color = "white") +
  geom_text(aes(x = Month, y = altura, label = Meses_longos, angle = angulos), size = 2) +
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
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Rye", size = 12, hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.size = unit(0.3, "cm"))

library(showtext)
showtext_auto()
font_add(family = "Rye", regular = "Rye-Regular.ttf")




