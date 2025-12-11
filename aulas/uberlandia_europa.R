library(tidyverse)
library(sf)

# como construir um objeto sf
pontos <- matrix(c(0,0,
                   1,0,
                   1,1,
                   0,0),
                 ncol = 2,
                 byrow = TRUE)

pontos <- list(pontos)

poligono <- st_polygon(pontos)

poligono_coluna <- st_sfc(poligono)

dados <- data.frame(id = 1,
                    nome = "A")

objeto_sf <- st_sf(dados, poligono_coluna)

objeto_sf |>
  ggplot() +
  geom_sf(fill = "darkorange")


# agora, vamos voltar para o mapa de uberlândia

uberlandia <- st_read("uberlandia.shp")
uberlandia |>
  filter(Layer == "BAIRROS_INTEGRADOS") |>
  ggplot() +
  geom_sf()

uberlandia_poligono <- uberlandia |>
  filter(Layer == "BAIRROS_INTEGRADOS") |>
  st_cast("POLYGON")

nrow(uberlandia_poligono)

uberlandia_poligono |>
  mutate(id = row_number()) |>
  mutate(selecao = ifelse(id == 23, "sim", "nao")) |>
  ggplot() +
  geom_sf(aes(fill = selecao))

# a partir de agora, vamos retomar o problema da clusterização

df <- read.table("protein.txt", sep = "\t", header = TRUE)
str(df)

library(cshapes)
mapa <- cshp(date = as.Date("1970-01-01"))

names(mapa)

paises <- df$Country

mapa$country_name

paises %in% mapa$country_name
which(mapa$country_name %in% paises)

paises[!paises %in% mapa$country_name] <- c("German Democratic Republic",
                                            "Italy/Sardinia",
                                            "Rumania",
                                            "United Kingdom",
                                            "Russia (Soviet Union)",
                                            "German Federal Republic")

mapa |>
  filter(country_name %in% paises) |>
  ggplot() +
  geom_sf()
