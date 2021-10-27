
## **************************************************************************
## Piso I - Opinión Pública
## Código local para documento Markdown
## 10/2021 UMAD
## **************************************************************************

library(tidyverse)
library(opuy)

## Evaluación del presidente  ===============================================


## Serie geom_smooth (aprobación y saldo) ==================================

# Descargar data desde opuy y crear tabla
dat_opuy <- opuy %>%
  filter(medicion == 'Evaluacion de gestion presidente') %>%
  select(fecha, anio_gobierno, empresa, valor, presidente, categoria_unificada)  %>%
  mutate(categoria_unificada = case_when(
    categoria_unificada == 3 ~ "Aprueba",  
    categoria_unificada == 2 ~ "Ni aprueba ni desaprueba",
    categoria_unificada == 1 ~ "Desaprueba",
    categoria_unificada == 0 ~ "NSNC")) %>%
  pivot_wider(names_from = categoria_unificada, values_from = valor) %>% 
  mutate(Saldo = Aprueba - Desaprueba) %>% 
  mutate(presidente = factor(presidente, 
                             levels = c("Lacalle", "Sanguinetti 2", "Batlle",
                                        "Vazquez 1", "Mujica", "Vazquez 2",
                                        "Lacalle Pou")))

# Anotaciones para gráfico
annotation <- data.frame(
  x = as.Date(c( "1992-01-01","1997-06-06", "2002-06-06", "2007-06-06", 
                 "2012-06-06", "2017-06-06", "2022-01-01")),
  y = 85,
  label = c("Lacalle","Sanguinetti II", "Batlle", "Vázquez I", "Mujica",
            "Vázquez II", "Lacalle Pou"))

fechas <- as.Date(c("1995-01-01","2000-01-01", "2005-01-01",
                    "2010-01-01", "2015-01-01", "2020-01-01"))


# Grafico % aprobación
aprob_serie <- dat_opuy %>%
  ggplot(aes(x = fecha, y = Aprueba, color = presidente)) +
  geom_smooth(aes(group = presidente), method ="loess", se = FALSE) +
  geom_point(size = 1.5, alpha = 0.3) +
  geom_vline(xintercept = as.numeric(fechas),
             linetype = "dashed", size = 0.3, color = "grey30") +
  geom_text(data = annotation, aes(x = x, y = y, label = label),
            color = "black", size = 3, fontface = "bold") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  labs(y = "% de aprobación",
       x = "",
       title = "Serie histórica de aprobación del presidente",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy \n 
       Datos originales de Equipos, Cifra, Factum, Opción, Interconsult y Radar') +
  scale_color_manual(name = "",
                     values = c("#5DADE2", "#BA0200", "#BA0200", "#013197",
                                "#013197", "#013197", "#5DADE2")) +
  scale_x_date(date_breaks = "2 years", date_minor_breaks = "1 year",
               date_labels = "%Y", limits = c(as.Date("1990-01-01"), NA))   

plot(aprob_serie)

# Grafico saldo neto
aprob_serie_s <- dat_opuy %>%
  ggplot(aes(x = fecha, y = Saldo, color = presidente)) +
  geom_smooth(aes(group = presidente), method ="loess", se = FALSE) +
  geom_point(size = 1.5, alpha = 0.3) +
  geom_vline(xintercept = as.numeric(fechas),
             linetype = "dashed", size = 0.3, color = "grey30") +
  geom_hline(yintercept = 0, size = 0.3) +
  geom_text(data = annotation, aes(x = x, y = y, label = label),
            color = "black", size = 3) +
  annotate("segment", x = as.Date("1990-01-01"), y = 10, xend = as.Date("1990-01-01"), yend = 25,
           arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
  annotate("text",
           label = "Evaluaciones \n positivas",
           x = as.Date("1992-06-01"), 
           y = 15,
           size = 3) +
  annotate("segment", x = as.Date("1990-01-01"), y = -10, xend = as.Date("1990-01-01"), yend = -25,
           arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
  annotate("text",
           label = "Evaluaciones \n negativas",
           x = as.Date("1992-06-01"), 
           y = -15,
           size = 3) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  labs(y = "Saldo neto",
       x = "",
       title = "Serie histórica de evaluación del presidente",
       subtitle = "Saldo neto = (% aprobación - % desaprobación)",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy \n 
       Datos originales de Equipos, Cifra, Factum, Opción, Interconsult y Radar') +
  scale_color_manual(name = "",
                     values = c("#5DADE2", "#BA0200", "#BA0200", "#013197",
                                "#013197", "#013197", "#5DADE2")) +
  scale_x_date(date_breaks = "2 years", date_minor_breaks = "1 year",
               date_labels = "%Y", limits = c(as.Date("1990-01-01"), NA))   

plot(aprob_serie_s)


## Por empresa (Equipos solo)   =============================================

rm(list = ls())

# Descargar data desde opuy y crear tabla
dat_equipos <- opuy %>%
  filter(medicion == 'Evaluacion de gestion presidente',
         empresa == "Equipos") %>%
  select(fecha, anio_gobierno, empresa, valor, presidente, categoria_unificada)  %>%
  mutate(categoria_unificada = case_when(
    categoria_unificada == 3 ~ "Aprueba",  
    categoria_unificada == 2 ~ "Ni aprueba ni desaprueba",
    categoria_unificada == 1 ~ "Desaprueba",
    categoria_unificada == 0 ~ "NSNC")) %>%
  pivot_wider(names_from = categoria_unificada, values_from = valor) %>% 
  mutate(Saldo = Aprueba - Desaprueba) %>% 
  mutate(presidente = factor(presidente, 
                             levels = c("Lacalle", "Sanguinetti 2", "Batlle",
                                        "Vazquez 1", "Mujica", "Vazquez 2",
                                        "Lacalle Pou")))

# Anotaciones para gráfico
annotation <- data.frame(
  x = as.Date(c( "1992-01-01","1997-06-06", "2002-06-06", "2007-06-06", 
                 "2012-06-06", "2017-06-06", "2022-01-01")),
  y = 85,
  label = c("Lacalle","Sanguinetti II", "Batlle", "Vázquez I", "Mujica",
            "Vázquez II", "Lacalle Pou"))

fechas <- as.Date(c("1995-01-01","2000-01-01", "2005-01-01",
                    "2010-01-01", "2015-01-01", "2020-01-01"))


# Grafico % aprobación
aprob_serie_e <- dat_equipos %>%
  ggplot(aes(x = fecha, y = Aprueba, color = presidente)) +
  geom_smooth(aes(group = presidente), method ="loess", se = FALSE) +
  geom_point(size = 1.5, alpha = 0.3) +
  geom_vline(xintercept = as.numeric(fechas),
             linetype = "dashed", size = 0.3, color = "grey30") +
  geom_text(data = annotation, aes(x = x, y = y, label = label),
            color = "black", size = 3, fontface = "bold") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  labs(y = "% de aprobación",
       x = "",
       title = "Serie histórica de aprobación del presidente",
       subtitle = "Datos de Equipos Consultores",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy') +
  scale_color_manual(name = "",
                     values = c("#5DADE2", "#BA0200", "#BA0200", "#013197",
                                "#013197", "#013197", "#5DADE2")) +
  scale_x_date(date_breaks = "2 years", date_minor_breaks = "1 year",
               date_labels = "%Y", limits = c(as.Date("1990-01-01"), NA))   

plot(aprob_serie_e)

# Grafico saldo neto
aprob_serie_s_e <- dat_equipos %>%
  ggplot(aes(x = fecha, y = Saldo, color = presidente)) +
  geom_smooth(aes(group = presidente), method ="loess", se = FALSE) +
  geom_point(size = 1.5, alpha = 0.3) +
  geom_vline(xintercept = as.numeric(fechas),
             linetype = "dashed", size = 0.3, color = "grey30") +
  geom_hline(yintercept = 0, size = 0.3) +
  geom_text(data = annotation, aes(x = x, y = y, label = label),
            color = "black", size = 3) +
  annotate("segment", x = as.Date("1990-01-01"), y = 10, xend = as.Date("1990-01-01"), yend = 25,
           arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
  annotate("text",
           label = "Evaluaciones \n positivas",
           x = as.Date("1992-06-01"), 
           y = 15,
           size = 3) +
  annotate("segment", x = as.Date("1990-01-01"), y = -10, xend = as.Date("1990-01-01"), yend = -25,
           arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
  annotate("text",
           label = "Evaluaciones \n negativas",
           x = as.Date("1992-06-01"), 
           y = -15,
           size = 3) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  labs(y = "Saldo neto",
       x = "",
       title = "Serie histórica de evaluación del presidente",
       subtitle = "Saldo neto = (% aprobación - % desaprobación), datos de Equipos Consultores",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy') +
  scale_color_manual(name = "",
                     values = c("#5DADE2", "#BA0200", "#BA0200", "#013197",
                                "#013197", "#013197", "#5DADE2")) +
  scale_x_date(date_breaks = "2 years", date_minor_breaks = "1 year",
               date_labels = "%Y", limits = c(as.Date("1990-01-01"), NA))   

plot(aprob_serie_s_e)



## Dyad-ratios  ============================================================

rm(list = ls())

# Cargar data opuy
dat_opuy <- opuy 

# Cargar datos Latinobarómetro y LAPOP
lat_lap <- readxl::read_excel("data/evpresi_lat_lapop.xlsx")

# Unir ambas bases
dat_final <- rbind(lat_lap, dat_opuy)

dat_final <- dat_final %>%
  filter(medicion == 'Evaluacion de gestion presidente') %>%
  select(fecha, anio_gobierno, empresa, valor, presidente, categoria_unificada)  %>%
  mutate(categoria_unificada = case_when(
    categoria_unificada == 3 ~ "Aprueba",  
    categoria_unificada == 2 ~ "Ni aprueba ni desaprueba",
    categoria_unificada == 1 ~ "Desaprueba",
    categoria_unificada == 0 ~ "NSNC")) %>%
  pivot_wider(names_from = categoria_unificada, values_from = valor) %>% 
  mutate(Saldo = Aprueba - Desaprueba) %>% 
  mutate(presidente = factor(presidente, 
                             levels = c("Lacalle", "Sanguinetti 2", "Batlle",
                                        "Vazquez 1", "Mujica", "Vazquez 2",
                                        "Lacalle Pou")))


source("R/Extract.r") # Dyad-ratios function (www.stimson.web.unc.edu)

range(dat_final$fecha)

# Algoritmo de dyad-ratios (Cada fuente cuenta como una serie aparte)
output <- extract(dat_final$empresa, # serie
                  dat_final$fecha, # date
                  dat_final$Aprueba, # score
                  unit = "A", # Units (A = anual)
                  begindt = ISOdate(1990, 08, 13), # Starting date
                  enddt = ISOdate(2021, 09, 03), # End date
                  smoothing = TRUE) # Exponential smoothing applied

display(output) # Series
summary(output) # Loadings

## Results  =================================================================

# Extract series
years <- output$period
pmood <- output$latent1
dim_latente <- data.frame(cbind(years, pmood))
write_xlsx(dim_latente, "results/final-series.xlsx") # Export in excel

# Extract loadings
vars <- output$varname
loads <- as.numeric(output$loadings1)
n_cases <- as.numeric(output$N)
loadings <- data.frame(cbind(vars, loads, n_cases))
loadings <- loadings %>% 
  arrange(desc(loads))
writexl::write_xlsx(loadings, "results/final-loadings.xlsx")


## Plots  ==================================================================

# * Series ----
dim_latente$version <- "final"
mood_append <- dim_latente

# Plot series
ggplot(mood_append, aes(x = years, y = pmood)) +
  geom_line(size = 1.5, alpha = 0.6, colour = "black") +
  geom_point(size = 3, colour = "black") +
  geom_hline(yintercept = 50, linetype="dashed") +
  theme_m() +
  theme(legend.position = "bottom") +
  labs(title = "Policy Mood in Uruguay",
       y = "Liberl score",
       x = "Years") +
  scale_x_continuous(breaks = seq(1993, 2020, by = 2)) 

ggsave(file="plots/pmood/final-series.png", width = 35, height = 20, units = "cm")



