
## **************************************************************************
## Piso I - Opinión Pública
## Código local para documento Markdown
## 10/2021 UMAD
## **************************************************************************

library(tidyverse)
library(opuy)
library(ggridges)
library(gt)
library(scales)
library(gtExtras)


## Evaluación del presidente  ===============================================

## Serie geom_smooth (aprobación y saldo) ==================================

rm(list = ls())

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
            color = "black", size = 3) +
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

# Distribución aprobación
promedio_aprob <- ggplot(dat_opuy %>% 
         filter(presidente != "Lacalle Pou"),
       aes(x = presidente, y = Aprueba)) +
  geom_boxplot(aes(fill = presidente), outlier.shape = NA, lwd = 1, alpha = 0.4) +
  geom_jitter(aes(color = presidente), size = 2, alpha = 0.5) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Evaluación del presidente",
       subtitle = "Diagrama de caja, % de aprobación",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy  
       Datos originales de Equipos, Cifra, Factum, Opción, Interconsult y Radar',
       y = "% de Aprobación",
       x = "") +
  scale_color_manual(name = "",
                     values = c("#5DADE2", "#BA0200", "#BA0200", "#013197",
                                "#013197", "#013197", "#5DADE2")) +
  scale_fill_manual(name = "",
                     values = c("#5DADE2", "#BA0200", "#BA0200", "#013197",
                                "#013197", "#013197", "#5DADE2"))
plot(promedio_aprob)


# Distribución saldo neto
promedio_saldo <- ggplot(dat_opuy %>% 
         filter(presidente != "Lacalle Pou"),
       aes(x = presidente, y = Saldo)) +
  geom_boxplot(aes(fill = presidente), outlier.shape = NA, lwd = 1, alpha = 0.4) +
  geom_jitter(aes(color = presidente), size = 2, alpha = 0.5) +
  geom_hline(yintercept = 0, size = .3, linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Evaluación del presidente",
       subtitle = "Diagrama de caja, saldo neto",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy 
       Datos originales de Equipos, Cifra, Factum, Opción, Interconsult y Radar',
       y = "% de Aprobación",
       x = "") +
  scale_color_manual(name = "",
                     values = c("#5DADE2", "#BA0200", "#BA0200", "#013197",
                                "#013197", "#013197", "#5DADE2")) +
  scale_fill_manual(name = "",
                    values = c("#5DADE2", "#BA0200", "#BA0200", "#013197",
                               "#013197", "#013197", "#5DADE2"))

plot(promedio_saldo)

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
  mutate(trimestre = lubridate::floor_date(fecha, "3 months" )) %>%
  mutate(trimestre = zoo::as.yearqtr(trimestre, format = "%Y-%m-%d")) %>% 
  mutate(quarter = zoo::as.yearqtr(format(trimestre), "%Y Q%q")) %>% 
  mutate(presidente = factor(presidente, 
                             levels = c("Lacalle", "Sanguinetti 2", "Batlle",
                                        "Vazquez 1", "Mujica", "Vazquez 2",
                                        "Lacalle Pou"))) %>% 
  group_by(quarter) %>% 
  summarize(Aprueba = mean(Aprueba),
            Saldo = mean(Saldo),
            presidente = first(presidente)) %>% 
  relocate(presidente, .after = quarter)

# Anotaciones para gráfico
annotation <- data.frame(
  x = fechas <- c(1992.25, 1997.25, 2002.25, 2007.25, 2012.25, 2017.25, 2022.25),
  y = 85,
  label = c("Lacalle","Sanguinetti II", "Batlle", "Vázquez I", "Mujica",
            "Vázquez II", "Lacalle Pou"))

fechas <- c(1994.85, 1999.85, 2004.85, 2009.85, 2014.85, 2019.85)

# Grafico % aprobación
aprob_serie_e <- dat_equipos %>%
  ggplot(aes(x = quarter, y = Aprueba, color = Aprueba)) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = 0.6) +
  geom_vline(xintercept = fechas,
             linetype = "dashed", size = 0.3, color = "grey30") +
  geom_text(data = annotation, aes(x = x, y = y, label = label),
            color = "black", size = 3) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  labs(y = "% de aprobación",
       x = "",
       title = "Serie trimestral de aprobación del presidente",
       subtitle = "Promedio a partir de datos de Equipos",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy') +
  scale_colour_gradient2(low = "firebrick2", mid = "gold2" , high = "forestgreen", 
                         midpoint = mean(dat_equipos$Aprueba)) 

plot(aprob_serie_e)

# Grafico saldo neto
aprob_serie_s_e <- dat_equipos %>%
  ggplot(aes(x = quarter, y = Saldo, color = Saldo)) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = 0.6) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = fechas,
             linetype = "dashed", size = 0.3, color = "grey30") +
  geom_hline(yintercept = 0, size = 0.3) +
  geom_text(data = annotation, aes(x = x, y = y, label = label),
            color = "black", size = 3) +
  annotate("segment", x = 1990.00, y = 15, xend = 1990.00, yend = 30,
           arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
  annotate("text",
           label = "Evaluaciones \n positivas",
           x = 1992.05, 
           y = 20,
           size = 3) +
  annotate("segment", x = 1990.00, y = -15, xend = 1990.00, yend = -30,
           arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
  annotate("text",
           label = "Evaluaciones \n negativas",
           x = 1992.05, 
           y = -20,
           size = 3) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  labs(y = "Saldo neto",
       x = "",
       title = "Serie trimestral de evaluación del presidente",
       subtitle = "Promedio de saldo neto a partir de datos de Equipos",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy') +
  scale_colour_gradient2(low = "firebrick2", mid = "gold2" , high = "forestgreen", 
                         midpoint = mean(dat_equipos$Saldo)) 

plot(aprob_serie_s_e)

# Tabla final
dat_equipos_avg <- dat_equipos %>% 
  filter(presidente != "Lacalle Pou") %>% # Solo mandatos terminados
  group_by(presidente) %>% 
  summarize(aprob_m = round(mean(Aprueba), digits = 1),
            aprob_sd = round(sd(Saldo), digits = 1),
            saldo_m = round(mean(Saldo), digits = 1),
            saldo_sd = round(sd(Saldo), digits = 1)) 

# % Aprobacion
plot_promedio <- ggplot(data = dat_equipos_avg,
                        aes(x = presidente, y = aprob_m, color = presidente)) +
  geom_errorbar(aes(ymin = aprob_m - aprob_sd, ymax = aprob_m + aprob_sd), width=.1) +
  geom_point(size=3) +
  labs(title = "Promedio y desvío estandar de aprobación del presidente según administración",
       subtitle = "Cálculos sobre datos de Equipos Consultores (% de aprobación)",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy',
       y = "", x = "") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  scale_color_manual(name = "",
                     values = c("#5DADE2", "#BA0200", "#BA0200", "#013197",
                                "#013197", "#013197", "#5DADE2")) 

plot(plot_promedio)

# Saldo neto
plot_promedio_s <- ggplot(data = dat_equipos_avg,
                        aes(x = presidente, y = saldo_m, color = presidente)) +
  geom_errorbar(aes(ymin = saldo_m - saldo_sd, ymax = saldo_m + saldo_sd), width=.1) +
  geom_point(size=3) +
  geom_hline(yintercept = 0, size = .3, linetype = "dashed") +
  labs(title = "Promedio y desvío estandar de evaluación del presidente según administración",
       subtitle = "Cálculos sobre datos de Equipos Consultores (saldo neto)",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy',
       y = "", x = "") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  scale_color_manual(name = "",
                     values = c("#5DADE2", "#BA0200", "#BA0200", "#013197",
                                "#013197", "#013197", "#5DADE2")) 

plot(plot_promedio_s)

## Dyad-ratios  ============================================================

## Cálculo aprobacion ----

rm(list = ls())

source("R/Extract.r") # Dyad-ratios function (www.stimson.web.unc.edu)

# Cargar data opuy
dat_opuy <- opuy 

# Cargar datos Latinobarómetro y LAPOP
lat_lap <- readxl::read_excel("data/aprob-dyad-ratio/evpresi_lat_lapop.xlsx")

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
  mutate(fecha = as.Date(fecha)) %>%
  mutate(presidente = factor(presidente, 
                             levels = c("Lacalle", "Sanguinetti 2", "Batlle",
                                        "Vazquez 1", "Mujica", "Vazquez 2",
                                        "Lacalle Pou"))) %>% 
  select(empresa, fecha, Aprueba)

# Algoritmo de dyad-ratios (Cada fuente cuenta como una serie aparte)
output <- extract(dat_final$empresa, # serie
                  dat_final$fecha, # date
                  dat_final$Aprueba, # score
                  ncases = NULL,
                  unit = "Q", # Units (A = anual)
                  smoothing = TRUE) # Exponential smoothing applied

display(output) # Series
summary(output) # Loadings

## Results

# Extract series
years <- output$period
aprob_opuy <- output$latent1
dim_latente <- data.frame(cbind(years, aprob_opuy))

# Extract loadings
vars <- output$varname
loads <- as.numeric(output$loadings1)
n_cases <- as.numeric(output$N)
loadings <- data.frame(cbind(vars, loads, n_cases))
loadings <- loadings %>% 
  arrange(desc(loads))
writexl::write_xlsx(loadings, "data/aprob-dyad-ratio/Q-loadings.xlsx")

## Plots
# * Serie
dim_latente$version <- "opuy"
aprob_serie <- dim_latente

quarters <- expand.grid(quarter = c("Q1", "Q2", "Q3", "Q4"),
                        gdpPercap = seq(1990, 2021, by = 1)) %>% 
  mutate(quarter = paste(quarter, gdpPercap)) %>% 
  slice(3:127)

aprob_serie <- cbind(aprob_serie, quarters$quarter)

aprob_serie <- aprob_serie %>% 
  mutate(quarter = zoo::as.yearqtr(format(aprob_serie$quarter), "Q%q %Y")) %>% 
  select(-`quarters$quarter`)

writexl::write_xlsx(aprob_serie, "data/aprob-dyad-ratio/Q-series.xlsx") # Export in excel

# Plot series
ggplot(aprob_serie, aes(x = quarter, y = aprob_opuy)) +
  geom_line(size = 1, alpha = 0.6, colour = "black") +
  # geom_point(size = 2, colour = "black") +
  geom_hline(yintercept = 50, linetype="dashed") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Serie de aprobación presidencial",
       y = "% aprobación",
       x = "") +
  scale_x_continuous(breaks = seq(1993, 2020, by = 2)) 

## Chequeo con EAD ----
# Leer data de exectuive approval
ead <- read.csv("data/aprob-dyad-ratio/EAD+2.0+quarter+101019.csv") %>% 
  as_tibble() %>% 
  filter(Country == "Uruguay") %>% 
  mutate(quarter = paste0("Q", quarter)) %>% 
  mutate(quarter = paste(quarter, year)) %>% 
  mutate(quarter = zoo::as.yearqtr(format(quarter), "Q%q %Y")) %>% 
  select(quarter, Approval_Smoothed)

# Pegar data a estimación opuy
aprob_serie <- aprob_serie %>% 
  left_join(ead) %>% 
  relocate(aprob_opuy, .after = Approval_Smoothed) %>% 
  pivot_longer(cols = Approval_Smoothed:aprob_opuy,
               names_to = "serie",
               values_to = "valor")

# Plot series
ggplot(aprob_serie, aes(x = quarter, y = valor, color = serie)) +
  geom_line(size = 1, alpha = 0.6) +
  # geom_point(size = 2, colour = "black") +
  geom_hline(yintercept = 50, linetype="dashed") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Serie de aprobación presidencial",
       y = "% aprobación",
       x = "") +
  ylim(0, 100) +
  scale_x_continuous(breaks = seq(1993, 2020, by = 2)) 


## Cálculo saldo neto ----

rm(list = ls())

source("R/Extract.r") # Dyad-ratios function (www.stimson.web.unc.edu)

# Cargar data opuy
dat_opuy <- opuy 

# Cargar datos Latinobarómetro y LAPOP
lat_lap <- readxl::read_excel("data/aprob-dyad-ratio/evpresi_lat_lapop.xlsx")

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
  mutate(fecha = as.Date(fecha)) %>% 
  mutate(presidente = factor(presidente, 
                             levels = c("Lacalle", "Sanguinetti 2", "Batlle",
                                        "Vazquez 1", "Mujica", "Vazquez 2",
                                        "Lacalle Pou"))) %>% 
  select(empresa, fecha, Saldo)


# Algoritmo de dyad-ratios (Cada fuente cuenta como una serie aparte)
output <- extract(dat_final$empresa, # serie
                  dat_final$fecha, # date
                  dat_final$Saldo, # score
                  ncases = NULL,
                  unit = "Q", # Units (A = anual)
                  smoothing = TRUE) # Exponential smoothing applied

display(output) # Series
summary(output) # Loadings

## Results

# Extract series
years <- output$period
aprob_opuy <- output$latent1
dim_latente <- data.frame(cbind(years, aprob_opuy))

# Extract loadings
vars <- output$varname
loads <- as.numeric(output$loadings1)
n_cases <- as.numeric(output$N)
loadings <- data.frame(cbind(vars, loads, n_cases))
loadings <- loadings %>% 
  arrange(desc(loads))
writexl::write_xlsx(loadings, "data/aprob-dyad-ratio/Q-saldo-loadings.xlsx")


## Plots

# * Serie
dim_latente$version <- "opuy"
aprob_serie <- dim_latente

quarters <- expand.grid(quarter = c("Q1", "Q2", "Q3", "Q4"),
                        gdpPercap = seq(1990, 2021, by = 1)) %>% 
  mutate(quarter = paste(quarter, gdpPercap)) %>% 
  slice(3:127)

aprob_serie <- cbind(aprob_serie, quarters$quarter)

aprob_serie <- aprob_serie %>% 
  mutate(quarter = zoo::as.yearqtr(format(aprob_serie$quarter), "Q%q %Y")) %>% 
  select(- `quarters$quarter`)

writexl::write_xlsx(aprob_serie, "data/aprob-dyad-ratio/Q-saldo-series.xlsx") # Export in excel

# Plot series
ggplot(aprob_serie, aes(x = quarter, y = aprob_opuy)) +
  geom_line(size = 1, alpha = 0.6, colour = "black") +
  # geom_point(size = 2, colour = "black") +
  geom_hline(yintercept = 0, linetype="dashed") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Serie de aprobación presidencial",
       y = "% saldo neto",
       x = "") +
  scale_x_continuous(breaks = seq(1993, 2020, by = 2)) 

## Chequeo con EAD ----
# Leer data de exectuive approval
ead <- read.csv("data/aprob-dyad-ratio/EAD+2.0+quarter+101019.csv") %>% 
  as_tibble() %>% 
  filter(Country == "Uruguay") %>% 
  mutate(quarter = paste0("Q", quarter)) %>% 
  mutate(quarter = paste(quarter, year)) %>% 
  mutate(quarter = zoo::as.yearqtr(format(quarter), "Q%q %Y")) %>% 
  select(quarter, NET_Smoothed)

# Pegar data a estimación opuy
aprob_serie <- aprob_serie %>% 
  left_join(ead) %>% 
  relocate(aprob_opuy, .after = NET_Smoothed) %>% 
  pivot_longer(cols = NET_Smoothed:aprob_opuy,
               names_to = "serie",
               values_to = "valor")

# Plot series
ggplot(aprob_serie, aes(x = quarter, y = valor, color = serie)) +
  geom_line(size = 1, alpha = 0.6) +
  # geom_point(size = 2, colour = "black") +
  geom_hline(yintercept = 50, linetype="dashed") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Serie de aprobación presidencial",
       y = "% aprobación",
       x = "") +
  ylim(-100, 100) +
  scale_x_continuous(breaks = seq(1993, 2020, by = 2)) 

## Gráficos para piso 1 OP  ----

rm(list = ls())

# Genero tabla primero
aprob_serie <- readxl::read_excel("data/aprob-dyad-ratio/Q-series.xlsx") %>% 
  rename(aprobacion = aprob_opuy)

saldo_serie <- readxl::read_excel("data/aprob-dyad-ratio/Q-saldo-series.xlsx") %>% 
  rename(saldo = aprob_opuy)

serie_dyad_ratios <- cbind(aprob_serie, saldo_serie$saldo) %>% 
  rename(saldo = `saldo_serie$saldo`) %>% 
  select(quarter, aprobacion, saldo) %>% 
  mutate(aprobacion = round(aprobacion, digits = 1),
         saldo = round(saldo, digits = 1)) %>%
  mutate(presidencia = case_when(
    quarter < 1995.00 ~ "Lacalle",
    quarter >= 1995.00 & quarter < 2000.00 ~ "Sanguinetti II",
    quarter >= 2000.00 & quarter < 2005.00 ~ "Batlle",
    quarter >= 2005.00 & quarter < 2010.00 ~ "Vázquez I",
    quarter >= 2010.00 & quarter < 2015.00 ~ "Mujica",
    quarter >= 2015.00 & quarter < 2020.00 ~ "Vázquez II",
    quarter >= 2020.00 & quarter < 2025.00 ~ "Lacalle Pou"
  )) %>% 
  mutate(quarter = as.character(zoo::as.yearqtr(quarter))) %>% 
  relocate(presidencia, .after=quarter)

writexl::write_xlsx(serie_dyad_ratios,
                    "data/aprob-dyad-ratio/serie_dyad_ratios.xlsx")

rm(list = ls())

# Cargo la tabla
serie_dr <- readxl::read_excel("data/aprob-dyad-ratio/serie_dyad_ratios.xlsx") %>% 
  mutate(quarter = zoo::as.yearqtr(format(quarter), "%Y Q%q"))

# Anotaciones para gráfico
annotation <- data.frame(
  x = fechas <- c(1992.25, 1997.25, 2002.25, 2007.25, 2012.25, 2017.25, 2022.25),
  y = 85,
  label = c("Lacalle","Sanguinetti II", "Batlle", "Vázquez I", "Mujica",
            "Vázquez II", "Lacalle Pou"))

fechas <- c(1994.85, 1999.85, 2004.85, 2009.85, 2014.85, 2019.85)

# Grafico % aprobación
plot_dr_a <- serie_dr %>%
  ggplot(aes(x = quarter, y = aprobacion, color = aprobacion)) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = 0.6) +
  geom_vline(xintercept = fechas,
             linetype = "dashed", size = 0.3, color = "grey30") +
  geom_text(data = annotation, aes(x = x, y = y, label = label),
            color = "black", size = 3) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  labs(y = "% de aprobación",
       x = "",
       title = "Serie histórica de aprobación del presidente",
       subtitle = "Estimación utilizando el algoritmo de dyads-ratio",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy  
       Datos originales de Equipos, Cifra, Factum, Opción, Interconsult y Radar') +
  scale_colour_gradient2(low = "firebrick2", mid = "gold2" , high = "forestgreen", 
                         midpoint = mean(serie_dr$aprobacion)) 

plot(plot_dr_a)

## Serie de saldo neto dyad-ratios 

# Grafico % aprobación
plot_dr_s <- serie_dr %>%
  ggplot(aes(x = quarter, y = saldo, color = saldo)) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = 0.6) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = fechas,
             linetype = "dashed", size = 0.3, color = "grey30") +
  geom_hline(yintercept = 0, size = 0.3) +
  geom_text(data = annotation, aes(x = x, y = y, label = label),
            color = "black", size = 3) +
  annotate("segment", x = 1990.00, y = 30, xend = 1990.00, yend = 45,
           arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
  annotate("text",
           label = "Evaluaciones \n positivas",
           x = 1992.05, 
           y = 35,
           size = 3) +
  annotate("segment", x = 1990.00, y = -30, xend = 1990.00, yend = -45,
           arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
  annotate("text",
           label = "Evaluaciones \n negativas",
           x = 1992.05, 
           y = -35,
           size = 3) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  labs(y = "saldo neto",
       x = "",
       title = "Serie histórica de evaluación del presidente",
       subtitle = "Estimación utilizando el algoritmo de dyads-ratio (saldo neto)",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy  
       Datos originales de Equipos, Cifra, Factum, Opción, Interconsult y Radar') +
  scale_colour_gradient2(low = "firebrick2", mid = "gold2" , high = "forestgreen", 
                         midpoint = mean(serie_dr$saldo)) +
  ylim(-50, 90)

plot(plot_dr_s)


# Gráfico promedio ----

# Cargo la tabla
serie_dr <- readxl::read_excel("data/aprob-dyad-ratio/serie_dyad_ratios.xlsx") %>% 
  mutate(quarter = zoo::as.yearqtr(format(quarter), "%Y Q%q"))

dat_promedio <- serie_dr %>% 
  mutate(presidencia = factor(presidencia, levels = c("Lacalle", "Sanguinetti II", 
                                                      "Batlle", "Vázquez I", "Mujica", 
                                                      "Vázquez II", "Lacalle Pou"))) %>%
  group_by(presidencia) %>% 
  filter(presidencia != "Lacalle Pou") %>% # Solo mandatos terminados
  summarize(aprob_m = round(mean(aprobacion), digits = 1),
            aprob_sd = round(sd(aprobacion), digits = 1),
            saldo_m = round(mean(saldo), digits = 1),
            saldo_sd = round(sd(saldo), digits = 1)) 

# Gráfico Aprobación
plot_promedio <- ggplot(data = dat_promedio,
       aes(x = presidencia, y = aprob_m, color = presidencia)) +
  geom_errorbar(aes(ymin = aprob_m - aprob_sd, ymax = aprob_m + aprob_sd), width=.1) +
  geom_point(size=3) +
  geom_text(aes(label = aprob_m),hjust = -.5, vjust = .5) +
  labs(title = "Promedio y desvío estandar de aprobación del presidente según administración",
       subtitle = "Cálculos sobre datos estimados mediante el algoritmo de dyads-ratio (% de aprobación)",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy  
       Datos originales de Equipos, Cifra, Factum, Opción, Interconsult y Radar',
       x = "",
       y = "") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  scale_color_manual(name = "",
                     values = c("#5DADE2", "#BA0200", "#BA0200", "#013197",
                                "#013197", "#013197", "#5DADE2")) 

plot(plot_promedio)


# Saldo neto
plot_promedio_s <- ggplot(data = dat_promedio,
                          aes(x = presidencia, y = saldo_m, color = presidencia)) +
  geom_errorbar(aes(ymin = saldo_m - saldo_sd, ymax = saldo_m + saldo_sd), width=.1) +
  geom_point(size=3) +
  geom_text(aes(label = saldo_m),hjust = -.5, vjust = .5) +
  geom_hline(yintercept = 0, size = .3, linetype = "dashed") +
  labs(title = "Promedio y desvío estandar de evaluación del presidente según administración",
       subtitle = "Cálculos sobre datos estimados mediante el algoritmo de dyads-ratio (saldo neto)",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy  
       Datos originales de Equipos, Cifra, Factum, Opción, Interconsult y Radar',
       y = "", x = "") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  scale_color_manual(name = "",
                     values = c("#5DADE2", "#BA0200", "#BA0200", "#013197",
                                "#013197", "#013197", "#5DADE2")) 

plot(plot_promedio_s)

## Ciclos de aprobación ----

rm(list = ls())

# Cargo la tabla
serie_dr <- readxl::read_excel("data/aprob-dyad-ratio/serie_dyad_ratios.xlsx") %>% 
  mutate(quarter = zoo::as.yearqtr(format(quarter), "%Y Q%q"))


trimestres <- c(seq(1, 18, 1), seq(1, 20, 1), seq(1, 20, 1), seq(1, 20, 1),
                seq(1, 20, 1), seq(1, 20, 1))

serie_dr <- serie_dr %>% 
  filter(presidencia != "Lacalle Pou") # Quito por no estar completo

serie_dr$trimestre <- trimestres

## Promedio general
promedios <- serie_dr %>% 
  group_by(trimestre) %>% 
  summarize(aprobacion = mean(aprobacion),
            saldo = mean(saldo))

ggplot(promedios,
       aes(x = trimestre, y = aprobacion)) +
  geom_point(size=3) +
  geom_smooth(se = FALSE) +
  theme_minimal(base_size = 10) 

## Mean center
# Calculo promedios
avgs <- serie_dr %>% 
  group_by(presidencia) %>% 
  summarise(media_aprob = mean(aprobacion))

serie_dr <- serie_dr %>% 
  left_join(avgs) %>% 
  mutate(dif = aprobacion - media_aprob)
  
promedios <- serie_dr %>% 
  mutate(anio = trimestre/ 4) %>% 
  group_by(anio) %>% 
  summarize(aprob_dif = mean(dif))

ggplot(promedios,
       aes(x = anio, y = aprob_dif)) +
  geom_point(size = 3, alpha = .4, colour = "#2c3e50") +
  geom_smooth(se = FALSE, span = 1, size = 1.5, colour = "#2c3e50") +
  theme_minimal(base_size = 12) +
  geom_hline(yintercept = 0, size = .3, linetype = "dashed") +
  labs(title = "Ciclos de aprobación presidencial en Uruguay",
       subtitle = "Tendencia de aprobación trimestral respecto a la media de cada período",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de opuy',
       y = "", x = "Años de gobierno") +
  annotate("segment", x = 1.5, y = 7.5, xend = 1, yend = 7.5,
           arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
  annotate("text",
           label = "Luna de miel",
           x = 1.25, 
           y = 8,
           size = 4)  +
  annotate("segment", x = 2, y = 5, xend = 2, yend = 2.5,
           arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
  annotate("text",
           label = "Declive",
           x = 2.35, 
           y = 3.75,
           size = 4)  +
  annotate("segment", x = 4, y = 2.5, xend = 4.5, yend = 2.5,
           arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
  annotate("text",
           label = "Repunte",
           x = 4.25, 
           y = 3,
           size = 4) 

ggsave("www/ciclos.png", units = "cm", width = 15, height = 20)


## EAD COMPARADO ----

rm(list = ls())

ead <- read.csv("data/aprob-dyad-ratio/EAD+2.0+quarter+101019.csv") %>% 
  as_tibble() %>% 
  mutate(quarter = paste0("Q", quarter)) %>% 
  mutate(quarter = paste(quarter, year)) %>% 
  mutate(quarter = zoo::as.yearqtr(format(quarter), "Q%q %Y")) %>% 
  mutate(color_uru = case_when(
    Country == "Uruguay" ~ "Uruguay",
    TRUE ~ "Others"
  )) %>% 
  filter(year >= 1989)

table(ead$Country)

paises <- c("Argentina", "Chile", "Brazil", "Bolivia", "Colombia",
            "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")

ead_latam <- ead %>% 
  filter(Country %in% paises)

## No usar, series crudas
ggplot(ead_latam,
       aes(x = quarter, y = Approval_Smoothed, group = Country, color = color_uru)) +
  geom_line(size = 1) +
  scale_color_manual(values = c(adjustcolor("grey60", alpha = .4),
                                "deepskyblue3")) +
  theme_minimal()


ggplot(ead %>% 
         filter(Country %in% paises),
       aes(x = quarter, y = NET_Smoothed, group = Country, color = color_uru)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = .3, linetype = "dashed") +
  scale_color_manual(values = c(adjustcolor("grey60", alpha = .4),
                                "deepskyblue3")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", 
       y = "Saldo neto")

## Promedio por presidente

# Data de mandatos
presidencias <- readxl::read_excel("data/presidencias-asur.xlsx") %>% 
  mutate(Comienzo_qtr = zoo::as.yearqtr(Comienzo, format = "%Y-%m-%d")) %>% 
  mutate(Final_qtr = zoo::as.yearqtr(Final, format = "%Y-%m-%d")) %>% 
  pivot_longer(Comienzo_qtr:Final_qtr,
               names_to = "tipo",
               values_to = "quarter") %>% 
  mutate(key = paste0(as.character(quarter), "-", Pais)) 
  
# Base vacía
quarters <- expand.grid(quarter = c("Q1", "Q2", "Q3", "Q4"),
                        gdpPercap = seq(1989, 2021, by = 1)) %>% 
  mutate(quarter = paste(quarter, gdpPercap)) %>% 
  mutate(quarter = zoo::as.yearqtr(quarter, format = "Q%q %Y")) 

data <- expand.grid(quarter = quarters$quarter,
                    paises = paises) %>%
  mutate(key = paste0(as.character(quarter), "-", paises)) %>% 
  left_join(presidencias, by = "key")

# Completo
dat <- data %>% 
  group_by(paises) %>% 
  fill(Presidencia, Ciclo_Cumplido) %>% 
  ungroup() %>% 
  fill(Presidencia, Ciclo_Cumplido, .direction = "up") 

# Pego a data EAD
ead_latam <- ead_latam %>% 
  mutate(key = paste0(as.character(quarter), "-", Country)) %>% 
  left_join(dat) %>% 
  select(quarter, Country, Approval_Smoothed, Presidencia, Ciclo_Cumplido)
  
writexl::write_xlsx(ead_latam, "data/ead_latam.xlsx")

# Asignar trimestres con dos presidentes al que tiene más 
ead_latam <- ead_latam %>% 
  filter(
    !(quarter == "1995 Q2" & Presidencia == "Carlos Menem"),
    !(quarter == "1999 Q3" & Presidencia == "Fernando de la Rúa"),
    !(quarter == "2001 Q4" & Presidencia == "Adolfo Rodriguez Saá"),
    !(quarter == "2011 Q4" & Presidencia == "Cristina Fernández de Kirchner II"),
    !(quarter == "2015 Q4" & Presidencia == "Mauricio Macri"),
    !(quarter == "2001 Q3" & Presidencia == "Jorge Quiroga Ramírez"),
    !(quarter == "2002 Q3" & Presidencia == "Jorge Quiroga Ramírez"),
    !(quarter == "2003 Q4" & Presidencia == "Carlos Mesa Gisbert"),
    !(quarter == "2005 Q2" & Presidencia == "Eduardo Rodríguez Veltzé"),
    !(quarter == "2006 Q1" & Presidencia == "Eduardo Rodríguez Veltzé"),
    !(quarter == "2010 Q1" & Presidencia == "Evo Morales"),
    !(quarter == "2015 Q1" & Presidencia == "Evo Morales II"),
    !(quarter == "1992 Q4" & Presidencia == "Itamar Franco"),
    !(quarter == "2016 Q3" & Presidencia == "Michel Temer"),
    !(quarter == "1994 Q1" & Presidencia == "Eduardo Frei Ruiz-Tagle"),
    !(quarter == "2000 Q1" & Presidencia == "Ricardo Lagos"),
    !(quarter == "2006 Q1" & Presidencia == "Michelle Bachelet"),
    !(quarter == "2010 Q1" & Presidencia == "Sebastián Piñera"),
    !(quarter == "2014 Q1" & Presidencia == "Michelle Bachelet II"),
    !(quarter == "2018 Q1" & Presidencia == "Sebastián Piñera II"),
    !(quarter == "1994 Q3" & Presidencia == "Ernesto Samper"),
    !(quarter == "1998 Q3" & Presidencia == "Andrés Pastrana"),
    !(quarter == "2002 Q3" & Presidencia == "Álvaro Uribe"),
    !(quarter == "2006 Q3" & Presidencia == "Álvaro Uribe II"),
    !(quarter == "2018 Q3" & Presidencia == "Iván Duque"),
    !(quarter == "1992 Q3" & Presidencia == "Rodrigo Borja"),
    !(quarter == "1996 Q3" & Presidencia == "Sixto Durán-Ballén"),
    !(quarter == "1997 Q1" & Presidencia == "Abdalá Bucaram"),
    !(quarter == "1998 Q3" & Presidencia == "Fabían Alarcón"),
    !(quarter == "2000 Q1" & Presidencia == "Jamil Mahuad Witt"),
    !(quarter == "2003 Q1" & Presidencia == "Gustavo Noboa"),
    !(quarter == "2005 Q2" & Presidencia == "Alfredo Palacio González"),
    !(quarter == "2007 Q1" & Presidencia == "Alfredo Palacio González"),
    !(quarter == "2009 Q3" & Presidencia == "Rafael Correa"),
    !(quarter == "2013 Q2" & Presidencia == "Rafael Correa III"),
    !(quarter == "2017 Q2" & Presidencia == "Lenin Moreno"),
    !(quarter == "1998 Q3" & Presidencia == "Raúl Cubas"),
    !(quarter == "1999 Q1" & Presidencia == "Luis Ángel González"),
    !(quarter == "2008 Q3" & Presidencia == "Nicanor Duarte"),
    !(quarter == "2012 Q2" & Presidencia == "Federico Franco"),
    !(quarter == "2013 Q3" & Presidencia == "Federico Franco"),
    !(quarter == "2018 Q3" & Presidencia == "Mario Abdo"),
    !(quarter == "2001 Q3" & Presidencia == "Valentín Paniagua"),
    !(quarter == "2006 Q3" & Presidencia == "Alejandro Toledo"),
    !(quarter == "2011 Q3" & Presidencia == "Alan Garcúa"),
    !(quarter == "2016 Q3" & Presidencia == "Ollanta Humala"),
    !(quarter == "1995 Q1" & Presidencia == "Luis Alberto Lacalle"),
    !(quarter == "2000 Q1" & Presidencia == "Julio María Sanguinetti"),
    !(quarter == "2005 Q1" & Presidencia == "Jorge Batlle"),
    !(quarter == "2010 Q1" & Presidencia == "Tabaré Vázquez"),
    !(quarter == "2015 Q1" & Presidencia == "José Mujica"),
    !(quarter == "1993 Q2" & Presidencia == "Octavio Lepage"),
    !(quarter == "1994 Q1" & Presidencia == "Ramón José Velásquez"),
    !(quarter == "1999 Q1" & Presidencia == "Rafael Caldera"),
    !(quarter == "1999 Q4" & Presidencia == "Hugo Chávez II"),
    !(quarter == "2001 Q1" & Presidencia == "Hugo Chávez II"),
    !(quarter == "2002 Q2" & Presidencia == "Hugo Chávez III"),
    !(quarter == "2007 Q1" & Presidencia == "Hugo Chávez IIII"),
    !(quarter == "2013 Q1" & Presidencia == "Hugo Chávez IIIII"),
    !(quarter == "2013 Q2" & Presidencia == "Nicolás Maduro II")
  )

# Promedio según presidente
ead_latam_r <- ead_latam %>%
  filter(Ciclo_Cumplido == 1) %>% 
  group_by(Presidencia) %>% 
  summarize(aprob = mean(Approval_Smoothed, na.rm=T),
            pais = first(Country))

tibble(
  val1 = c(3, 2, 4),
  val2 = c(1, 4, 5),
  val3 = c(5, 8, 6),
  cat = factor(month.name[1:3], levels = rev(month.name[1:3]))
) -> xdf

# library(hrbrthemes)

ggplot() +
  # reshape the data frame & get min value so you can draw an eye-tracking line (this is one geom)
  geom_segment(
    data = ead_latam_r %>% 
      group_by(pais) %>% 
      top_n(-1) %>% 
      slice(1) %>%
      ungroup(),
    aes(x = 0, xend = aprob, y = reorder(pais, desc(pais)), yend = reorder(pais, desc(pais))),
    linetype = "dotted", size = 0.5, color = "gray50"
  ) +
  # reshape the data frame & get min/max category values so you can draw the segment (this is another geom)
  geom_segment(
    data = ead_latam_r %>% 
      group_by(pais) %>% 
      summarise(start = range(aprob)[1], end = range(aprob)[2]) %>% 
      ungroup(),
    aes(x = start, xend = end, y = reorder(pais, desc(pais)), yend = reorder(pais, desc(pais))),
     size = 2, alpha = .2
  ) +
  # reshape the data frame & plot the points
  geom_point(
    data = ead_latam_r,
    aes(aprob, reorder(pais, desc(pais)), group = Presidencia, fill = pais), 
    size = 4, alpha = .9, shape = 21
  ) +
  labs(
    x = "% Promedio de aprobación", y = NULL,
    title = "Promedio de aprobación presidencial por país (ciclos completos)",
    caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de Executive Approval Project'
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12)) +
  scale_fill_manual(values = c("lightblue2",
                                "forestgreen",
                                "gold2",
                                "firebrick2",
                                "gold2",
                                "gold2",
                                "firebrick2",
                                "firebrick2",
                                "lightblue2",
                                "darkorchid4"))

ggsave("data/aprob_comparada.png", width = 17.5, height = 22, units = "cm")


## Autoidentificación ideológica ----

# Cargar tabla con datos y cambiar formato a largo
autid <- readxl::read_excel("data/latino/autid.xlsx") %>% 
  mutate(Media = round(as.numeric(Media), digits = 1)) %>% 
  mutate(sd = round(as.numeric(sd), digits = 1))

uy_media <- autid %>% 
  filter(pais == "Uruguay") 

autid <- autid %>% 
  pivot_longer(`0`:n, 
               names_to = "cat",
               values_to = "valor") %>% 
  mutate(valor = as.numeric(valor)) 

# Filtrar por Uruguay y preparar datos para gráfico
autid_uy <- autid %>% 
  filter(pais == "Uruguay") %>% 
  filter(cat != "Media",
         cat != "n",
         cat != "sd") %>% 
  mutate(valor = valor * 1000) %>% 
  select(-pais) %>% 
  filter(!is.na(valor))

# Expandir datos agregados a observaciones
expanded <- data.frame(autid = rep(autid_uy$cat, autid_uy$valor),
                       year = rep(autid_uy$year, autid_uy$valor)) %>% 
  mutate(Partido = case_when(
    year >= 1995 & year <= 2004 ~ "Partido Colorado",
    year >= 2005 & year <= 2019 ~ "Frente Amplio",
    year >= 2020  ~ "Partido Nacional",
  ))

# Distribución 
ggplot(expanded, 
       aes(x = as.numeric(autid), y = fct_rev(as.factor(year)), fill = Partido)) + 
  ggridges::geom_density_ridges(rel_min_height = 0.005, 
                                scale = 3,
                                alpha = .4,
                                color = "black",
                                linetype = 1, # Tipo de línea
                                lwd = 0.5,
                                quantile_lines = TRUE, 
                                quantile_fun = mean) +
  theme_minimal() +
  theme(legend.position = "none") +
  # stat_density_ridges(quantile_lines = TRUE, alpha = 0.75, quantiles = 2) +
  labs(title = "Distribución de autoidentificación ideológica por año",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de Latinobarómetro',
       x = "", y = "") +
  scale_fill_manual(values = c("#013197", "#BA0200",  "#5DADE2")) +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  geom_label(data = uy_media,
             aes(label = Media , x= 12), fill = "white",
             fontface="bold", size=4, show.legend = FALSE, vjust=-.25) +
  geom_label(data = uy_media,
             aes(label = Media , x= 12.5), fill = "white",
             fontface="bold", size=4, show.legend = FALSE, vjust=-.25) +
  geom_label(data = uy_media,
             aes(label = sd , x= 14), fill = "white",
             fontface="bold", size=4, show.legend = FALSE, vjust=-.25) +
  annotate("text",
           label = "Media",
           x = 12.5, 
           y = 23.5,
           fontface = "bold") +
  annotate("text",
           label = "Desvío",
           x = 14, 
           y = 23.5,
           fontface = "bold") 

ggsave("data/latino/ideo.png", height = 30, width = 20, units = "cm")


# Comparación Uru vs la región
autid_la <- autid %>% 
  filter(pais %in% c("Total América Latina", "Uruguay")) 

autid_la_uy <- autid_la %>% 
  filter(cat != "Media",
         cat != "n",
         cat != "sd") %>% 
  mutate(valor = valor * 1000) %>% 
  filter(!is.na(valor))

# Expandir datos agregados a observaciones
df_expanded <- data.frame(autid = rep(autid_la_uy$cat, autid_la_uy$valor),
                       year = rep(autid_la_uy$year, autid_la_uy$valor),
                       pais = rep(autid_la_uy$pais, autid_la_uy$valor))

# Gráfico
ggplot(df_expanded,
       aes(x = as.numeric(autid), y = fct_rev(as.factor(year)), 
           fill = pais, linetype = pais)) + 
  geom_density_ridges(alpha = .6, 
                      quantile_lines = TRUE, 
                      quantile_fun = mean) +
  scale_linetype_manual(name = "",
                        values = c("solid", "dashed")) +
  scale_fill_manual(name = "",
                    values = c("gold2", 
                               "dodgerblue3")) +
  labs(title = "Distribución de autoidentificación ideológica por año, Uruguay y Total América Latina",
       caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de Latinobarómetro',
       x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) 

ggsave("data/latino/ideo_comp.png", height = 30, width = 20, units = "cm")

# # Media y desvío
# autid_avg <- expanded %>% 
#   mutate(autid = as.numeric(autid)) %>% 
#   filter(!is.na(autid)) %>% 
#   group_by(year) %>% 
#   summarize(autid_m = round(mean(autid, na.rm = T), digits = 1),
#             autid_sd = round(sd(autid, na.rm = T), digits = 1),
#             Partido = first(Partido)) 
# 
# # Gráfico media
# ggplot(data = autid_avg,
#                         aes(x = year, y = autid_m, color = Partido)) +
#   geom_errorbar(aes(ymin = autid_m - autid_sd, ymax = autid_m + autid_sd), width=.1) +
#   geom_point(size=3) +
#   geom_text(aes(label = autid_m), vjust = -.5) +
#   labs(title = "Promedio y desvío estandar de aprobación del presidente según administración",
#        subtitle = "Cálculos sobre datos estimados mediante el algoritmo de dyads-ratio (% de aprobación)",
#        caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de Latinobarómetro',
#        x = "",
#        y = "") +
#   theme_minimal(base_size = 10) +
#   theme(legend.position = "none") +
#   scale_color_manual(name = "",
#                      values = c("#5DADE2", "#BA0200", "#BA0200", "#013197",
#                                 "#013197", "#013197", "#5DADE2")) +
#   ylim(0, 10)


library(gt)
library(scales)
library(gtExtras)

# Cargar tabla con datos y cambiar formato a largo
autid <- readxl::read_excel("data/latino/autid.xlsx") %>% 
  mutate(Media = round(as.numeric(Media), digits = 1)) %>% 
  mutate(sd = round(as.numeric(sd), digits = 1))

autid20 <- autid %>% 
  filter(year == '2020\r') %>% 
  select(pais, Media, sd) 

autid_n <- readxl::read_excel("data/latino/autid_ninguno.xlsx") %>% 
  arrange(desc(ninguno)) %>% 
  mutate(Ninguno = round(ninguno * 100, digits = 0)) %>% 
  # mutate(Ninguno = paste(round(ninguno * 100, digits = 0), "%")) %>% 
  select(-ninguno, -year) %>% 
  left_join(autid20) %>% 
  relocate(Ninguno, .after = last_col()) 

(tab <- gt(autid_n) %>% 
  cols_label(pais = "País",
             sd = "Desvío estandar",
             Ninguno = "% sin identificación") %>% 
  tab_header(title = md("Ideología simbólica en América Latina en 2020")) %>% 
  tab_source_note(source_note = "Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de Latinobarómetro"))

(tab <- tab %>% 
    #Apply new style to all column headers
    tab_style(
      locations = cells_column_labels(columns = everything()),
      style     = list(
        #Give a thick border below
        cell_borders(sides = "bottom", weight = px(3)),
        #Make text bold
        cell_text(weight = "bold")
      )
    ) %>% 
    #Apply different style to the title
    tab_style(
      locations = cells_title(groups = "title"),
      style     = list(
        cell_text(weight = "bold", size = 24)
      )
    ) %>% 
    cols_align(
      align = c("center"),
    ) %>% 
    cols_align(
      align = c("left"),
      columns = pais
    ))
    
min_id <- min(autid_n$Ninguno)
max_id <- max(autid_n$Ninguno)
id_palette <- col_numeric(c("#FEF0D9", "#990000"),
                          domain = c(min_id, max_id), 
                          alpha = 0.75)

# Con color
(tab_1 <- tab %>%
      data_color(columns = c(Ninguno),
               colors = id_palette)) %>% 
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )) 

# Con bar
(tab_2 <- tab %>%
    gt_plt_bar(column = Ninguno, scaled = TRUE, 
                   color = "#2c3e50", scale_type = "number") %>% 
    gt_highlight_rows(rows = 18, fill = "lightblue",
                      bold_target_only = TRUE, target_col = pais) %>% 
    gt_highlight_rows(rows = 8, fill = "lightgrey",
                      bold_target_only = TRUE, target_col = pais))
  
# Con bar 2
(tab_3 <- tab %>%
      gtExtras::gt_plt_bar(
        Ninguno, color = "#2874A6",
        width = 40, scale_type = "number"))



gt(autid_n) %>% 
  gtExtras::gt_duplicate_column(
    Ninguno, dupe_name = "mean_plot") %>% 
  gtExtras::gt_plt_bar(
    mean_plot, color = "lightblue",
    width = 40, scale_type = "number") %>% 
  gtExtras::gt_theme_nytimes() %>% 
  cols_label(
    mean = html("Average<br>2013-17"),
    mean_plot = ""
  )

## Confianzas 2020 ----

# Cargo la tabla (la que se presenta en el markdown)
confianza <- readxl::read_excel("data/latino/confianzas2020.xlsx") %>% 
  mutate(Mucha = round(Mucha * 100, digits = 0),
         Algo = round(Algo * 100, digits = 0),
         Poca = round(Poca * 100, digits = 0),
         Ninguna = round(Ninguna * 100, digits = 0)) %>% 
  mutate(Saldo = (Mucha + Algo) - (Poca + Ninguna))  

conf_rec <- confianza %>% 
  mutate(confia = Mucha + Algo) %>% 
  pivot_longer(Mucha:confia,
               names_to = "cat",
               values_to = "valor") 

# Confianza - Instituciones Políticas
confia_pol <- conf_rec %>% 
  filter(cat == "confia") %>% 
  filter(Institucion %in% c("Congreso", "Gobierno", "Presidente",
                         "Partidos Políticos"))

ggplot() +
  geom_segment(
    data = confia_pol %>% 
      group_by(Pais) %>% 
      top_n(-1) %>% 
      slice(1) %>%
      ungroup(),
    aes(x = 0, xend = 100, y = reorder(Pais, desc(Pais)), yend = reorder(Pais, desc(Pais))),
    linetype = "dotted", size = 0.5, color = "gray50"
  ) +
  geom_segment(
    data = confia_pol %>% 
      group_by(Pais) %>% 
      summarise(start = range(valor)[1], end = range(valor)[2]) %>% 
      ungroup(),
    aes(x = start, xend = end, y = reorder(Pais, desc(Pais)), yend = reorder(Pais, desc(Pais))),
    size = 2, alpha = .2
  ) +
  geom_point(
    data = confia_pol,
    aes(valor, reorder(Pais, desc(Pais)), group = Institucion, fill = Institucion), 
    size = 4, alpha = .9, shape = 21
  ) +
  labs(
    x = "% que confía mucho o algo", y = NULL,
    title = "Confianza en insituciones políticas en 2020 según país",
    caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de Latinobarómetro'
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = 12)) +
  scale_fill_brewer(name = "", palette = "Dark2") +
  xlim(0, 100)


# Confianza - Otras Instituciones
confia_otr <- conf_rec %>% 
  filter(cat == "confia") %>% 
  filter(Institucion %in% c("Policía", "Fuerzas armadas", "Iglesia", "Poder Judicial"))

ggplot() +
  geom_segment(
    data = confia_otr %>% 
      group_by(Pais) %>% 
      top_n(-1) %>% 
      slice(1) %>%
      ungroup(),
    aes(x = 0, xend = 100, y = reorder(Pais, desc(Pais)), yend = reorder(Pais, desc(Pais))),
    linetype = "dotted", size = 0.5, color = "gray50"
  ) +
  geom_segment(
    data = confia_otr %>% 
      group_by(Pais) %>% 
      summarise(start = range(valor)[1], end = range(valor)[2]) %>% 
      ungroup(),
    aes(x = start, xend = end, y = reorder(Pais, desc(Pais)), yend = reorder(Pais, desc(Pais))),
    size = 2, alpha = .2
  ) +
  geom_point(
    data = confia_otr,
    aes(valor, reorder(Pais, desc(Pais)), group = Institucion, fill = Institucion), 
    size = 4, alpha = .9, shape = 21
  ) +
  labs(
    x = "% que confía mucho o algo", y = NULL,
    title = "Confianza en insituciones en 2020 según país",
    caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de Latinobarómetro'
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = 12)) +
  scale_fill_brewer(name = "", palette = "Dark2")


# Series
confianza <- readxl::read_excel("data/latino/confianzas_serie.xlsx") %>% 
  mutate(confia = `Mucha confianza` + `Algo de confianza`) %>%
  mutate(year = as.numeric(year))

confianza <- confianza%>% 
  pivot_longer(cols = `Mucha confianza`:confia,
               names_to = "cat",
               values_to = "value") %>% 
  mutate(value = round(value * 100, digits = 0))
  
# Confianza - Instituciones Políticas
confia_pol <- confianza %>% 
  filter(cat == "confia") %>% 
  filter(Institucion %in% c("Congreso", "Gobierno", "Presidente", "Partidos Políticos")) %>% 
  select(-cat)

# Tabla
confia_tabla <- confia_pol %>% 
  pivot_wider(names_from = Institucion,
              values_from = value)

ggplot(confia_pol,
       aes(x = year, y = value, 
           color = Institucion, fill = Institucion)) +
  geom_line(size=1.25) +
  geom_point(size=3, shape=21, fill="white") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_brewer(name = "", palette = "Paired") +
  labs(
    y = "% que confía mucho o algo", x = NULL,
    title = "Confianza en insituciones políticas en Uruguay",
    caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de Latinobarómetro'
  )  


# Confianza - Otras Instituciones 
confia_pol <- confianza %>% 
  filter(cat == "confia") %>% 
  filter(!(Institucion %in% c("Congreso", "Gobierno", "Presidente", "Partidos Políticos"))) %>% 
  select(-cat)

# Tabla
confia_tabla <- confia_pol %>% 
  pivot_wider(names_from = Institucion,
              values_from = value)

ggplot(confia_pol,
       aes(x = year, y = value, 
           color = Institucion, fill = Institucion)) +
  geom_line(size=1.25) +
  geom_point(size=3, shape=21, fill="white") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_brewer(name = "", palette = "Paired") +
  labs(
    y = "% que confía mucho o algo", x = NULL,
    title = "Confianza en insituciones en Uruguay",
    caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de Latinobarómetro'
  )  


## Democracia
demo <- readxl::read_excel("data/latino/tabla_demo.xlsx") %>% 
  select(-satisfecho) %>% 
  arrange(desc(preferible))

(tab <- gt(demo) %>% 
    cols_label(Pais = "País",
               preferible = "% que cree que la democracia es preferible a cualquier otra forma de gobierno",
               churchil = "% de acuerdo o muy de acuerdo con \"La democracia puede tener problemas pero es el mejor sistema de gobierno\"",
               satisfecho = "% satisfecho o muy satisfecho con el funcionamiento de la democracia en su país") %>% 
    tab_header(title = md("Actitudes hacia la democracia en América Latina en 2020")) %>% 
    tab_source_note(source_note = "Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de Latinobarómetro"))

(tab <- tab %>% 
    #Apply new style to all column headers
    tab_style(
      locations = cells_column_labels(columns = everything()),
      style     = list(
        #Give a thick border below
        cell_borders(sides = "bottom", weight = px(3)),
        #Make text bold
        cell_text(weight = "bold")
      )
    ) %>% 
    #Apply different style to the title
    tab_style(
      locations = cells_title(groups = "title"),
      style     = list(
        cell_text(weight = "bold", size = 24)
      )
    ) %>% 
    cols_align(
      align = c("center"),
    ) %>% 
    cols_align(
      align = c("left"),
      columns = Pais
    ) %>% 
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts())
    )) 

# Con bar
(tab_2 <- tab %>%
    gt_plt_bar(column = preferible, scaled = TRUE, 
               color = "#2c3e50", scale_type = "number") %>% 
    gt_plt_bar(column = churchil, scaled = TRUE, 
               color = "#2c3e50", scale_type = "number") %>% 
    gt_highlight_rows(rows = 1, fill = "lightblue",
                      bold_target_only = TRUE, target_col = Pais) %>% 
    gt_highlight_rows(rows = 9, fill = "lightgrey",
                      bold_target_only = TRUE, target_col = Pais) %>% 
    cols_width(
      Pais ~ px(150),
      everything() ~ px(300)
    )
  )


## Principal problema del país ----

## * Todos los paises ----
# Cargar tabla con datos y cambiar formato a largo
prob <- readxl::read_excel(
  "data/latino/prob_pais.xlsx",
  col_types = c(
    "text", "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric")) 

glimpse(prob)

problemas <- c("Desocupación/desempleo",
               "La economía/problemas económicos/financieros",
               "Delincuencia/seguridad pública",
               "Corrupción",
               "Situación política/problemas de la política",
               "La Pandemia/ Coronavirus/ Covid-19",
               "Problemas de la salud")

prob_rec <- prob %>% 
  select(Categoría, problemas) %>% 
  pivot_longer(-Categoría, 
               names_to = "cat",
               values_to = "valor") %>% 
  mutate(cat_rec = case_when(
    cat == "La Pandemia/ Coronavirus/ Covid-19" ~ "Covid-19/Salud",
    cat == "Problemas de la salud" ~ "Covid-19/Salud",
    cat == "La economía/problemas económicos/financieros" ~ "Economía/Desocupación",
    cat == "Desocupación/desempleo" ~ "Economía/Desocupación",
    TRUE ~ cat
  )) %>% 
  rename(Pais = Categoría) %>% 
  select(Pais, cat_rec, valor) %>% 
  group_by(Pais, cat_rec) %>% 
  summarise(valor = sum(valor)) 


prob_tabla <- prob_rec %>%
  mutate(valor = round(valor, digits = 0)) %>% 
  pivot_wider(names_from = "cat_rec",
              values_from = "valor")

prob_rec <- prob_tabla %>%
  pivot_longer(-Pais, 
               names_to = "cat_rec",
               values_to = "valor") 


ggplot(prob_rec, aes(y = valor, x = tidytext::reorder_within(cat_rec, valor, Pais), fill = cat_rec)) +
  geom_col(color = "black", alpha = .8) +
  facet_wrap(~ Pais, scales = "free_y", ncol = 4) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 11, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_viridis_d(name = "") +
  coord_flip() +
  ylim(0, 50) +
  labs(x = "",
       y = "",
       title = "Principal problema del país en 2020, según país")


## * Comparación -----
prob_filtro <- c("Covid-19/Salud", 
                 "Economía/Desocupación",
                 "Situación política/problemas de la política",
                 "Corrupción",
                 "Delincuencia/seguridad pública",
                 "")

prob_comp <- readxl::read_excel("data/latino/prob_18_20.xlsx") %>% 
  pivot_longer(Total:Uruguay,
               values_to = "valor",
               names_to = "pais") %>% 
  mutate(cat_rec = case_when(
    Categoría == "La Pandemia/ Coronavirus/ Covid-19" ~ "Covid-19/Salud",
    Categoría == "Problemas de la salud" ~ "Covid-19/Salud",
    Categoría == "La economía/problemas económicos/financieros" ~ "Economía/Desocupación",
    Categoría == "Desocupación/desempleo" ~ "Economía/Desocupación",
    TRUE ~ Categoría
  )) %>% 
  select(pais, fecha, cat_rec, valor) %>% 
  group_by(pais, fecha, cat_rec) %>% 
  summarise(valor = sum(valor)) %>% 
  filter(cat_rec %in% prob_filtro) %>% 
  mutate(pais = case_when(
    pais == "Total" ~ "Total América Latina",
    TRUE ~ pais
  ))

ggplot(prob_comp,
       aes(x = fecha, y = valor, color = cat_rec)) +
  geom_point(size = 4,  stroke = 1.5) +
  geom_line(size = 1) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold")) +
  facet_wrap(~ pais) +
  scale_color_viridis_d(name = "") +
  labs(x = "",
       y = "",
       title = "Principal problema del país 2018 y 2020") +
  scale_x_continuous(limits = c(2018, 2020), 
                     breaks = seq(2018, 2020, by = 2))

ggsave("www/ppal_prob_uytot.png", units = "cm", width = 35, height = 25)


## * Serie ----
problemas <- c("Desocupación/desempleo",
               "La economía/problemas económicos/financieros",
               "Delincuencia/seguridad pública",
               "Corrupción",
               "Situación política/problemas de la política",
               "La Pandemia/ Coronavirus/ Covid-19",
               "Problemas de la salud",
               "Inflación/aumento de precios")

prob_serie <- readxl::read_excel("data/latino/prob_uru_serie.xlsx") %>% 
  mutate(`2020` = as.numeric(`2020`)) %>% 
  filter(Categoría %in%  problemas) %>%
  pivot_longer(-Categoría, 
               names_to = "fecha",
               values_to = "valor") %>% 
  mutate(cat_rec = case_when(
    Categoría == "La Pandemia/ Coronavirus/ Covid-19" ~ "Covid-19/Salud",
    Categoría == "Problemas de la salud" ~ "Covid-19/Salud",
    Categoría == "La economía/problemas económicos/financieros" ~ "Economía/Desocupación",
    Categoría == "Desocupación/desempleo" ~ "Economía/Desocupación",
    Categoría == "Inflación/aumento de precios" ~ "Economía/Desocupación/Inflación",
    TRUE ~ Categoría
  )) %>% 
  select(fecha, cat_rec, valor) %>% 
  group_by(fecha, cat_rec) %>% 
  summarise(valor = sum(valor)) %>% 
  filter(cat_rec %in% prob_filtro) %>% 
  mutate(fecha = as.numeric(fecha))

prob_tabla <- prob_serie %>% 
  pivot_wider(names_from = "cat_rec",
              values_from = "valor")

prob_serie <- prob_tabla %>% 
  pivot_longer(-fecha,
               names_to = "cat_rec",
               values_to = "valor")

ggplot(prob_serie,
       aes(x = fecha, y = valor, color = cat_rec, fill = cat_rec)) +
  geom_line(size = 1.25) +
  geom_point(size = 3, shape = 21, fill = "white") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.size = unit(2,"line")) +
  scale_color_viridis_d(name = "") +
  labs(
    y = "% por problema", x = NULL,
    title = "Principal problema en Uruguay (2004-2020)",
    caption = 'Fuente: Unidad de Métodos y Acceso a Datos (FCS-UdelaR) en base a datos de Latinobarómetro'
  )  






  
