
## **************************************************************************
## Piso I - Opinión Pública
## Código local para documento Markdown
## 10/2021 UMAD
## **************************************************************************

library(tidyverse)
library(opuy)

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
