##se cargan las librerias requeridas
require(pacman)
p_load(tidyverse, rvest, skimr, caret)

##listado de urls para web scraping
url <-  c("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9",
          "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10")

df <- data.frame()

#loop para scrapear las url y unir en un solo dataframe
for (i in url) {
  chunk_i <-read_html(i)
  df_i <- chunk_i %>%
    html_nodes(xpath = "/html/body/table") %>%
    html_table() %>%
    as.data.frame()
  
  df <-bind_rows(df,df_i)
}

#transformacion de la base en un tibble para facilitar la limpieza
GEIH <- as_tibble(df)
write_csv(GEIH, "G:/Mi unidad/00.uniandes/07.Big Data/Taller 1/Taller-1/stores/GEIH_2018")


#Ajuste de variables de control
GEIH <- GEIH %>% 
  rename(ID= Var.1) %>% #Identificador unico
  rename(estrato = estrato1) %>% #estrato socioeconomico
  mutate(mujer= (sex*-1)+1) %>%
  rename(urban = clase) %>% #dummy para sectores urbanos 
  mutate(jefe_hogar = ifelse(p6050 == 1,1,0)) %>% #dummy para jefe de hogar
  rename(nivel_educ = p6210) %>% #Nivel educativo alcanzado (categorico)
  rename(grado_esc = p6210s1) %>% #grado escolar aprobado (continuo)
  rename(max_educ_level = maxEducLevel) %>% # maximo nibel educativo alcanzado
  rename(hours_per_week1 = hoursWorkUsual) %>% #horas trabajadas a la semana en trabajo principal
  mutate(second_job = ifelse(p7040 == 1,1,0)) %>% #dummy para segundo trabajo
  rename(hours_per_week2 = hoursWorkActualSecondJob) %>% #horas trabajadas a la semana en segundo trabajo
  rename(hours_worked = totalHoursWorked) %>% #horas laboradas en total
  rename(pension = cotPension) %>% #cotizacion a pension (categorica)
  rename(salud = regSalud) %>% #seguridad social en salud (categorica)
  rename(cuenta_propia = cuentaPropia) %>% # trabajadores por cuenta propia (dummy)
  rename(PET = wap) %>% #Poblacion en edad de trabajar (dummy)
  rename(OCU = ocu) %>% #poblacion ocupada (dummy)
  rename(DES =dsi) %>%  #poblacion desempleada (dummy)
  rename(INAC =ina) %>% #poblacion inactiva (dummy)
  rename(PEA = pea) #poblacion economicamente activa (dummy)


##ajuste de valores de ingresos
GEIH <- GEIH %>%
  mutate(impa1 = if_else(is.na(impa), 0, impa)) %>%
  mutate(isa1 = if_else(is.na(isa), 0, isa)) %>%
  mutate(impa2 = if_else(is.na(impaes), 0, impaes)) %>%
  mutate(isa2 = if_else(is.na(isaes), 0, isaes)) %>%
  mutate(inglab = impa1 + isa1) %>%
  mutate(inglabes = impa2 + isa2) %>%
  mutate(inglab = if_else(inglab == 0, inglabes, inglab)) %>%
  mutate(ingtot =if_else(is.na(ingtot), 0, ingtot)) %>%
  mutate(ingnolab = ingtot - inglab) 

#seleccion de variables de interes
GEIH <- GEIH %>%
  select(ID, estrato, age, mujer, urban, depto, jefe_hogar, nivel_educ,
         grado_esc, max_educ_level, college, oficio, relab, second_job,
         hours_worked, pension, salud, cuenta_propia, informal, PET, OCU, DES,
         INAC, PEA, inglab, ingnolab, ingtot)

#restriccion de la muestra a PEA mayor de 18 años
GEIH <- GEIH %>%
  filter(age >= 18, PEA==1)

skim(GEIH)

# Obtener las estadísticas descriptivas y el número de observaciones para cada variable en GEIH
stats_table <- GEIH %>%
  summarise(across(-ID, list(
    media = mean,
    sd = sd,
    min = min,
    max = max
  ))) 
# Transformar la tabla de estadísticas descriptivas en el formato deseado
stats_table <- stats_table %>%
  pivot_longer(everything(), names_to = "Estadistica", values_to = "Valor") %>%
  separate(Estadistica, into = c("Variable", "Estadistica"), sep = "_") %>%
  pivot_wider(names_from = Estadistica, values_from = Valor) %>%
  select(Variable, media, sd, min, max)

# Imprimir la tabla de estadísticas descriptivas
print(stats_table)
