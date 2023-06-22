##se cargan las librerias requeridas
require(pacman)
p_load(tidyverse, rvest)

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
  mutate(inglab1 = ifelse(cclasnr2 == 1, impaes, impa)) %>%  
  mutate(inglab2 = ifelse(cclasnr3 == 1, isaes, isa)) %>% 
  mutate(ingnolab1= ifelse(cclasnr6 == 1, iof1es, iof1)) %>% 
  mutate(ingnolab2= ifelse(cclasnr7 == 1, iof2es, iof2)) %>% 
  mutate(iof3h = if_else(is.na(iof3h), 0, iof3h)) %>% 
  mutate(iof3hes = if_else(is.na(iof3hes), 0, iof3hes)) %>% 
  mutate(iof3i = if_else(is.na(iof3i), 0, iof3i)) %>% 
  mutate(iof3ies = if_else(is.na(iof3ies), 0, iof3ies)) %>% 
  mutate(ingnolab3= ifelse(cclasnr8 == 1, iof3hes + iof3ies, iof3h + iof3i)) %>% 
  mutate(ingnolab4 = ifelse(cclasnr11 ==1, iof6es, iof6)) %>% 
  mutate(inglab1 = if_else(is.na(inglab1), 0, inglab1)) %>% 
  mutate(inglab2 = if_else(is.na(inglab2), 0, inglab2)) %>% 
  mutate(ingnolab1 = if_else(is.na(ingnolab1), 0, ingnolab1)) %>% 
  mutate(ingnolab2 = if_else(is.na(ingnolab2), 0, ingnolab2)) %>% 
  mutate(ingnolab3 = if_else(is.na(ingnolab3), 0, ingnolab3)) %>%
  mutate(ingnolab4 = if_else(is.na(ingnolab4), 0, ingnolab4)) %>% 
  mutate(inglab = inglab1 + inglab2) %>% 
  mutate(ingnolab = ingnolab1 + ingnolab2 + ingnolab3 + ingnolab4) 

#seleccion de variables de interes
GEIH <- GEIH %>%
  select(ID, estrato, age, mujer, urban, depto, jefe_hogar, nivel_educ,
         grado_esc, max_educ_level, college, oficio, relab, second_job,
         hours_worked, pension, salud, cuenta_propia, informal, PET, OCU, DES,
         INAC, PEA, , depto, college, oficio, relab, informal, inglab,
         ingnolab, ingtot)

#restriccion de la muestra a PEA mayor de 18 años
GEIH <- GEIH %>%
  filter(age >= 18, PEA==1)

write_csv(GEIH, "G:/Mi unidad/00.uniandes/07.Big Data/Taller 1/Taller-1/stores/GEIH_2018_LIMPIA")



GEIH <- GEIH %>%
  mutate(impa1 = if_else(is.na(impa), 0, impa)) %>%
  mutate(isa1 = if_else(is.na(isa), 0, isa)) %>%
  mutate(impa2 = if_else(is.na(impaes), 0, impaes)) %>%
  mutate(isa2 = if_else(is.na(isaes), 0, isaes)) %>%
  mutate(inglab = impa1 + isa1) %>%
  mutate(inglabes = impa2 + isa2) %>%
  mutate(inglab = if_else(inglab == 0), inglabes, inglab)) %>%
  mutate(ingnolab = ingtot - inglab)


