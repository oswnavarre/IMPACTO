library(agricolae)
library(tidyverse)
library(writexl)
library(readxl)
library(jtools)
library(plotly)

orange_earth <- "#f37321"

green_earth <- "#008C99"

datos <- read_excel("datos/data_final.xlsx")

gender <- datos |>
  group_by(Gender) |> 
  summarise(
    N = n(), 
    Porcentaje = round(100*N/nrow(datos),2)
  )

gender_country <- datos |>
  group_by(Gender, Origin_Country) |> 
  summarise(
    N = n(), 
    Porcentaje = round(100*N/nrow(datos),2)
  ) |>
  ungroup()

g01 <- gender_country |>
  mutate(Origin_Country = fct_reorder(Origin_Country, Porcentaje)) |>
  ggplot(aes(fill=Gender, y=Porcentaje, x=Origin_Country)) + 
  geom_bar(position="stack", stat="identity" ) +
  labs(x = "País de Origen", y = "Porcentaje") +
  scale_fill_discrete(name = "Género") +
  coord_flip() +
  theme_apa() +
  theme(
    legend.position = c(0.80,0.10)
  ) 
g01
ggsave("graficos/country_origin_gdr.png",g01,width = 8, height = 8)

country <- datos |>
  group_by(Origin_Country) |> 
  summarise(
    N = n(), 
    Porcentaje = round(100*N/nrow(datos),2)
  )

g02 <- country |>
  mutate(Origin_Country = fct_reorder(Origin_Country, N)) |>
  ggplot( aes(x = Origin_Country, y = Porcentaje)) +
  geom_bar(stat = "identity", fill = orange_earth) +
  coord_flip() + 
  theme_apa() +
  geom_text(aes(label = round(Porcentaje,2)), size = 3.5, hjust = -0.5, vjust = 0.5, position = "stack") +
  labs(
    x = "País", y = "Frecuencia"
  )
g02
ggsave("graficos/country_origin.png",g02,width = 8, height = 8)

cohort <- datos |>
  group_by(Prom) |> 
  summarise(
    N = n(), 
    Porcentaje = round(100*N/nrow(datos),2)
  )

cohort_gender <- datos |>
  group_by(Prom, Gender) |> 
  summarise(
    N = n(), 
    Porcentaje = round(100*N/nrow(datos),2)
  )

datos <- datos |>
  mutate(
    StudyLevel2 = fct_recode(StudyLevel, "Doctorado" = "Doctorado",
                             "Licenciatura" = "Licenciatura",
                             "Maestría" = "Maestría",
                             "Otro" = "Otro (especificar): - cursando Doctorado",
                             "Otro" = "Otro (especificar): - Cannabis Industry Specialization",
                             "Otro" = "Otro (especificar): - Postgraduado en Manejo de Recursos Naturales",
                             "Otro" = "Otro (especificar): - Especialidad Desarrollo Local",
                             "Otro" = "Otro (especificar): - Diplomado",
                             "Otro" = "Otro (especificar): - Especialización"),
    StudyLevel2 = fct_relevel(StudyLevel2, "Licenciatura","Maestría", "Doctorado","Otro")
  ) 



studylvl <- datos |>
  group_by(StudyLevel2) |> 
  summarise(
    N = n(), 
    Porcentaje = round(100*N/nrow(datos),2)
  )

g03 <-  studylvl |>
  mutate(StudyLevel2 = fct_rev(StudyLevel2)) |>
  ggplot(aes(x = StudyLevel2, y = N)) +
  geom_bar(stat = "identity",fill = green_earth) +
  labs(x = "Nivel de Estudios", y = "Frecuencia") +
  theme_apa() +
  coord_flip()
g03
ggsave("graficos/studylvl.png", g03,width = 4, height = 4)

studylvl_gdr <- datos |>
  group_by(StudyLevel2, Gender) |> 
  summarise(
    N = n(), 
    Porcentaje = round(100*N/nrow(datos),2)
  ) |>
  ungroup()

g04 <- studylvl_gdr |>
  mutate(StudyLevel2 = fct_rev(StudyLevel2)) |>
  ggplot( aes(x = StudyLevel2, y = N, fill = Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  theme_apa() +
  labs(x = "Nivel de Estudios", y = "Frecuencia") +
  theme(legend.position = c(0.75, 0.15))
g04
ggsave("graficos/studylvl_gdr.png",g04, width = 4, height = 4)

work <- datos |>
  group_by(Work) |> 
  summarise(
    N = n(), 
    Porcentaje = round(100*N/nrow(datos),2)
  )

work_gdr <- datos |>
  group_by(Work, Gender) |> 
  summarise(
    N = n(), 
    Porcentaje = round(100*N/nrow(datos),2)
  )

g05 <- ggplot(work_gdr, aes(x= Work , fill = Gender, y = Porcentaje) ) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_apa() + 
  labs(x = "Trabaja", y = "Porcentaje (%)") +
  theme(legend.position = "bottom")
g05

datos <- datos |>
  mutate(
    Principal_Activity = str_remove(Principal_Activity, " \\(incluir trabajo independiente\\)")
  )

prin_acti <- datos |>
  group_by(Principal_Activity) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(datos),2)
  )

g06 <- prin_acti |>
  mutate(Principal_Activity = fct_reorder(Principal_Activity, N)) |>
  ggplot(aes(x= Principal_Activity , fill = Principal_Activity, y = Porcentaje) ) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "none") +
  coord_flip()

g06

work_prin_acti <- datos |>
  group_by(Work, Principal_Activity) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(datos),2)
  )

g07 <- work_prin_acti |>
  mutate(Principal_Activity = fct_reorder(Principal_Activity, N)) |>
  ggplot(aes(x= Principal_Activity , fill = Work, y = Porcentaje) ) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "bottom") +
  coord_flip()

g07

studyl_prin_acti <- datos |>
  group_by(StudyLevel2, Principal_Activity) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(datos),2)
  )

g08 <- studyl_prin_acti |>
  mutate(Principal_Activity = fct_reorder(Principal_Activity, N)) |>
  ggplot(aes(x= Principal_Activity , fill = StudyLevel2, y = Porcentaje) ) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "bottom") +
  coord_flip()
g08

current_work <- datos |>
  select(c(1,142,17:22)) |>
  drop_na()

current_work_long <- current_work |>
  pivot_longer(
    3:8,
    names_to = "Variable",
    values_to = "Score"
  ) |>
  mutate(
    Score = as_factor(Score),
    Variable = case_when(Variable == "OpportunitiesWork" ~ "Oportunidades",
                         Variable == "SatisfiedWork" ~ "Satisfacción",
                         Variable == "SignificativeWork" ~ "Significativo",
                         Variable == "SocietyWork" ~ "Sociedad",
                         Variable == "SufficientPayment" ~ "Pago",
                         Variable == "CoincidentWork" ~ "Coincidente"),
    Variable = fct_relevel(Variable, "Significativo", "Coincidente" ,"Sociedad", 
                           "Satisfacción", "Oportunidades", "Pago")
  ) 
  

stats_current_work_long <- current_work_long |>
  group_by(Variable, Score) |>
  summarise(
    N = n()
  ) |>
  mutate(
    Porcentaje = round(100*N/sum(N),2)
  )

g09 <- stats_current_work_long |>
  mutate(Variable = fct_rev(Variable)) |>
  ggplot(aes(x = Variable, y = Porcentaje, fill = Score))  +
  geom_bar(position = "stack", stat = "identity") + 
  coord_flip() +
  theme_apa()
g09
ggplotly(g09)

stats_current_work_long_gdr <- current_work_long |>
  group_by(Variable, Gender, Score) |>
  summarise(
    N = n()
  ) |>
  mutate(
    Porcentaje = round(100*N/sum(N),2)
  )

g010 <- stats_current_work_long_gdr |>
  mutate(Variable = fct_rev(Variable)) |>
  ggplot(aes(x = Variable, y = Porcentaje, fill = Score))  +
  geom_bar(position = "stack", stat = "identity") + 
  coord_flip() +
  facet_wrap(. ~ Gender) +
  theme_apa()
g010
ggplotly(g010)

stats_current_work_long_stlev <- current_work_long |>
  group_by(Variable, StudyLevel2, Score) |>
  summarise(
    N = n()
  ) |>
  mutate(
    Porcentaje = round(100*N/sum(N),2)
  )

g011 <- stats_current_work_long_stlev |>
  mutate(Variable = fct_rev(Variable)) |>
  ggplot(aes(x = Variable, y = Porcentaje, fill = Score))  +
  geom_bar(position = "stack", stat = "identity") + 
  coord_flip() +
  facet_grid(. ~ StudyLevel2) +
  theme_apa()
g011
ggplotly(g011)

first_work <- datos |>
  select(c(1,142,23:27)) |>
  mutate(
    FirstActivity = str_remove(FirstActivity, " \\(incluir trabajo independiente\\)")
  ) |>
  mutate(
    FirstActivity = if_else(FirstActivity %in% c("Estudios de posgrado",
                                                 "Trabajando de tiempo completo",
                                                 "Pasantía",
                                                 "Trabajando a tiempo parcial",
                                                 NA),
                            FirstActivity,
                            "Otro")
  )

unique(first_work$FirstActivity)

firstact <- first_work |>
  select(Gender, StudyLevel2, FirstActivity) |>
  drop_na()

firstact_all <- firstact |>
  group_by(FirstActivity) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(firstact),2)
  )

g012 <-  firstact_all |>
  mutate(FirstActivity = fct_reorder(FirstActivity, N)) |>
  ggplot(aes(x= FirstActivity ,y = Porcentaje, fill = FirstActivity) ) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "none") +
  coord_flip()

g012

firstact_gdr <- firstact |>
  group_by(Gender, FirstActivity) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(firstact),2)
  )


g013 <-  firstact_gdr |>
  mutate(FirstActivity = fct_reorder(FirstActivity, N)) |>
  ggplot(aes(x= FirstActivity ,y = Porcentaje, fill = Gender) ) +
  geom_bar(stat = "identity", position = "stack") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "bottom") +
  coord_flip() 

g013

firstact_lvl <- firstact |>
  group_by(StudyLevel2, FirstActivity) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(firstact),2)
  )


g014 <-  firstact_lvl |>
  mutate(FirstActivity = fct_reorder(FirstActivity, N)) |>
  ggplot(aes(x= FirstActivity ,y = Porcentaje, fill = StudyLevel2) ) +
  geom_bar(stat = "identity", position = "stack") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "bottom") +
  coord_flip() 

g014



firstcoincident <- first_work |>
  select(Gender, StudyLevel2, CoincidentFirst) |>
  mutate(CoincidentFirst = fct_relevel(CoincidentFirst, "Nula coincidencia",
                                       "Baja coincidencia", "Mediana coincidencia",
                                       "Total coincidencia")) |>
  drop_na()

firstcoincident_all <- firstcoincident |>
  group_by(CoincidentFirst) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(firstcoincident),2)
  )

g015 <-  firstcoincident_all |>
  mutate(CoincidentFirst = fct_reorder(CoincidentFirst, N)) |>
  ggplot(aes(x= CoincidentFirst ,y = Porcentaje, fill = CoincidentFirst) ) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "none") +
  coord_flip()

g015

firstcoincident_gdr <- firstcoincident|>
  group_by(Gender, CoincidentFirst) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(firstcoincident),2)
  )


g016 <-  firstcoincident_gdr |>
  mutate(CoincidentFirst = fct_reorder(CoincidentFirst, N)) |>
  ggplot(aes(x= CoincidentFirst ,y = Porcentaje, fill = Gender) ) +
  geom_bar(stat = "identity", position = "stack") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "bottom") +
  coord_flip() 

g016

firstcoincident_lvl <- firstcoincident |>
  group_by(StudyLevel2, CoincidentFirst) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(firstcoincident),2)
  )


g017 <-  firstcoincident_lvl |>
  mutate(CoincidentFirst = fct_reorder(CoincidentFirst, N)) |>
  ggplot(aes(x= CoincidentFirst ,y = Porcentaje, fill = StudyLevel2) ) +
  geom_bar(stat = "identity", position = "stack") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "bottom") +
  coord_flip() 

g017


time_firstact <- first_work |>
  select(Gender, StudyLevel2, TimeFirstActivity) |>
  mutate(TimeFirstActivity = fct_relevel(TimeFirstActivity, "0-3 meses",
                                       "4-6 meses", "7-12 meses",
                                       "1-2 años", "Más de 2 años")) |>
  drop_na()

firsttime_all <- time_firstact |>
  group_by(TimeFirstActivity) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(time_firstact),2)
  )

g018 <-  firsttime_all |>
  mutate(TimeFirstActivity = fct_reorder(TimeFirstActivity, N)) |>
  ggplot(aes(x= TimeFirstActivity ,y = Porcentaje, fill = TimeFirstActivity) ) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "none") +
  coord_flip()

g018

firsttime_gdr <- time_firstact|>
  group_by(Gender, TimeFirstActivity) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(time_firstact),2)
  )


g019 <-  firsttime_gdr |>
  mutate(TimeFirstActivity = fct_reorder(TimeFirstActivity, N)) |>
  ggplot(aes(x= TimeFirstActivity ,y = Porcentaje, fill = Gender) ) +
  geom_bar(stat = "identity", position = "stack") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "bottom") +
  coord_flip() 

g019

firsttime_lvl <- time_firstact |>
  group_by(StudyLevel2, TimeFirstActivity) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(time_firstact),2)
  )


g020 <-  firsttime_lvl |>
  mutate(TimeFirstActivity = fct_rev(TimeFirstActivity)) |>
  ggplot(aes(x= TimeFirstActivity ,y = Porcentaje, fill = StudyLevel2) ) +
  geom_bar(stat = "identity", position = "stack") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "bottom") +
  coord_flip() 

g020


firstcountry <- first_work |>
  select(Gender, StudyLevel2, CountryFirst) |>
  drop_na()

firstcountry_all <- firstcountry |>
  group_by(CountryFirst) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(firstcountry),2)
  )

g021 <-  firstcountry_all |>
  mutate(CountryFirst = fct_reorder(CountryFirst, N)) |>
  ggplot(aes(x= CountryFirst ,y = Porcentaje, fill = CountryFirst) ) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "none") +
  coord_flip()

g021

countryfirst_gdr <- firstcountry |>
  group_by(Gender, CountryFirst) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(firstcountry),2)
  )


g022 <-  countryfirst_gdr |>
  mutate(CountryFirst = fct_reorder(CountryFirst, N)) |>
  ggplot(aes(x= CountryFirst ,y = Porcentaje, fill = Gender) ) +
  geom_bar(stat = "identity", position = "stack") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "bottom") +
  coord_flip() 

g022

countryfirst_lvl <- firstcountry |>
  group_by(StudyLevel2, CountryFirst) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(firstcountry),2)
  )


g023 <-  countryfirst_lvl |>
  ggplot(aes(x= CountryFirst ,y = Porcentaje, fill = StudyLevel2) ) +
  geom_bar(stat = "identity", position = "stack") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "bottom") +
  coord_flip() 

g023

firstcountrywhat <- first_work |>
  select(Gender, StudyLevel2, WhatCountryFirst) |>
  drop_na()

firstcountrywhat_all <- firstcountrywhat |>
  group_by(WhatCountryFirst) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(firstcountrywhat),2)
  )

g024 <-  firstcountrywhat_all |>
  mutate(WhatCountryFirst = fct_reorder(WhatCountryFirst, N)) |>
  ggplot(aes(x= WhatCountryFirst ,y = Porcentaje, fill = WhatCountryFirst) ) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "none") +
  coord_flip()

g024

whatcountryfirst_gdr <- firstcountrywhat |>
  group_by(Gender, WhatCountryFirst) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(firstcountrywhat),2)
  )


g025 <-  whatcountryfirst_gdr |>
  mutate(WhatCountryFirst = fct_reorder(WhatCountryFirst, firstcountrywhat$Porcentaje)) 

g025 <- ggplot(whatcountryfirst_gdr,aes(x= WhatCountryFirst ,y = Porcentaje, fill = Gender) ) +
  geom_bar(stat = "identity", position = "stack") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "bottom") +
  coord_flip() 

g025

countryfirst_lvl <- firstcountry |>
  group_by(StudyLevel2, CountryFirst) |>
  summarise(
    N = n(),
    Porcentaje = round(100*N/nrow(time_firstact),2)
  )


g023 <-  countryfirst_lvl |>
  ggplot(aes(x= CountryFirst ,y = Porcentaje, fill = StudyLevel2) ) +
  geom_bar(stat = "identity", position = "stack") +
  theme_apa() + 
  labs(x = "", y = "Porcentaje (%)") +
  theme(legend.position = "bottom") +
  coord_flip() 

g023
