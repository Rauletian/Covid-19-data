install.packages(
  c("tidyverse", "lubridate", "janitor", "readxl", "scales", "RColorBrewer")
)
library(tidyverse)
library(readxl)
library(utils)
hoy <- format(Sys.time(), "%Y-%m-%d")
url_covid <- 
  paste0(
    "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", 
    hoy,  ".xlsx"
  )

archivo_covid <- paste0(hoy, "_covid19.xlsx")
download.file(
  url = url_covid, 
  destfile = archivo_covid, 
  mode = "wb"
)
read_excel(archivo_covid,col_names=TRUE)->data_covid

##DATOS
dAtos_euro<-data_covid%>% ## Datos paises europa, pob>30 millones
  filter(month==5, continentExp=="Europe", popData2018>30*(10^6))%>%
  select(day:deaths,popData2018, countriesAndTerritories)%>%
  mutate( mill_habit=cases/(popData2018/10^6))

data_country<-data_covid%>%  #datos por país con acumulados
  group_by(countriesAndTerritories)%>%
  arrange((dateRep))%>%
  select(dateRep:deaths)%>%
  mutate(casos_acumulado=cumsum(cases),
         muertes_acumulado=cumsum(deaths))%>%
  mutate(crec.casos =((casos_acumulado/lag(casos_acumulado)-1)*100), 
                      crec.muertes=(((muertes_acumulado/lag(muertes_acumulado)-1)*100 )))
data_spain<-filter(data_country, countriesAndTerritories=="Spain", month>=4, casos_acumulado>1)

data_africa<-data_covid %>%
  filter(continentExp=="Africa")%>%
  select(dateRep:deaths,popData2018, countriesAndTerritories)%>%
    mutate( mill_habit=cases/(popData2018/10^6))%>%
    mutate(casos_acumulado=cumsum(cases),
           muertes_acumulado=cumsum(deaths),
    crec.casos =((casos_acumulado/lag(casos_acumulado)-1)*100), 
           crec.muertes=(((muertes_acumulado/lag(muertes_acumulado)-1)*100 )))%>%
    filter(casos_acumulado>1)

##Gráficos

ggplot(datos_euro, mapping=aes(x= day, y= mill_habit, colour=countriesAndTerritories))+
  geom_point()+
  geom_smooth() +
  facet_wrap(~countriesAndTerritories, 2)+
  xlab("Dia")+
  ylab("Casos por millon de habitantes")

ggplot(data_country, mapping=aes(x=day, y=deaths, colour=countriesAndTerritories)) +
  geom_smooth(se=F) +
  ylab("Muertos")+
  xlab("dia")
  

ggplot(data_spain)  +
  geom_line(mapping=aes(x=dateRep, y=casos_acumulado), colour="blue") +
  geom_line(mapping=aes(x=dateRep, y= muertes_acumulado), colour="black")+ 
  geom_col(mapping=aes(x=dateRep, y=cases), fill="red")+
  xlab("Fecha")+
  ylab("Número")+
  theme_minimal()+
  ggtitle ("Evolución Covid-19 España")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1), #Tamaño relativo de la letra del título
                                   vjust=2, #Justificación vertical, para separarlo del gráfico
                                   hjust = 0.5,
                                   face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                   color="black", #Color del texto
                                   lineheight=1.5))

ggplot(data_spain)   +
  geom_line(mapping = aes(x=dateRep, y=crec.casos, color= "crecimiento casos"), colour="blue") +
  geom_line(mapping=aes(x=dateRep, y=crec.muertes, color="crecimiento muertes"), colour="black")+
  xlab("Fecha")+
  ylab("Porcentaje de crecimiento")+
  theme_minimal()+
  ggtitle ("Crecimiento porcentual de los casos y muertes Covid-19 en Espana")+
  theme (plot.title = element_text(
                                  size=rel(0.7), #Tamaño relativo de la letra del título
                                   vjust=2, #Justificación vertical, para separarlo del gráfico
                                   hjust = 0.5,
                                   face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                   color="black", #Color del texto
                                   lineheight=1.5) )

  

  

  
  
  

