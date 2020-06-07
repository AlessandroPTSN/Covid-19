library(plotly)
library(dplyr)
library(shiny)
library(tidyr)
library(shinythemes)
library(readxl)
library(httr)
library(stringr)
library(DT)
library(xlsx)
library(readxl)
library(openxlsx)
###############################################################
template <- tempfile(fileext = ".xlsx")
url <- httr::GET("https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalGeral",
                 httr::add_headers("X-Parse-Application-Id" =
                                     "unAFkcaNDeXajurGB7LChj8SgQYS2ptm")) %>%
  httr::content() %>%
  '[['("results") %>%
  '[['(1) %>%
  '[['("arquivo") %>%
  '[['("url")




httr::GET(url,
          write_disk(template) )
my_data = openxlsx::read.xlsx(template,1)
my_data$data2 = convertToDate(c(my_data$data))



cidade <- my_data %>%
  select(municipio,data2,populacaoTCU2019,casosAcumulado,obitosAcumulado,estado) %>%
  group_by(estado,municipio,data2) %>%
ungroup()
my_data=0
cidade = cidade %>% drop_na()


cidade$countriesAndTerritories = str_c(cidade$municipio," ","(",cidade$estado,")")
cidade$dateRep = cidade$data2
cidade$cases = as.integer(cidade$casosAcumulado)
cidade$deaths = as.integer(cidade$obitosAcumulado)
cidade$Data = cidade$data2
cidade$Quantidade = as.integer(cidade$casosAcumulado)

cidade = cidade %>% drop_na()

j=aggregate(cidade$cases, by=list(countriesAndTerritories=cidade$countriesAndTerritories), FUN=max)
k=aggregate(cidade$deaths, by=list(countriesAndTerritories=cidade$countriesAndTerritories), FUN=max)


cidade2=cidade

cidade=left_join(cidade,j,by="countriesAndTerritories")
cidade2=left_join(cidade2,k,by="countriesAndTerritories")
j=0;k=0
tabela2=cidade
tabela2$y=cidade2$x
tabela2$Local = tabela2$municipio

tabela2 <- tabela2 %>%
  select(Local,x,y) %>%
  group_by(Local) %>%
  summarise('Total Casos' = max(x),'Total Mortes' = max(y))


###############################################################



df1 <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
df1$Data = as.Date(df1$dateRep,"%d/%m/%Y")
df1=df1 %>%  filter(countriesAndTerritories!="Cases_on_an_international_conveyance_Japan")
df1$countriesAndTerritories = str_replace_all(df1$countriesAndTerritories,"_"," ")
df1$Quantidade = df1$cases
df2=df1

h=aggregate(df1$cases, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=sum)
hh=aggregate(df1$popData2018, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=mean)
names(hh)[names(hh) == "x"] <- "o"
i=aggregate(df1$deaths, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=sum)
ii=aggregate(df1$popData2018, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=mean)
names(ii)[names(ii) == "x"] <- "o"



df1=left_join(df1,h,by="countriesAndTerritories")
df2=left_join(df2,i,by="countriesAndTerritories")




mini1=data.frame(countriesAndTerritories=h$countriesAndTerritories,incidencia=round((h$x*100000)/hh$o,1))
mini2=data.frame(countriesAndTerritories=h$countriesAndTerritories,mortalidade=round((i$x*100000)/ii$o,1))
h=0;i=0
hh=0;ii=0

df1=left_join(df1,mini1,by="countriesAndTerritories")
df2=left_join(df2,mini2,by="countriesAndTerritories")
mini1=0
mini2=0


df1$Quantidade = round((df1$Quantidade*100000)/df1$popData2018,1)
df2$deaths = round((df2$deaths*100000)/df2$popData2018,1)




df11=df1
df22=df1



tabela = df1[,-c(1,2,3,4,8,9,14,15)]
tabela$Mortalidade = df2$deaths
tabela$Locais = tabela$countriesAndTerritories


tabela1 <- tabela %>%
  select(Locais,cases,deaths,Quantidade,Mortalidade) %>%
  group_by(Locais) %>%
  summarise('Total Incidência' = sum(Quantidade),'Total Casos' = sum(cases),'Total Mortalidade' = sum(Mortalidade),'Total Mortes' = sum(deaths))
tabela=0

df11$Quantidade = df11$cases



df111=df1
df222=df1


df111=  df111 %>%
  select(countriesAndTerritories,cases,deaths,Data) %>%
  group_by(countriesAndTerritories) %>%
  mutate(cases = rev(cumsum(rev(cases))),deaths = rev(cumsum(rev(deaths))),Data=Data)
  
df222=  df222 %>%
    select(countriesAndTerritories,cases,deaths,Data) %>%
    group_by(countriesAndTerritories) %>%
    mutate(cases = rev(cumsum(rev(cases))),deaths = rev(cumsum(rev(deaths))),Data=Data)

df111$Quantidade=df111$cases

df111=ungroup(df111)
df222=ungroup(df222)
