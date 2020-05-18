library(plotly)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(tidyr)
library(shinythemes)
library(viridis)
library(readxl)
library(httr)
library(stringr)


#Datasets

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

my_data <- read_excel(template)
my_data$data2 = as.Date(my_data$data,"%Y-%m-%d")




cidade <- my_data %>%
  select(municipio,data2,populacaoTCU2019,casosAcumulado,obitosAcumulado,estado) %>%
  group_by(estado,municipio,data2) %>%
  ungroup()

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

cidade=merge(cidade,j)
cidade2=merge(cidade2,k)

###############################################################



df1 <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
df1$Data = as.Date(df1$dateRep,"%d/%m/%Y")
df1$Quantidade = df1$cases
df2=df1

h=aggregate(df1$cases, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=sum)
hh=aggregate(df1$popData2018, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=mean)
names(hh)[names(hh) == "x"] <- "o"
i=aggregate(df1$deaths, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=sum)
ii=aggregate(df1$popData2018, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=mean)
names(ii)[names(ii) == "x"] <- "o"

df1=merge(df1,h)
df2=merge(df2,i)



mini1=data.frame(countriesAndTerritories=h$countriesAndTerritories,incidencia=round((h$x*100000)/hh$o,1))
mini2=data.frame(countriesAndTerritories=h$countriesAndTerritories,mortalidade=round((i$x*100000)/ii$o,1))


df1=merge(df1,mini1)
df2=merge(df2,mini2)
