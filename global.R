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
library(zoo)
###############################################################


#######################################################################
### teste para verificar se o arquivo em disco tem mais de 24 horas ###
#######################################################################

# data de criacao dos dados salvos em disco

dataCriacao <- file.info("dados.rda")

# calcula a idade do aruivo em segundos

idade <- difftime(Sys.time(), dataCriacao$mtime, units = "secs")

idade <- as.numeric(gsub("([0-9]+).*$", "\\1", idade))

# condicional: se a idade do arquivo for maior que 86400 segundos, 
# entao baixa o app baixa os dados novamente

if (idade > 86400) {
  source("baixarDados.R")
}

# carrega os dados em disco, atualizados no passo anterior ou nao

load("dados.rda")


# 

my_data$municipio = my_data$city
my_data$estado = my_data$state
my_data$data2 = as.Date(my_data$date)
my_data$casosAcumulado = my_data$last_available_confirmed
my_data$obitosAcumulado = my_data$last_available_deaths

cidade <- my_data %>%
  select(municipio,data2,casosAcumulado,obitosAcumulado,estado) %>%
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

########################################
#criando incidencia e mortalidade
h=aggregate(df1$cases, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=sum)
hh=aggregate(df1[,10], by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=mean)
names(hh)[names(hh) == "x"] <- "o"
i=aggregate(df1$deaths, by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=sum)
ii=aggregate(df1[,10], by=list(countriesAndTerritories=df1$countriesAndTerritories), FUN=mean)
names(ii)[names(ii) == "x"] <- "o"


########################################
#criando incidencia e mortalidade soma total
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

########################################
#criando incidencia e mortalidade diaria
df1$Quantidade = round((df1$Quantidade*100000)/df1[,10],1)
df2$deaths = round((df2$deaths*100000)/df1[,10],1)




df11=df1
df22=df1




#####################################################################################
#Tabela Mundo

tabela = df1[,-c(1,2,3,4,8,9,14,15)]
tabela$Mortalidade = df2$deaths
tabela$Locais = tabela$countriesAndTerritories


tabela1 <- tabela %>%
  select(Locais,cases,deaths,Quantidade,Mortalidade) %>%
  group_by(Locais) %>%
  summarise('Total Incidência' = sum(Quantidade),'Total Casos' = sum(cases),'Total Mortalidade' = sum(Mortalidade),'Total Mortes' = sum(deaths))
tabela=0

df11$Quantidade = df11$cases



#################################################################################

#Acumulado
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

##############################################################################################
#removendo 6 primeiros dias para fazer media movel de 7 dias
hplm=aggregate(Data ~ countriesAndTerritories, df22, function(x) order(unique(x)))
ffffddff=unlist(hplm$Data)


#Media Movel 
deaths2=rollmean(df22$deaths, 7, align = "right")
deaths3 = cbind(df22$deaths, c(deaths2,0,0,0,0,0,0))
df22$deaths2 = deaths3[,2]

QuantidadeMedia=rollmean(df11$Quantidade, 7, align = "right")
Quantidade3 = cbind(df11$Quantidade, c(QuantidadeMedia,0,0,0,0,0,0))
df11$QuantidadeMedia = Quantidade3[,2]


df11$order=ffffddff
df22$order=ffffddff


df11=df11[df11$order!=1,];df22=df22[df22$order!=1,]
df11=df11[df11$order!=2,];df22=df22[df22$order!=2,]
df11=df11[df11$order!=3,];df22=df22[df22$order!=3,]
df11=df11[df11$order!=4,];df22=df22[df22$order!=4,]
df11=df11[df11$order!=5,];df22=df22[df22$order!=5,]
df11=df11[df11$order!=6,];df22=df22[df22$order!=6,]


#Media Movel 2
deaths22=rollmean(df2$deaths, 7, align = "right")
deaths33 = cbind(df2$deaths, c(deaths22,0,0,0,0,0,0))
df2$deaths2 = deaths33[,2]

QuantidadeMedia2=rollmean(df1$Quantidade, 7, align = "right")
Quantidade33 = cbind(df1$Quantidade, c(QuantidadeMedia2,0,0,0,0,0,0))
df1$QuantidadeMedia = Quantidade33[,2]


df1$order=ffffddff
df2$order=ffffddff


df1=df1[df1$order!=1,];df2=df2[df2$order!=1,]
df1=df1[df1$order!=2,];df2=df2[df2$order!=2,]
df1=df1[df1$order!=3,];df2=df2[df2$order!=3,]
df1=df1[df1$order!=4,];df2=df2[df2$order!=4,]
df1=df1[df1$order!=5,];df2=df2[df2$order!=5,]
df1=df1[df1$order!=6,];df2=df2[df2$order!=6,]
