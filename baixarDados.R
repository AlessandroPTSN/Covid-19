# funcao que baixa os dados nacionais e internacionais
# caso os arquivos em disco tenham mais de 24 horas de idade

# dados nacionais

#my_data <- read.csv("https://brasil.io/dataset/covid19/caso_full/?format=csv", na.strings = "", fileEncoding = "UTF-8-BOM")
#my_data <- read.csv("https://raw.githubusercontent.com/AlessandroPTSN/Covid-19/master/my_data2.csv")
download_brasilio_table <- function(dataset, table_name){
  url <- sprintf("https://data.brasil.io/dataset/%s/%s.csv.gz", dataset, table_name)
  tmp <- tempfile()
  download.file(url, tmp)
  response <- read.csv(gzfile(tmp), encoding = "UTF-8")
  unlink(tmp)
  return(response)
}

# Passe o nome da tabela para a funcao, como "caso", "caso_full", "obito_cartorio":
my_data <- download_brasilio_table("covid19", "caso_full")
my_data = my_data %>%  filter(city!="")


# dados internacionais

df1 <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

# salva os objetos em um arquivo .rda, 
# cuja leitura eh mais rapida do que .csv

save(list = c("my_data", "df1"), file = "dados.rda")

