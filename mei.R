rm(list=ls())


check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

pacotes <-c("dplyr", "openxlsx", "curl", "lubridate", "rio", "stringr", "zoo", "tidyr")

check.packages(pacotes)




ano <- year(Sys.Date()) %>% as.numeric()

##################################

file = "mei.ods"

for (i in 1:12){

if (i < 10) {
  
try(curl_download(paste0("http://www8.receita.fazenda.gov.br/SimplesNacional/Arquivos/estatisticaArrecadacao/MEI_Quantidade_Optantes_0",i,".2022.ods"), destfile = "mei.ods"))

  } else {
  
    try(curl_download(paste0("http://www8.receita.fazenda.gov.br/SimplesNacional/Arquivos/estatisticaArrecadacao/MEI_Quantidade_Optantes_",i,".2022.ods"), destfile = "mei.ods"))
}

} 


j <- 1

for (i in 0:(ano-2012)){  


  print(paste0("lendo Municípios no ano ", 2012+i, " planilha ", j))  
  
  
  assign(paste0("Município_",2012 + i), import(file, sheet = j))  

 
   j <- j + 2 
 
  
}




for (i in 2012:ano) {


df_t <- eval(str2expression(paste0("Município_",i,"[-nrow(Município_",i,"),]")))

colnames(df_t) <- eval(str2expression(paste0("Município_",i,"[1,]")))

df_t <- df_t[-1,]

df_t["uf"] = NA
a = NA
for (j in 1:nrow(df_t)) {
  
if (str_length(df_t[j,1]) == 2) {
  
df_t$uf[j:j] <- df_t[j,1] 

a[j] <- j 



}  

  
  }


df_t["ano"] <- i 

df_t$uf<-na.locf(df_t$uf)

a <- as.data.frame(a)

a <- a[!is.na(a$a),]

df_t<-df_t[-c(a),]


df_t <- df_t %>% 
  pivot_longer(
    cols = JAN:DEZ, 
    names_to = "mês",
    values_to = "qtd_mei")


mesn <- data.frame(mês = c("JAN", "FEV", "MAR","ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"), num = c(01,02,03,04,05,06,07,08,09,10,11,12))


df_t <- left_join(df_t, mesn, by = 'mês')

df_t$num <- formatC(df_t$num, width=2, format="d", flag="0")

df_t["data"] <- paste0("01/",df_t$num,"/",df_t$ano)


df_t["munic_uf"] <- paste0(df_t$`Municípios/UF`,"_",df_t$uf)


assign(paste0("df_t_",i), df_t)

rmv <- paste0("rm(Município_",i,")")

eval(str2expression(rmv))



}

rm(df_t, mesn, i, j, file, df_ano, a, pacotes, rmv, check.packages, ano)


df_merge <- sapply(.GlobalEnv, is.data.frame)
df_merge <-  as.data.frame(do.call(rbind, mget(names(df_merge))))


suporte_munic <- read.xlsx("suporte_munic.xlsx")


dff <- left_join(df_merge, suporte_munic, by = 'munic_uf')


dff <- dff %>% select(cod_munic, data, qtd_mei)


