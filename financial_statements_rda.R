rm(list=ls())

##LIBRERIAS#########################################################
library("RSelenium")
library("rvest")
library("tidyverse")
library("RODBC")
library("lubridate")

##FUNCIONES#########################################################
myswitch <- function (remDr, windowId) 
{
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL, 
                   remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

acc_to_number <- function(columna)
{
  as.numeric(gsub("\\(", "-", gsub("\\)|,", "", columna)))
}

fct_smv_esf <- NULL
fct_smv_er <- NULL

##CONFIGURACIONES###################################################
setwd("C:/Users/msam/dchrome/95.0.4638.69")  #MODIFICAR: ruta donde se encuentra el controlador chromedriver previamente descargado (https://chromedriver.chromium.org/)
browser <- "chrome"  #NO MODIFICAR
chromever <- '95.0.4638.69'   #MODIFICAR: version del chromedriver utilizado
pagina_web = "https://www.smv.gob.pe/Frm_InformacionFinancieraPorPeriodo?data=16BF4C8EF499F678B5AE402C1CFE8FC16710365A6C"  #NO MODIFICAR
anio <- 2021  #MODIFICAR: parametro del proceso. Selecciona el anio
trimestre <- 3  #MODIFICAR: parametro del proceso. Selecciona el trimestre
inicio <- 1 #MODIFICAR (DE SER NECESARIO): es fila de inicio, dejar en 1; salvo que el proceso se haya cortado y se desee continuar desde una fila en especifico
con <- odbcConnect(dsn="dwsbs", uid="msam", pwd="msam2104")   #MODIFICAR: conexion SQL, modificar 1) uid (usuario) y 2) pwd (contraseña)
rD <- rsDriver(browser=browser, port=4585L, chromever=chromever, verbose=F)  #NO MODIFICAR

##RDA Y WEBSCRAPPING################################################
t0 <- Sys.time()
remDr <- rD[["client"]]
remDr$navigate(pagina_web)

mmdd <- switch(trimestre, "1"="0331","2"="0630","3"="0930","4"="1231")

#Entrar al buscador segun combinacion
remDr$findElement("css", "#MainContent_cboTipo_2")$clickElement()     #tipo (Todos)
remDr$findElement("css", "#MainContent_cboPeriodo_0")$clickElement()  #periodo (Intermedio)
remDr$findElement("css", paste("#MainContent_cboAnio option[value='",anio,"']",sep=""))$clickElement()  #anio (Anio seleccionado)
remDr$findElement("css", paste("#MainContent_cboTrimestre option[value='",trimestre,"']",sep=""))$clickElement()  #trimestre (Trimestre seleccionado)

#Buscar seleccion
remDr$findElement("css", "#MainContent_cbBuscar")$clickElement()
Sys.sleep(5)

#Extraer tabla y quedarse con el expediente de fecha de presentacion mas reciente
html <- remDr$getPageSource()[[1]]
total <- read_html(html) %>%
  html_nodes("#MainContent_grdInfoFinanciera") %>%
  html_table() %>%.[[1]] %>%
  select(c(2,4,7,8)) %>% data.frame() %>%
  rename("RSOCIAL"=1, "TIPO"=2, "EXPEDIENTE_ID"=3, "FECHA_PRESENTACION"=4) %>%
  mutate(FILA=seq.int(nrow(.))) %>%
  mutate(FECHA_PRESENTACION = dmy_hms(FECHA_PRESENTACION))

validos <- total %>% 
  group_by(RSOCIAL,TIPO) %>%
  summarise(FECHA_PRESENTACION= max(FECHA_PRESENTACION))

inf_finan <- left_join(validos, total, by = c('RSOCIAL','TIPO', 'FECHA_PRESENTACION')) %>%
  mutate(PERIODO_ID = as.numeric(paste(anio,mmdd,sep=""))) %>%
  select(c(1,2,3,4,6,5)) %>%
  arrange(FILA)
      
#Iterativa para entrar a cada uno de los emisores
for (y in inf_finan$FILA[inf_finan$FILA>=inicio]) {
h_inf_finan <- paste("#MainContent_grdInfoFinanciera_lkButton_",y-1, sep="")

#Hacer click en la lupa para entrar a la empresa
remDr$findElement("css", h_inf_finan)$clickElement()
Sys.sleep(6)
remDr$findElement("css", "body")$sendKeysToElement(list(key = "home"))
for (k in 1:10) {
  remDr$findElement("css", "body")$sendKeysToElement(list(key = "down_arrow"))
}

#Extraer informacion de la tabla de emisores
emisor <- NULL
while(is.null(emisor)){
  html <- remDr$getPageSource()[[1]]
  Sys.sleep(2.5)
  tryCatch({
    emisor <- read_html(html) %>% # parse HTML
      html_nodes("#MainContent_grdInfoFinancieraDetalle") %>%
      html_table() %>%.[[1]] %>% data.frame() %>%
      mutate(FILA=seq.int(nrow(.))) %>%
      filter(Documento=="Estado Financieros") %>%
      filter(N..Expediente == toString(inf_finan[inf_finan$FILA==y,"EXPEDIENTE_ID"]))
    },error = function(e){NULL})
  Sys.sleep(1.5)
}

#Iterativa para extraer informacion de los EEFF#
  if (length(emisor$FILA)==1) {
    h_emisor <- paste("#MainContent_grdInfoFinancieraDetalle_HyperLink1_",emisor$FILA-1, sep="")
      
    #Hacer click en la lupa para entrar a los EEFF
    Sys.sleep(1.5)
    remDr$findElement("css", h_emisor)$clickElement()
    Sys.sleep(1.5)
    
    #Seleccionar pestania de internet de los EEFF
    tryCatch({
        windows_handles <-  remDr$getWindowHandles()   
        myswitch(remDr, windows_handles[[2]])
        
        #Extraer ESF
        html <- remDr$getPageSource()[[1]]
        esf <- read_html(html) %>% # parse HTML
          html_nodes("#grdBalanceGeneral") %>%
          html_table() %>%.[[1]] %>% 
          select(c(1,3)) %>%
          rename("CONCEPTO"=1, "SALDO"=2) %>%
          mutate(FILA_ID=seq.int(nrow(.))) %>%
          data.frame() %>%
          cbind(inf_finan[inf_finan$FILA==y,],.) %>%
          select(c(5,1,2,3,4,9,7,8,6)) %>%
          mutate(SALDO =  acc_to_number(SALDO))
        
        #Hacer click en ER
        remDr$findElement("css", "#lnkEstadoGP")$clickElement()
        Sys.sleep(1.5)
        
        #Extraer ER
        html <- remDr$getPageSource()[[1]]
        er <- read_html(html) %>% 
          html_nodes("#grdEstadoGP") %>%
          html_table() %>%.[[1]] %>%
          select(c(1,5)) %>%
          rename("CONCEPTO"=1, "SALDO"=2) %>%
          mutate(FILA_ID=seq.int(nrow(.))) %>%
          data.frame() %>%
          cbind(inf_finan[inf_finan$FILA==y,],.) %>%
          select(c(5,1,2,3,4,9,7,8,6)) %>%
          mutate(SALDO =  acc_to_number(SALDO))
        
        #Consolidar informacion
        fct_smv_esf <- rbind(fct_smv_esf, esf)
        fct_smv_er <- rbind(fct_smv_er, er)
        } ,
    error = function(e){NULL})
    
    #Cerrar
    Sys.sleep(1)
    remDr$closeWindow()
    #Seleccionar pestania de internet de la empresa
    myswitch(remDr, windows_handles[[1]])
    Sys.sleep(2)
}
webElem <-NULL
while(is.null(webElem)){
  tryCatch({remDr$findElement("css", "#MainContent_cboRegresaArriba")$clickElement()},
           error = function(e){NULL})
  Sys.sleep(3)
  webElem <- tryCatch({remDr$findElement("css", h_inf_finan)},
                      error = function(e){NULL})
  if (is.null(webElem)) {
    remDr$findElement("css", "body")$sendKeysToElement(list(key = "home"))
    for (k in 1:10) {
      remDr$findElement("css", "body")$sendKeysToElement(list(key = "down_arrow"))
    }
  }
}
}
tf <- Sys.time()
t <- tf-t0
print(t)

#Terminar procesos RDA / Webscrapping
remDr$close()
rD$server$stop()
rm(rD, remDr)
gc()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

##GRABACION EN ORACLE SQL#################################
sqlSave(con, fct_smv_esf[,-9], tablename='FCT_SMV_ESF', rownames=FALSE, append=T, 
        varTypes=c(PERIODO_ID="NUMBER(8)", SALDO="NUMBER(18,2)", FECHA_PRESENTACION="DATE"))
sqlSave(con, fct_smv_er[,-9], tablename='FCT_SMV_ER', rownames=FALSE, append=T,
        varTypes=c(PERIODO_ID="NUMBER(8)", SALDO="NUMBER(18,2)", FECHA_PRESENTACION="DATE"))
#Terminar conexion Oracle SQL
odbcClose(con)
rm(con)
