
# Parte I - Librerías -----------------------------------------------------

require(rvest)
require(RSelenium)
require(dplyr)

# Parte II - Navegador ----------------------------------------------------

servidor <-rsDriver(browser = "firefox", port = 5664L) 
cliente <- servidor$client              
cliente$navigate("https://www.familysearch.org/es/") 

# Inicio
iniciar_sesion <- cliente$findElement(using = "css", "#signInLink")
iniciar_sesion$clickElement() 

# Username
username <- cliente$findElement(using = "css", value = '#userName')
# Clic
username$sendKeysToElement(list("agusnieto77"))

# Password and Clic
passwd <- cliente$findElement(using = "css", value = '#password')
passwd$sendKeysToElement(list("Cora@2022"))

# Iniciar sesión
iniciar_sesion2 <- cliente$findElement(using = "css", "#login")
iniciar_sesion2$clickElement() 

# Ir a colecciones 
buscar <- cliente$findElement(using = "xpath", "/html/body/div[1]/header/div[2]/div[1]/nav/div[2]/button")
buscar$clickElement() 

buscar <- cliente$findElement(using = "xpath", "/html/body/div[1]/header/div[2]/div[1]/nav/div[2]/ul/li[1]/a")
buscar$clickElement() 

colecciones <- cliente$findElement(using = "xpath", "/html/body/div[1]/div/div/div/div[1]/div/div/div/div[1]/div/div/div/main/div/div/div/div[2]/div[2]/div/div/div/div/div[2]/div[1]/div/div[2]/div/a")
colecciones$clickElement() 

todas_paginas <- cliente$findElement(using = "xpath", '/html/body/div[1]/div/div/div/div[1]/div/div/div/div[1]/div/div/div/main/div/div/div/div[3]/div/div/div/div/div/div[3]/div[1]/div/div[2]/div/div/select/option[5]')
todas_paginas$clickElement() 

censo_arg_1895 <- cliente$findElement(using = "xpath", "/html/body/div/div/div/div/div[1]/div/div/div/div[1]/div/div/div/main/div/div/div/div[3]/div/div/div/div/div/div[2]/div/table/tbody/tr[150]/td[1]/a")
censo_arg_1895$clickElement() 

censo_arg_1895_todo <- cliente$findElement(using = "xpath", '//*[@id="main"]/div/div/div/div[2]/div/div/div/div/div/div[1]/div[3]/div[2]/a')
censo_arg_1895_todo$clickElement() 

cliente$navigate("https://www.familysearch.org/search/image/index?owc=https://www.familysearch.org/service/cds/recapi/collections/1410078/waypoints") 

censo_arg_1895_pampa <- cliente$findElement(using = "css", 'li.ng-scope:nth-child(15) > a:nth-child(1)')
censo_arg_1895_pampa$clickElement() 

cliente$navigate("https://www.familysearch.org/search/image/index?owc=M68G-4N5%3A23938501%3Fcc%3D1410078") 

censo_arg_1895_pampa_dpto_4 <- cliente$findElement(using = "xpath", '/html/body/div[1]/main/div[1]/fs-film-viewer/div/div[1]/div[1]/div/fs-waypoints/fs-waypoint-browse-pane/ul/li[1]/a')
censo_arg_1895_pampa_dpto_4$clickElement() 

censo_arg_1895_pampa_dpto_4_bernasconi <- cliente$findElement(using = "xpath", '/html/body/div[1]/main/div[1]/fs-film-viewer/div/div[1]/div[1]/div/fs-waypoints/fs-waypoint-browse-pane/ul/li[1]/a')
censo_arg_1895_pampa_dpto_4_bernasconi$clickElement() 

censo_arg_1895_pampa_dpto_4_bernasconi_p1 <- cliente$findElement(using = "xpath", '/html/body/div[1]/main/div[1]/fs-film-viewer/div/div[1]/div[2]/div[3]/div[1]/span[3]')

output <- data.frame()
for (i in 1:191){
  cliente$findElement(using = "xpath", '/html/body/div[1]/main/div[1]/fs-film-viewer/div/div[1]/div[2]/div[3]/div[1]/span[3]')$clickElement() 
  Sys.sleep(4.0)
  output <- rbind(output,html_table(read_html(cliente$getPageSource()[[1]]))[[2]][,2:8] |> 
                    mutate(censo = html_text2(html_element(read_html(cliente$getPageSource()[[1]]), 'li.ng-scope:nth-child(2) > a:nth-child(1)')),
                           prov_territorio = html_text2(html_element(read_html(cliente$getPageSource()[[1]]), 'li.ng-scope:nth-child(3) > a:nth-child(1)')),
                           departamento = html_text2(html_element(read_html(cliente$getPageSource()[[1]]), 'li.ng-scope:nth-child(4) > a:nth-child(1)')),
                           distrito_pedania_subdivision = html_text2(html_element(read_html(cliente$getPageSource()[[1]]), 'span.ng-scope')),
                           imagen = i+1))
}

# Filtramos por valores únicos
bernasconi_1895 <- output |> distinct(Nombre, Sexo, Edad, `Año de nacimiento (estimado)`, 
                                      `Lugar de nacimiento`, `Estado civil`, 
                                      .keep_all = T)

# Imprimimos
bernasconi_1895

# Guardamos
write.csv2(bernasconi_1895,'bernasconi_1895.csv', row.names = F)
saveRDS(bernasconi_1895,'bernasconi_1895.rds')
