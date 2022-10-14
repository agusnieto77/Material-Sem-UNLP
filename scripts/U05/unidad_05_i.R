require(tidyverse)
require(viridis)
require(sf)

links <- c('https://datos.mardelplata.gob.ar/sites/default/files/fracciones_0.zip',
           'https://datos.mardelplata.gob.ar/sites/default/files/paradas.zip',
           'https://datos.mardelplata.gob.ar/sites/default/files/radios_2.zip',
           'https://datos.mardelplata.gob.ar/sites/default/files/barrios_populares.zip')

dir.create('scripts/U05/shp')

for (i in links) {
  
  directorio <- 'scripts/U05/shp/'
  
  url <- i
  
  destino <- paste0(directorio,basename(url))
  
  download.file(url <- url,
                destfile = destino,
                mode='wb')
  
  unzip(zipfile = destino,
        exdir = directorio)
  
}

frac_mdp <- read_sf(paste0(directorio,'fracciones.shp'))
radi_mdp <- read_sf(paste0(directorio,'radios.shp'))
para_mdp <- read_sf(paste0(directorio,'paradas.shp'))
b_po_mdp <- read_sf(paste0(directorio,'barrios_populares_2020.shp'))

st_crs(frac_mdp) = "WGS84"

ggplot(
  frac_mdp 
  |> filter(!fraccion %in% c('77','83','82','81','80','79','78','76','75','74','73'))
         ) +
  geom_sf(
    fill = 'skyblue', 
    alpha = .1
    ) +
  geom_sf_text(aes(label = fraccion),size = 2) +
  geom_sf(aes(
    fill = cantidad_f
  ),
          data = b_po_mdp) +
  geom_sf(data = para_mdp, size = .5, color = 'purple', alpha = .1) +
  coord_sf(
    xlim = c(-57.8, -57.5), 
    ylim = c(-38.1, -37.9), 
    expand = FALSE) +
  scale_fill_viridis() +
  labs(x=NULL,y=NULL) +
  theme_bw() +
  theme(
    legend.title = element_blank()
  )
  