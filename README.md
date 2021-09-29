# Simulación de la congestión hospitalaria en Galicia

## Introducción
Este repositorio contiene la adaptación del modelo de simulación estadística creado por REFERENCIA a datos reales. El objetivo es simular la evolución epidemiológica de un número de individuos a lo largo de varios días. El modelo compartimenta a cada individuo en varias posibles etapas, que se ven en la siguiente imagen:

![image](https://drive.google.com/uc?export=view&id=1WJgkYuF3glt7ONa0ggvaAoaHsG5ZP7q5)

Cada individuo va cambiando de estado a lo largo de la simulación según unas probabilidades calculadas a partir de datos reales.

El sistema se puede ejecutar por scripts ([sim_datos_agregados.R](sim_datos_agregados.R) y [analisis_capacidad.R](analisis_capacidad.R)), por informe ([Informe_Congestion.Rmd](Informe_Congestion.Rmd)) o mediante una aplicación Shiny ([CongestionHospitalariaApp/app.R](CongestionHospitalariaApp/app.R)).


## Aplicación
Para ejecutar la aplicación, la cual permite usar los scripts de una manera muy intuitiva e interactiva, se ejecuta desde la base del repositorio el script [CongestionHospitalariaApp/app.R](CongestionHospitalariaApp/app.R).

### Traducciones en aplicación
Se puede elegir el idioma en el que aparece la aplicación. Esto funciona gracias a un diccionario situado en [CongestionHospitalariaApp/translations/translation.json](CongestionHospitalariaApp/translations/translation.json), donde se especifica la traducción de cada línea desde el idioma principal (English o "en") al resto. La aplicación usa el paquete `shiny.i18n`, y la sintaxis es:

```
translator <- Translator$new(translation_json_path = "./translations/translation.json") # diccionario
translator$set_translation_language("es") # lenguaje por defecto
translator$t("Outliers") # traducción
```

La cadena insertada en `translator$t("Outliers")` debe ser una del diccionario y en la lengua principal, en este caso inglés. Si no existe en el diccionario saltará un warning cuando intente traducir pero no impide que funcione la app, usaría esa cadena por defecto.

## Directamente con scripts
El sistema también se puede ejecutar paso a paso en los scripts, primero [sim_datos_agregados.R](sim_datos_agregados.R) y después [analisis_capacidad.R](analisis_capacidad.R).

#### Paso 1: [sim_datos_agregados.R](sim_datos_agregados.R)

#### Paso 2: [analisis_capacidad.R](analisis_capacidad.R)

## Creación de informe
Así mismo, se puede crear un informe a partir de [Informe_Congestion.Rmd](Informe_Congestion.Rmd). Para su uso apropiado, es necessario tener en cuenta cómo está formado. El archivo llama a otros scripts con la siguiente sintaxis:

```
{r, include=FALSE, cache=FALSE}
knitr::read_chunk('sim_datos_agregados.R')
```
Esto lee los distintos apartados del script en cuestión, delimitados con `# ---- Apartado ----`. Después, ejecuta cada el apartado indicado por el script con esta sintaxis:

```
{r, Apartado, echo=TRUE}
```

Esto reduce la duplicación de código y permite compartimentalizarlo mejor. No obstante, si el nombre de un apartado cambia es necesario cambiarlo también en el Informe.Rrmd. Además, si el apartado se corta con otro apartado en medio, dará errores.


## Datos y carpetas
Este es el árbol de carpetas necesario para que todo funcione bien (se pone para que se vean los archivos de datos necesarios):

├── analisis_capacidad.R\
├── common\
│   ├── create_table.R\
│   └── graficas.R\
├── CongestionHospitalariaApp\
│   ├── app.R\
│   ├── translations\
│   │   └── translation.json\
│   └── www\
│   │    ├── galicia.svg\
│   │    ├── logo_citic_cortado.png\
│   │    ├── logo_citic.png\
│   │    ├── logo_feder.jpg\
│   │    ├── logo_gain.png\
│   │    ├── logo_xunta.png\
│   │    ├── spain.svg\
│   │    └── united-kingdom.svg\
├── datos\
│   ├── areas_hospitales_correspondencia.csv\
│   ├── capacidadasistencial.csv\
│   ├── full_weibull.Rdata\
│   ├── org\
│   │   ├── SERGASCOVID_05_11.RData\
│   ├── sivies_agreg_area_sanitaria.csv\
├── Informe_Congestion.Rmd\
├── README.md\
├── sim_datos_agregados.R\
└── weibull_parameters.R\










