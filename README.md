# Simulación de la congestión hospitalaria en Galicia

> **AVISO**: Este código ha sido creado con Ubuntu 20.04 y codificación UTF-8. Si se abre con windows los caracteres raros como acentos y Ñs estarán mal representados. En ese caso, para verlo bien es necesario hacer lo siguiente: File > Reopen with encoding > UTF-8 > UTF-8 as default. 
> 
> Esto arregla el visualizado del código, pero aún así, algunas partes se ven afectadas por acentos en la ejecución (Ej: Los nombres de hospitales son el ID de elementos en varias listas) y puede dar error al ejecutar en Windows.

## Introducción
Este repositorio contiene la adaptación del modelo de simulación estadística creado por [Ana López‐Cheda et al. (2021)](https://doi.org/10.1017/S0950268821000959) a datos reales de capacidad asistencial, junto con una aplicación y la paralelización del modelo. El objetivo es simular la evolución epidemiológica de un número de individuos a lo largo de varios días. El modelo compartimenta a cada individuo en varias posibles etapas, que se ven en la siguiente imagen:


<p align="center">
  <img src="https://drive.google.com/uc?export=view&id=1NyPX1Wor_gkohnYgfAwGNyIhjffLvC4j" width="600" title="Estados">
</p>


Cada individuo va cambiando de estado a lo largo de la simulación según unas probabilidades calculadas a partir de datos reales.

El sistema se puede ejecutar por scripts ([sim_datos_agregados.R](sim_datos_agregados.R) y [analisis_capacidad.R](analisis_capacidad.R)), por informe ([Informe_Congestion.Rmd](Informe_Congestion.Rmd)) o mediante una aplicación Shiny ([CongestionHospitalariaApp/app.R](CongestionHospitalariaApp/app.R)).


## Aplicación

### Uso de app
Para ejecutar la aplicación, la cual permite usar los scripts de una manera muy intuitiva e interactiva, se ejecuta desde la base del repositorio el script [CongestionHospitalariaApp/app.R](CongestionHospitalariaApp/app.R).

<p align="center">
  <img src="https://drive.google.com/uc?export=view&id=1i3hFo8GU8AdbuOypLisReAChNxvKhLEu" width="700" title="App">
</p>


### Traducciones en aplicación
Se puede elegir el idioma en el que aparece la aplicación. Esto funciona gracias a un diccionario situado en [CongestionHospitalariaApp/translations/translation.json](CongestionHospitalariaApp/translations/translation.json), donde se especifica la traducción de cada línea desde el idioma principal (English o "en") al resto. La aplicación usa el paquete `shiny.i18n`, y la sintaxis es:

```
translator <- Translator$new(translation_json_path = "./translations/translation.json") # diccionario
translator$set_translation_language("es") # lenguaje por defecto
translator$t("Outliers") # traducción
```

La cadena insertada en `translator$t("Outliers")` debe ser una del diccionario y en la lengua principal, en este caso inglés. Si no existe en el diccionario saltará un warning cuando intente traducir pero no impide que funcione la app, usaría esa cadena por defecto.

## Directamente con scripts
El sistema también se puede ejecutar paso a paso en los scripts, usando [sim_datos_agregados.R](sim_datos_agregados.R), el cual también llama a [analisis_capacidad.R](analisis_capacidad.R).

#### Simulación completa (incluye análisis de capacidad): [sim_datos_agregados.R](sim_datos_agregados.R)

Este script es el principal y ejecuta todo el proceso: tratamiento de datos, análisis de capacidad, cálculos de probabilidades y simulaciones.

> **A tener en cuenta**: la variable num.cores especifica los hilos usados en la simulación. Dependiendo del número de simulaciones un num.cores alto puede causar problemas de RAM. Ejemplo: Un ordenador con 8 hilos y 16 GB de RAM usará, como máximo, alrededor de 8-9 GB de RAM si:
> * `num.cores` = 6
> * `m` = 1000
> * `n.ind` = 1000
> * `n.time` = 250

#### Únicamente análisis de capacidad: [analisis_capacidad.R](analisis_capacidad.R)

Este script sirve para visualizar los análisis de capacidad de cada hospital y del conjunto seleccionado, pero no ejecuta la simulación, únicamente el procesamiento de los datos de capacidad.

> **A tener en cuenta**: Es llamado por `sim_datos_agregados.R`, de tal manera que hay ciertas variables en este script que deben comentarse si se usa `sim_datos_agregados.R`, como `area.sanitaria`, `hosp.ref`, `outlier.filter.type` y `window.size`. Si no se comentan, se sobreescribirán al ejecutar `analisis_capacidad.R`.  
> 
> Esto es un compromiso necesario para poder ejecutar `analisis.capacidad.R` por separado.

## Creación de informe
Así mismo, se puede crear un informe a partir de [Informe_Congestion.Rmd](Informe_Congestion.Rmd). Para su uso apropiado, es necesario tener en cuenta cómo está formado. El archivo llama a otros scripts con la siguiente sintaxis:

```
{r, include=FALSE, cache=FALSE}
knitr::read_chunk('sim_datos_agregados.R')
```
Esto lee los distintos apartados del script en cuestión, delimitados con `# ---- Apartado ----`. Después, ejecuta cada el apartado indicado por el script con esta sintaxis:

```
{r, Apartado, echo=TRUE}
```

Esto reduce la duplicación de código y permite compartimentalizarlo mejor. No obstante, si el nombre de un apartado cambia es necesario cambiarlo también en el `Informe.Rmd`. Además, si el apartado se corta con otro apartado en medio, dará errores.


## Datos y carpetas

### Árbol de carpetas
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
└── weibull_parameters.R

### Explicación de ficheros

* `analisis_capacidad.R`: Incluye el código necesario para analizar la capacidad de cada hospital y del conjunto de hospitales mediante el uso de gráficas. La definición de las gráficas se encuentra en `common/graficas.R`.
* `sim_datos_agregados.R`: Script principal en el que se sitúa la preparación de datos de casos/hospitalizados y las simulaciones.
* `Informe_Congestion.Rmd`: Informe automático parametrizado para ejecutar todo el proceso en conjunto, obteniendo un informe en HTML interactivo.
* `common/`: Incluye funciomnes comunes entre varias partes del repositorio, como pueden ser la definición de gráficas.
* `CongestionHospitalariaApp/`: Aquí reside la aplicación de Shiny, siendo `app.R` el script principal. Además, incluye la carpeta `www/`, que almacena las imágenes e iconos de la aplicación (su nombre debe ser así) y `translations/`, que incluye un diccionario de traducciones entre idiomas para la aplicación.
* `datos/`: Carpeta donde se almacenan los datos de capacidad, Weibull y casos/hospitalizaciones. Los archivos necesarios son:
  * `areas_hospitales_correspondencia.csv` incluye la asociación entre áreas sanitarias y los concellos que pertenecen a estas. Se usa para asociar los datos de capacidad con los casos (SIVIES).
  * `capacidadasistencial.csv` contiene el número de camas ocupadas y el total de camas por unidad/hospital (tanto por pacientes COVID como por pacientes normales) a lo largo de la pandemia.
  * `full_weibull.Rdata` contiene objetos R con las distribuciones Weibull calculadas automáticamente, según grupo de edad y sexo, usando datos del principio de la pandemia.
  * `sivies_agreg_area_sanitaria.csv` es resultado de una consulta agregada a la base de datos de SIVIES. En esta consulta se obtiene el número de casos/hospitalizados agregando por: área sanitaria, grupo de edad, sexo, comunidad, ingreso hospitalario, ingreso en UCI, en qué unidad ingresaron primero (UCI o hospital) y estado (si fueron hospitalizado o sólo fueron casos). Esto permite obtener el total de individuos, proporciones en UCI/hospital y proporción de muertos.







