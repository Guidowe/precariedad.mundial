# Repositorio precariedad.mundial

Bienvenido al repositorio de datos para el procesamiento de encuestas de hogares, en el marco de la línea de trabajo sobre precariedad mundial del Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED - IIEEP – UBA). Este proyecto utiliza microdatos de encuestas de hogares de institutos de estadística oficiales de distintos países del mundo y los convierte en un dataframe unificado con información homogeneizada.

## Estructura del Repositorio

El repositorio está organizado de la siguiente manera:

- **Bases**: Contiene las bases de microdatos en el formato de publicación del instituto de estadística de cada país.
- **scripts**: Incluye los scripts por país utilizados para transformar los datos de cada país en un dataframe homogéneo.
- **bases_homog**: Contiene las bases homogéneas por país y el archivo base_homog.Rds con los datos unificados.
- **unificar.R**: Script que unifica toda la información procesada por país en un único dataframe.
- **metada.xlsx**: Documento que describe los criterios utilizados para la creación de variables de la base homogeneizada.

## Descripción del Dataframe

Las encuestas de cada país son filtradas para obtener información solo del sector urbano para el empleo, sin incluir el sector público y el trabajo doméstico remunerado. El dataframe base_homog.Rds cuenta con las siguientes columnas:

- **PAIS**: País de la encuesta.
- **WEIGHT**: Ponderador.
- **CATOCUP**: Categoría ocupacional.
- **COND**: Condición de actividad.
- **PRECAPT**: Precariedad por trabajo part-time.
- **PRECAREG**: Precariedad por no registro de la relación laboral.
- **PRECATEMP**: Precariedad por trabajo temporario.
- **PRECASALUD**: Precariedad por falta de cobertura de salud.
- **PRECASEG**: Precariedad por falta de aportes a la seguridad social.
- **TAMA**: Tamaño del establecimiento.
- **CALIF**: Calificación del puesto.
- **ING**: Ingreso de la ocupación principal en moneda local.
- **ANO**: Año de referencia de la encuesta.
- **PERIODO**: Período de referencia de la encuesta.

¡Gracias por utilizar nuestro repositorio! Si tienes alguna pregunta o sugerencia, no dudes en contactarnos. Si utilizas información de este proyecto, agradecemos que cites este repositorio o alguna de nuestras publicaciones:

- **La calidad del empleo en la Argentina reciente: un análisis sobre su relación con la calificación y el tamaño de las unidades productivas en perspectiva comparada**
  - J Graña, G Weksler, F Lastra
  - *Trabajo y Sociedad 38, 423-446*

- **Calidad del empleo y estructura del mercado de trabajo en América Latina desde una perspectiva comparada**
  - S Fernández-Franco, JM Graña, F Lastra, G Weksler
  - *Ensayos de Economía 32 (61), 124-151*

