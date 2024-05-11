--english version below--
# Repositorio precariedad.mundial

Bienvenido al repositorio de datos para el procesamiento de encuestas de hogares, en el marco de la línea de trabajo sobre precariedad mundial del Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED - IIEEP – UBA). Este proyecto utiliza microdatos de encuestas de hogares de institutos de estadística oficiales de distintos países del mundo y los convierte en un dataframe unificado con información homogeneizada.

## Estructura del Repositorio

El repositorio está organizado de la siguiente manera:

- **\Bases**: Contiene las bases de microdatos en el formato de publicación del instituto de estadística de cada país.
- **\scripts**: Incluye los scripts por país utilizados para transformar los datos de cada país en un dataframe homogéneo.
- **\bases_homog**: Contiene las bases homogéneas por país y el archivo base_homog.Rds con los datos unificados.
- **\Resultados**: Contiene tablas de resultados de distintas tasas e indicadores del mercado de trabajo.
- **\procesamiento.R**: Procesa los datos unificados y construye el dataframe de resultados
- **\bases_homog\unificar.R**: Script que unifica toda la información procesada por país en un único dataframe.
- **metadata.xlsx**: Documento que describe los criterios utilizados para la creación de variables de la base homogeneizada.

## Descripción del Dataframe

Las encuestas de cada país son filtradas para obtener información para el total del empleo urbano del país sin incluir el sector público y el trabajo doméstico remunerado. El dataframe base_homog.Rds cuenta con las siguientes columnas:

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

- **La calidad del empleo en la Argentina reciente: un análisis sobre su relación con la calificación y el tamaño de las unidades productivas en perspectiva comparada** J Graña, G Weksler, F Lastra *Trabajo y Sociedad 38, 423-446*

- **Calidad del empleo y estructura del mercado de trabajo en América Latina desde una perspectiva comparada** S Fernández-Franco, JM Graña, F Lastra, G Weksler *Ensayos de Economía 32 (61), 124-151*






# Repository precariedad.mundial

Welcome to the data repository for household survey processing, within the framework of the world precariousness research line of the Center for Studies on Population, Employment, and Development (CEPED - IIEEP - UBA). This project uses microdata from household surveys conducted by official statistical institutes in various countries around the world and converts them into a unified dataframe with standardized information.

## Repository Structure

The repository is organized as follows:

- **\Bases**: Contains microdata sets in the publication format of each country's statistical institute.
- **\scripts**: Includes country-specific scripts used to transform the data from each country into a homogeneous dataframe.
- **\bases_homog**: Contains homogeneous databases by country and the base_homog.Rds file with unified data.
- **\Results**: Contains result tables of different rates and labor market indicators.
- **\processing.R**: Processes unified data and constructs the results dataframe.
- **metadata.xlsx**: Document describing the criteria used to create variables in the standardized database.

## Description of the Dataframe

Surveys from each country are filtered to obtain information for total urban employment in the country, excluding the public sector and paid domestic work. The base_homog.Rds dataframe includes the following columns:

- **PAIS**: Country of the survey.
- **WEIGHT**: Weight.
- **CATOCUP**: Occupational category.
- **COND**: Activity condition.
- **PRECAPT**: Precariousness due to part-time work.
- **PRECAREG**: Precariousness due to lack of registration of the employment relationship.
- **PRECATEMP**: Precariousness due to temporary work.
- **PRECASALUD**: Precariousness due to lack of health coverage.
- **PRECASEG**: Precariousness due to lack of social security contributions.
- **TAMA**: Establishment size.
- **CALIF**: Job qualification.
- **ING**: Principal occupation income in local currency.
- **ANO**: Survey reference year.
- **PERIODO**: Survey reference period.

Thank you for using our repository! If you have any questions or suggestions, please feel free to contact us. If you use information from this project, we appreciate citing this repository or one of our publications:

- **La calidad del empleo en la Argentina reciente: un análisis sobre su relación con la calificación y el tamaño de las unidades productivas en perspectiva comparada** J Graña, G Weksler, F Lastra *Trabajo y Sociedad 38, 423-446*

- **Calidad del empleo y estructura del mercado de trabajo en América Latina desde una perspectiva comparada** S Fernández-Franco, JM Graña, F Lastra, G Weksler *Ensayos de Economía 32 (61), 124-151*
