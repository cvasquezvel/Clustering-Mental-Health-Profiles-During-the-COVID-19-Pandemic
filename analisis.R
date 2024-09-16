##############################################################################
# Determinación de perfiles clínicos de salud mental en pacientes asegurados #
# a Essalud en época de pandemia                                             #
##############################################################################

# Autores:
# - Galvan, Jhon.
# - Soto, Percy.
# - Vásquez, Christian.
# - Vega, Carlo.
# - Zavaleta, Diego.
  

# Cambiar el directorio de trabajo ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Definir opciones ----

options(scipen = 9999,
        max.print = 9999)

# Cargar paquetes ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(haven, dplyr, tidyverse, labelled, ggthemes, SmartEDA, klaR,
               factoextra, ggplot2, clustMixType, broom, fpc,clusterSim,
               clValid, tidyr, extrafont,patchwork, clustertend,
               factoextra, FeatureImpCluster, flexclust, ade4, pca3d,
               tidyr, rJava, RWeka, mlr3, mlr3cluster)

### Tema elegante para plots ----

theme_elegante <- function(base_size = 10,
                           base_family = "Raleway"
)
{
  color.background = "#FFFFFF" # Chart Background
  color.grid.major = "#D9D9D9" # Chart Gridlines
  color.axis.text = "#666666" # 
  color.axis.title = "#666666" # 
  color.title = "#666666"
  color.subtitle = "#666666"
  strip.background.color = '#9999CC'
  
  ret <-
    theme_bw(base_size=base_size) +
    
    # Set the entire chart region to a light gray color
    # theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.background = element_rect(fill = "white", 
                                          colour = NA)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    # theme(panel.border=element_rect(color=color.background)) +
    theme(panel.border=element_rect(fill = NA, 
                                    colour = "grey20")) +
    
    # Format the grid
    theme(panel.grid = element_line(colour = color.grid.major)) +
    theme(panel.grid.major=element_line(color=color.grid.major,size=.55, linetype="solid")) +
    # theme(panel.grid.minor=element_line(color=color.grid.major,size=.55, linetype="solid")) +
    theme(panel.grid.minor=element_line(color=color.grid.major, linetype="solid",
                                        size = rel(0.5))) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=base_size-3,color=color.axis.title, family = base_family)) +
    
    theme(strip.text.x = element_text(size=base_size,color=color.background, family = base_family)) +
    theme(strip.text.y = element_text(size=base_size,color=color.background, family = base_family)) +
    #theme(strip.background = element_rect(fill=strip.background.color, linetype="blank")) +
    theme(strip.background = element_rect(fill = "grey70", colour = "grey20")) +
    # theme(panel.border= element_rect(fill = NA, colour = "grey70", size = rel(1)))+
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, 
                                  size=20, 
                                  vjust=1.25, 
                                  family=base_family, 
                                  hjust = 0.5
    )) +
    
    theme(plot.subtitle=element_text(color=color.subtitle, size=base_size+2, family = base_family,  hjust = 0.5))  +
    
    theme(axis.text.x=element_text(size=base_size,color=color.axis.text, family = base_family,
                                   angle=90, hjust=1)) +
    theme(axis.text.y=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(text=element_text(size=base_size, color=color.axis.text, family = base_family)) +
    
    theme(axis.title.x=element_text(size=base_size+2,color=color.axis.title, vjust=0, family = base_family)) +
    theme(axis.title.y=element_text(size=base_size+2,color=color.axis.title, vjust=1.25, family = base_family)) +
    theme(plot.caption=element_text(size=base_size-2,color=color.axis.title, vjust=1.25, family = base_family)) +
    
    # Legend  
    theme(legend.text=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.title=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.key=element_rect(colour = color.background, fill = color.background)) +
    theme(legend.position="bottom", 
          legend.box = "horizontal", 
          legend.title = element_blank(),
          legend.key.width = unit(.75, "cm"),
          legend.key.height = unit(.75, "cm"),
          legend.spacing.x = unit(.25, 'cm'),
          legend.spacing.y = unit(.25, 'cm'),
          legend.margin = margin(t=0, r=0, b=0, l=0, unit="cm")) +
    
    # Plot margins
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
  
  ret
}

# Importar base de datos ----

Database <- read_dta("Database.dta")%>%
  as_factor()%>%
  mutate(Sexo = factor(Sexo, 
                          levels = c("0","1"),
                          labels = c("Másculino","Femenino")),
         Estudiosfinalizados = factor(Estudiosfinalizados,
                                         levels = c("1","2","3","4"),
                                         labels = c("Primaria","Secundaria",
                                                    "Técnico","Universitario")),
         Estadocivil = factor(Estadocivil, 
                                 levels = c("1","2","3","4"),
                                 labels = c("Casado o conviviente","Divorciado",
                                            "Soltero", "Viudo")),
         Situaciónlaboral = factor(Situaciónlaboral, 
                                 levels = c("1","2","3"),
                                 labels = c("Empleo formal","Empleo informal",
                                            "Sin empleo")),
         Profesaalgunareligión = factor(Profesaalgunareligión, 
                                           levels = c("0","1"),
                                           labels = c("No","Sí")),
         Ustedhasidodiagnosticadode = factor(Ustedhasidodiagnosticadode, 
                                                levels = c("0","1"),
                                                labels = c("No","Sí")),
         estres = factor(estres,
                         levels = c("0","1"),
                         labels = c("No","Con estrés"))
  ) %>%
  rename("sexo" = "Sexo",
         "estudios" = "Estudiosfinalizados",
         "civil" = "Estadocivil",
         "edad" = "Edad",
         "labor" = "Situaciónlaboral",
         "religion" = "Profesaalgunareligión",
         "diag_prev" = "Ustedhasidodiagnosticadode",
         "intrusion" = "INTRUSIÓN",
         "evitacion" = "EVITACIÓN",
         "hiperactivacion" = "HIEPRACTIVACIÓN"
         ) %>%
  dplyr::select(-c(1:2),-c(9:34),
                -agecat) %>%
  labelled::set_variable_labels(edad = "Edad, años",
                      sexo = "Sexo",
                      civil = "Estado civil",
                      estudios = "Nivel educativo",
                      labor = "Estado laboral",
                      religion = "Religión",
                      PHQ_1 = "Poco interés en hacer cosas",
                      PHQ_2 = "Sensación de decaimiento, depresión o no esperanza",
                      PHQ_3 = "Dificultad para quedarse o permanecer dormido, o dormir demasiado",
                      PHQ_4 = "Sensación de cansancio o poca energía",
                      PHQ_5 = "Sin apetito o exceso de apetito",
                      PHQ_6 = "Sensación de estar mal consigo mismo o con su familia",
                      PHQ_7 = "Dificultad para concentrarse en actividades rutinarias",
                      PHQ_8 = "Sensación de movilidad lenta o excesiva",
                      PHQ_9 = "Pensamiento suicida o autolesivo",
                      GAD_1 = "Sensación de nerviosismo, ansiedad o alteración",
                      GAD_2 = "No poder dejar de preocuparse",
                      GAD_3 = "Preocupación excesiva por diferentes cosas",
                      GAD_4 = "Dificultad para relajarse",
                      GAD_5 = "Intranquilidad y no poder quedarse quieto",
                      GAD_6 = "Irritación o enfado con facilidad",
                      GAD_7 = "Sensación de que algo terrible va a suceder",
                      IES_1 = "Cualquier recuerdo me hacía volver a sentir lo que sentía antes",
                      IES_2 = "Tenía problemas para permanecer dormido",
                      IES_3 = "Otras cosas me hacían pensar en el suceso",
                      IES_4 = "Me sentía irritable y enojado",
                      IES_5 = "Procuraba no alterarme cuando recordaba lo sucedido",
                      IES_6 = "Pensaba en ello aún cuando no quería",
                      IES_7 = "Sentía como si no hubiese sucedido o no fuese real",
                      IES_8 = "Me mantenía lejos de cualquier cosa que me recordara lo sucedido",
                      IES_9 = "Imágenes del suceso asaltaban mi mente",
                      IES_10 = "Me sobresaltaba y asustaba fácilmente",
                      IES_11 = "Intentaba no pensar en el suceso",
                      IES_12 = "Me daba cuenta de que quedaban muchos sentimientos sin resolver",
                      IES_13 = "Mis sentimientos sobre el suceso estaban como adormecidos",
                      IES_14 = "Me encontraba como si estuviese funcionando o sintiendo como durante el evento",
                      IES_15 = "Tenía problemas para conciliar el sueño",
                      IES_16 = "Me invadían oleadas de fuertes sentimientos sobre lo sucedido",
                      IES_17 = "Intentaba apartarlo de mi memoria",
                      IES_18 = "Tenía problemas de concentración",
                      IES_19 = "Cosas que me recordaban lo sucedido me causaban reacciones fisiológicas tales como transpiración, dificultad al respirar, nauseas o taquicardia",
                      IES_20 = "Soñaba con lo sucedido",
                      IES_21 = "Me sentía vigilante y en guardia",
                      IES_22 = "Intentaba no hablar de ello",
                      FCV_1 = "Tengo mucho miedo al covid",
                      FCV_2 = "Me pone incomodo pensar en el covid",
                      FCV_3 = "Mis manos se ponen humedas cuando pienso en covid",
                      FCV_4 = "Tengo miedo de perder mi vida por el covid",
                      FCV_5 = "Cuando veo noticias sobre covid me pongo nervioso",
                      FCV_6 = "No puedo dormir por la preocupación de contagiarme de covid",
                      FCV_7 = "Mi corazón se acelera cuando pienso en contagiarme de covid")%>%
  dplyr::select(-FCV_1,-FCV_2,-FCV_3,-FCV_4,-FCV_5,-FCV_6,-FCV_7,-FCV19STOTAL) 

data.clust<-Database%>%dplyr::select(-PHQ9TOTAL,-GAD7TOTAL,-IESRTOTAL,#-edad,
                                     #-sexo,-civil,-estudios,-labor,-religion,
                                     -intrusion,-evitacion,-hiperactivacion,
                                     -ansiedad,-depresion,-estres,
                                     -F1,-F2,-SintomasCOVID)%>%
  dplyr::mutate_at(.vars = vars(dplyr::matches(c("PHQ","GAD"))),
                   .funs = funs(factor(., levels = 0:3,
                                       ordered = TRUE)))%>%
  dplyr::mutate_at(.vars = vars(dplyr::matches(c("IES"))),
                   .funs = funs(factor(., levels = 0:4,
                                       ordered = TRUE)))

data.final <- data.clust %>%
  dplyr::bind_cols(Database%>%dplyr::select(intrusion,evitacion,hiperactivacion,
                                            ansiedad,depresion,estres,
                                            F1,F2,SintomasCOVID,
                                            PHQ9TOTAL,GAD7TOTAL,IESRTOTAL))

summarytools::dfSummary(data.clust)
str(data.clust)


# Crear un informe en el que la variable de destino sea categórica
# ExpReport(Database,theme=theme_economist(),
#           op_file="Samp1.html",Rc=3)


#----------------------#
# I. CLUSTER K-MODES   #
#----------------------#

# I. CLUSTER K-MODES ----------------------------------------

# 1. Encuentre el número óptimo de clusters para un k-modes ----

datos.t <- data.clust %>% as.data.frame() %>%
  dplyr::select(-edad,-sexo,-civil,-estudios,-labor,-religion)

# 1.1. Criterio de la suma de cuadrados ----
RNGkind(sample.kind = "Rounding")
set.seed(2021)
fviz_nbclust(datos.t, kmodes, method = "wss", k.max=10) +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "criterio de la suma de cuadrados") + theme_bw()

# 1.2. Criterio del Grafico de Silueta ----

RNGkind(sample.kind = "Rounding")
set.seed(2021)
fviz_nbclust(datos.t,
             kmodes, method = "silhouette", k.max=10) +
  labs(subtitle = "Silhouette method")

# 1.3. Criterio del Método de Elbow ----

RNGkind(sample.kind = "Rounding")
set.seed(2021)
fviz_nbclust(datos.t %>% as.data.frame(), kmodes,
             method = "wss", k.max=10) +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Metodo Elbow") + theme_bw()

# 1.4. Número óptimo de cluster = 3. ----

kmodes <- klaR::kmodes(datos.t, 
             modes = 3, 
             iter.max = 10, 
             weighted = FALSE) 

# str(kmodes)

kmodes$cluster
kmodes$size
kmodes$modes
kmodes$withindiff

# Tamaño de cada cluster
kmodes$size
prop.table(kmodes$size)

grp = kmodes$cluster

cbind(data.final, grp=factor(kmodes$cluster)) -> Data.patrones

data.cl <- datos.t %>%
  bind_cols(grp)

data.cl <- data.cl %>% rename(grp = `...39`)

# 1.5. Validacion k mean: indice de Dunn ----

kdunn <- clValid::dunn(Data= data.cl,
                       clusters=grp, distance = NULL)
kdunn

# 2. CARACTERIZANDO A LOS CLUSTERS ----

# 2.1 Describiendo los clusters usando la función summary() ----
kmodes_results <- Data.patrones %>%
  mutate(cluster = kmodes$cluster) %>%
  group_by(cluster) %>%
  do(resumen = summary(.))
kmodes_results
kmodes_results$resumen

kmodes_results %>% group_by(cluster) %>% unnest(resumen) 

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

datos.kmodes.sn <- data.final %>% 
  dplyr::select(-c(edad,
                sexo,civil,estudios,labor,religion,
                intrusion,evitacion,hiperactivacion,
                ansiedad,depresion,estres,
                F1,F2,SintomasCOVID,
                PHQ9TOTAL,GAD7TOTAL,IESRTOTAL)) %>%
  mutate_if(is.numeric, normalize) %>%
  dplyr::bind_cols(Database%>%dplyr::select(edad,
                                            sexo,civil,estudios,labor,religion,
                                            intrusion,evitacion,hiperactivacion,
                                            ansiedad,depresion,estres,
                                            F1,F2,SintomasCOVID,
                                            PHQ9TOTAL,GAD7TOTAL,IESRTOTAL)) %>%
  bind_cols(grp=as.factor(kmodes$cluster))


datos.kmodes.sn %>% 
  group_by(grp) %>% 
  select_if(is.numeric) %>%
  summarise_all(~list(mean(.,na.rm = TRUE))) -> medias
medias

datos.kmodes.sn %>% 
  select_if(is.numeric) %>%
  summarise_all(~list(mean(.,na.rm = TRUE))) -> general
general
general <- cbind(grp="general",general)
general


medias  <- as.data.frame(rbind(medias,general))

# Convirtiendo la data formato tidy (gather y spread)

gathered_datos.kmodes <- pivot_longer(data  = medias, 
                                  -grp,
                                  names_to = "variable",
                                  values_to = "valor")%>%
  dplyr::filter(!is.na(valor))

gathered_datos.kmodes

# 2.2 Describiendo los clusters usando la función summary() ----
kmodes_results <- data.final %>%
  mutate(cluster = kmodes$cluster) %>%
  group_by(cluster) %>%
  do(resumen = summary(.))
#kmodes_results
kmodes_results$resumen

# 2.3 Diagrama de Cajas de cada variable numérica según Cluster ----
#     usando el paquete ggplot2

ggplot(datos.kmodes.sn) + aes(x = grp, y = edad, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Edad",
       title = "Edad") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g1; g1

ggplot(datos.kmodes.sn) + aes(x = grp, y = intrusion, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Intrusión",
       title = "Intrusión") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g2; g2

ggplot(datos.kmodes.sn) + aes(x = grp, y = evitacion, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Evitación",
       title = "Evitación") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g3; g3

ggplot(datos.kmodes.sn) + aes(x = grp, y = hiperactivacion, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Hiperactivación",
       title = "Hiperactivación") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g4; g4

ggplot(datos.kmodes.sn) + aes(x = grp, y = F1, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "F1",
       title = "F1") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g5; g5

ggplot(datos.kmodes.sn) + aes(x = grp, y = F2, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "F2",
       title = "F2") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g6; g6

ggplot(datos.kmodes.sn) + aes(x = grp, y = SintomasCOVID, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Síntomas COVID",
       title = "Síntomas COVID") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g7; g7

ggplot(datos.kmodes.sn) + aes(x = grp, y = PHQ9TOTAL, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "PHQ9 TOTAL",
       title = "PHQ9 TOTAL") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g8; g8

ggplot(datos.kmodes.sn) + aes(x = grp, y = GAD7TOTAL, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "GAD7 TOTAL",
       title = "GAD7 TOTAL") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g9; g9

ggplot(datos.kmodes.sn) + aes(x = grp, y = IESRTOTAL, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "IESR TOTAL",
       title = "IESR TOTAL") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g10; g10

(g1 + g2) / (g3 + g4) / (g5 + g6 + g7) / (g8 + g9 + g10)


# 2.4 Diagrama de barras por cluster en PHQ -------------

ggplot(datos.kmodes.sn) + aes(PHQ_1, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "PHQ_1", y = "Proporción",
       title = "PHQ_1") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g1; g1

ggplot(datos.kmodes.sn) + aes(x= PHQ_2, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "PHQ_2", y = "Proporción",
       title = "PHQ_2") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g2; g2

ggplot(datos.kmodes.sn) + aes(x= PHQ_3, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "PHQ_3", y = "Proporción",
       title = "PHQ_3") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g3; g3

ggplot(datos.kmodes.sn) + aes(x= PHQ_4, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "PHQ_4", y = "Proporción",
       title = "PHQ_4") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g4; g4


ggplot(datos.kmodes.sn) + aes(x= PHQ_5, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "PHQ_5r", y = "Proporción",
       title = "PHQ_5") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g5; g5

ggplot(datos.kmodes.sn) + aes(x= PHQ_6, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "PHQ_6", y = "Proporción",
       title = "PHQ_6") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g6; g6


ggplot(datos.kmodes.sn) + aes(x= PHQ_7, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "PHQ_7", y = "Proporción",
       title = "PHQ_7") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g7; g7


ggplot(datos.kmodes.sn) + aes(x= PHQ_8, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "PHQ_8r", y = "Proporción",
       title = "PHQ_8") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g8; g8

ggplot(datos.kmodes.sn) + aes(x= PHQ_9, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "PHQ_9", y = "Proporción",
       title = "PHQ_9") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g9; g9

(g1 + g2 + g3) / (g4 + g5 + g6) / (g7 + g8 + g9)

# 2.5 Diagrama de barras por cluster en GAD -------------

ggplot(datos.kmodes.sn) + aes(GAD_1, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "GAD_1", y = "Proporción",
       title = "GAD_1") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g1; g1

ggplot(datos.kmodes.sn) + aes(GAD_2, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "GAD_2", y = "Proporción",
       title = "GAD_2") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g2; g2

ggplot(datos.kmodes.sn) + aes(GAD_3, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "GAD_3r", y = "Proporción",
       title = "GAD_3") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g3; g3

ggplot(datos.kmodes.sn) + aes(GAD_4, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "GAD_4", y = "Proporción",
       title = "GAD_4") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g4; g4


ggplot(datos.kmodes.sn) + aes(GAD_5, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "GAD_5", y = "Proporción",
       title = "GAD_5") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g5; g5

ggplot(datos.kmodes.sn) + aes(GAD_6, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "GAD_6", y = "Proporción",
       title = "GAD_6") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g6; g6


ggplot(datos.kmodes.sn) + aes(GAD_7, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "GAD_7", y = "Proporción",
       title = "GAD_7") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g7; g7

(g1 + g2 + g3) / (g4 + g5) / (g6 + g7)

# 2.6 Diagrama de barras por cluster en IES -------------

ggplot(datos.kmodes.sn) + aes(IES_1, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_1", y = "Proporción",
       title = "IES_1") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g1; g1

ggplot(datos.kmodes.sn) + aes(IES_2, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_2", y = "Proporción",
       title = "IES_2") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g2; g2

ggplot(datos.kmodes.sn) + aes(IES_3, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_3", y = "Proporción",
       title = "IES_3") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g3; g3

ggplot(datos.kmodes.sn) + aes(IES_4, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_4", y = "Proporción",
       title = "IES_4") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g4; g4


ggplot(datos.kmodes.sn) + aes(IES_5, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_5", y = "Proporción",
       title = "IES_5") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g5; g5

ggplot(datos.kmodes.sn) + aes(IES_6, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_6", y = "Proporción",
       title = "IES_6") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g6; g6


ggplot(datos.kmodes.sn) + aes(IES_7, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_7", y = "Proporción",
       title = "IES_7") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g7; g7


ggplot(datos.kmodes.sn) + aes(IES_8, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_8", y = "Proporción",
       title = "IES_8") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g8; g8

ggplot(datos.kmodes.sn) + aes(IES_9, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_9", y = "Proporción",
       title = "IES_9") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g9; g9

ggplot(datos.kmodes.sn) + aes(IES_10, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_10", y = "Proporción",
       title = "IES_10") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g10; g10


ggplot(datos.kmodes.sn) + aes(IES_11, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_11", y = "Proporción",
       title = "IES_11") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g11; g11

ggplot(datos.kmodes.sn) + aes(IES_12, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_12", y = "Proporción",
       title = "IES_12") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g12; g12

ggplot(datos.kmodes.sn) + aes(IES_13, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_13", y = "Proporción",
       title = "IES_13") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g13; g13

ggplot(datos.kmodes.sn) + aes(IES_14, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_14", y = "Proporción",
       title = "IES_14") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g14; g14

ggplot(datos.kmodes.sn) + aes(IES_15, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_15", y = "Proporción",
       title = "IES_15") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g15; g15

ggplot(datos.kmodes.sn) + aes(IES_16, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_16", y = "Proporción",
       title = "IES_16") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g16; g16


ggplot(datos.kmodes.sn) + aes(IES_17, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_17", y = "Proporción",
       title = "IES_17") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g17; g17

ggplot(datos.kmodes.sn) + aes(IES_18, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_18", y = "Proporción",
       title = "IES_18") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g18; g18


ggplot(datos.kmodes.sn) + aes(IES_19, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_19", y = "Proporción",
       title = "IES_19") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g19; g19


ggplot(datos.kmodes.sn) + aes(IES_20, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_20", y = "Proporción",
       title = "IES_20") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g20; g20

ggplot(datos.kmodes.sn) + aes(IES_21, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_21", y = "Proporción",
       title = "IES_21") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g21; g21

ggplot(datos.kmodes.sn) + aes(IES_22, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "IES_22", y = "Proporción",
       title = "IES_22") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g22; g22

(g1 + g2 + g3) / (g4 + g5 + g6) / (g7 + g8 + g9) / (g10 + g11 + g12)

(g13 + g14 + g15) / (g16 + g17 + g18) / (g19 + g20) / (g21 + g22)

# 2.7 Gráficos apilados para las variables categóricas --------
ggplot(datos.kmodes.sn) + aes(sexo, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Sexo") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g1; g1

ggplot(datos.kmodes.sn) + aes(civil, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Civil") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g2; g2

ggplot(datos.kmodes.sn) + aes(estudios, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Estudios") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")-> g3; g3

ggplot(datos.kmodes.sn) + aes(labor, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Labor") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")-> g4; g4

ggplot(datos.kmodes.sn) + aes(religion, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Religión") +
  theme_elegante() +
  theme(axis.text.x=element_text(angle=0),
        legend.position = "right") -> g5; g5

ggplot(datos.kmodes.sn) + aes(ansiedad, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Ansiedad") +
  theme_elegante() +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g6; g6

ggplot(datos.kmodes.sn) + aes(depresion, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Depresión") +
  theme_elegante() +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g7; g7

ggplot(datos.kmodes.sn) + aes(grp, fill = grp) + geom_bar() + 
  labs(x = "Cluster", y = "Frecuencia",
       title = "Cluster") +
  theme_elegante() +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "none") -> g8 ; g8

(g1 + g5 + g3) / (g4 + g2) / (g6 + g7) / (g8)

###############
# II. K-MEANS #
###############

# II. CLUSTER K-MEANS ----------------------------------------

data.clust<-Database%>%dplyr::select(-PHQ9TOTAL,-GAD7TOTAL,-IESRTOTAL,-edad,
                                     -sexo,-civil,-estudios,-labor,-religion,
                                     -intrusion,-evitacion,-hiperactivacion,
                                     -IESRTOTAL,-ansiedad,-depresion,-estres,
                                     -F1,-F2,-SintomasCOVID) 

data.final <- data.clust %>%
  dplyr::bind_cols(Database%>%dplyr::select(edad,
                                            sexo,civil,estudios,labor,religion,
                                            intrusion,evitacion,hiperactivacion,
                                            ansiedad,depresion,estres,
                                            F1,F2,SintomasCOVID,
                                            PHQ9TOTAL,GAD7TOTAL,IESRTOTAL))

data <- as.data.frame(normalize(data.clust))

# 1. Encuentre el número óptimo de clusters para un k-means ----

# 1.1. Criterio de la suma de cuadrados ----
RNGkind(sample.kind = "Rounding")
set.seed(2021)
fviz_nbclust(data, kmeans, method = "wss", k.max=10) +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "criterio de la suma de cuadrados") + theme_bw()


# 1.2. Criterio del Grafico de Silueta  ----

RNGkind(sample.kind = "Rounding")
set.seed(2021)
fviz_nbclust(data, kmeans, method = "silhouette", k.max=10) +
  labs(subtitle = "Silhouette method")


# 1.3. Criterio del Gap Statistics ----
RNGkind(sample.kind = "Rounding")
set.seed(2021)
fviz_nbclust(data, kmeans, nstart = 25, k.max=15,
             method = "gap_stat",nboot = 20) +
  labs(subtitle = "Gap statistic method")

# 1.4. Eleccion del K optimo - kmeans ----
RNGkind(sample.kind = "Rounding")
set.seed(2021)
fviz_nbclust(data.clust, kmeans, method = "wss", k.max=10) +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Metodo Elbow") + theme_bw()

# 1.5. Usando la funcion kmeans() con 3 clusters ----
RNGkind(sample.kind = "Rounding")
set.seed(2021)
km <- kmeans(data, 
             centers=3,      # Numero de Cluster
             algorithm = "Hartigan-Wong",
             trace = FALSE,
             iter.max = 100, # Numero de iteraciones maxima
             nstart = 25);km    # Numero de puntos iniciales

# Tamaño de cada cluster
km$size
prop.table(km$size)

# promedios de cada cluster
aggregate(data, by=list(cluster=km$cluster), mean)

# Se visualiza cada elemento con su respectivo cluster
broom::augment(km, data) %>% View()

# Con el comando tidy podemos ver los principales indicadores por cluster,
# promedio por cluster y variable, el tamanio y la suma de cuadrados.
broom::tidy(km)
broom::glance(km)

# 2. Validacion k mean. ----

# 2.1. Bootstrap ----

kclusters <- fpc::clusterboot(data,
                              B = 100, # Number of resampling runs for each scheme
                              clustermethod = fpc::kmeansCBI,
                              k = 3,
                              seed = 2021)
kclusters$bootmean

# 2.2. Validacion k mean: indice de Dunn ----

data %>% mutate(grp=km$cluster) -> datos.k
grupo <- as.integer(datos.k$grp)
grupo
kdunn <- clValid::dunn(Data= data, clusters=grupo, distance = NULL)
kdunn

# 2.3. Usando el Criterio del estadístico Hopkins --------------

# Estadítico H

hopkins(data = data.clust, n = nrow(data.clust)-1) 

# 3. Análisis de Componentes Principales ----

acp <- dudi.pca(data.clust,scannf=FALSE,nf=ncol(data.clust))
summary(acp)

# Valores propios
acp$eig

inertia.dudi(acp)

# Correlaciones entre las variables y los componentes
acp$co[c(1:5)]

# Gráfica de Valores propios - ScreePlot
fviz_eig(acp, addlabels=TRUE, hjust = -0.3,
         barfill="white", barcolor ="darkblue",
         linecolor ="red") + ylim(0,80) + theme_minimal()

# Scores o Puntuaciones de cada individuo
acp$li[1:10,1:2]

# Visualización de cluster por PCA

fviz_cluster(km, data = data.clust, ellipse.type = "convex") +
  theme_elegante()

fviz_cluster(km, data = data.clust, ellipse.type = "convex",
             axes = c(2,3)) +
  theme_elegante()

pca <- prcomp(data)

gr <- factor(broom::augment(km, data) %>% 
               dplyr::select(.cluster) %>% unlist)
summary(gr)

pca3d::pca3d(pca, group=gr)

# 4. Visualización de los centroides de 1 a 10 clusters ----

kclusts <- 
  tibble(k = 1:10) %>%
  mutate(
    kclust = purrr::map(k, ~kmeans(data, .x)),
    tidied = purrr::map(kclust, broom::tidy),
    glanced = purrr::map(kclust, broom::glance),
    augmented = purrr::map(kclust, broom::augment, data)
  )
kclusts

# Separacion por cada indicador de cluster para cada valor de K
clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

# Separacion de los individuos por cada cluster para cada valor de K
assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

# indicador total para cada valor de K
clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

# Grafico mostrando como se distribuyen los diferentes cluster para cada valor de K
p1 <- 
  ggplot(assignments, aes(x = PHQ_1, y = PHQ_2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

# Se adiciona el centroide de cada cluster para cada valor de K
p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2

# De los graficos anteriores es ahora muy claro el que K tome el valor de 3
# Valor que sera confirmado por el metodo de Elbow.

# Se validara usando el Índice de Validación de Davies-Bouldin y el Índice de Dunn

set.seed(2021)
db <- numeric()
dunn <- numeric()
for (h in 2:6){
  b          <- kmeans(data,h)
  grupo      <- b$cluster
  indiceDB   <- clusterSim::index.DB(data, grupo, centrotypes = "centroids")
  db[h]      <- indiceDB$DB 
  indiceDUNN <- clValid::dunn(Data = data, clusters = grupo, distance = NULL)
  dunn[h]    <- indiceDUNN
  
}
db
dunn
indices  <- data.frame(cluster = c(2:6),
                       Indice_DB = db[2:6],
                       Indice_Dunn = dunn[2:6])
indices

# Valores medios por cluster
aggregate(data.final, by=list(km$cluster), mean)

# 5. Importancia de las variables ----

cl <- flexclust::as.kcca(km,data)
x11()
barplot(cl)

Importancia <- FeatureImpCluster::FeatureImpCluster(cl, as.data.table(data))
plot(Importancia)

# de acuerdo al grafico generado las variables menos importantes son PHQ9 y PHQ8

datos.km <- data.clust %>%
  dplyr::bind_cols(Database%>%dplyr::select(edad,
                                            sexo,civil,estudios,labor,religion,
                                            intrusion,evitacion,hiperactivacion,
                                            ansiedad,depresion,estres,
                                            F1,F2,SintomasCOVID,
                                            PHQ9TOTAL,GAD7TOTAL,IESRTOTAL)) %>%
  mutate_if(is.numeric, normalize) %>%
  bind_cols(grp=as.factor(km$cluster))

datos.km.sn <- data.clust %>%
  mutate_if(is.numeric, normalize) %>%
  dplyr::bind_cols(Database%>%dplyr::select(edad,
                                            sexo,civil,estudios,labor,religion,
                                            intrusion,evitacion,hiperactivacion,
                                            ansiedad,depresion,estres,
                                            F1,F2,SintomasCOVID,
                                            PHQ9TOTAL,GAD7TOTAL,IESRTOTAL)) %>%
  bind_cols(grp=as.factor(km$cluster))


datos.km.sn %>% 
  select_if(is.numeric) %>%
  bind_cols(grp=as.factor(km$cluster)) %>%
  group_by(grp) %>% 
  summarise_all(list(mean)) -> medias
medias

datos.km.sn %>%  summarise_if(is.numeric,mean) -> general
general
general <- cbind(grp="general",general)
general

medias  <- as.data.frame(rbind(medias,general))

# Convirtiendo la data formato tidy (gather y spread)

gathered_datos.km <- pivot_longer(data  = medias, 
                                   -grp,
                                   names_to = "variable",
                                   values_to = "valor")%>%
  dplyr::filter(!is.na(valor))

gathered_datos.km

# 6. CARACTERIZANDO A LOS CLUSTERS ----

# 6.1 Describiendo los clusters usando la función summary() ----
km_results <- data.final %>%
  mutate(cluster = km$cluster) %>%
  group_by(cluster) %>%
  do(resumen = summary(.))
#km_results
km_results$resumen


# 6.2 Diagrama de Cajas de cada variable numérica según Cluster ----
#     usando el paquete ggplot2

ggplot(datos.km.sn) + aes(x = grp, y = edad, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Edad",
       title = "Edad") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g1; g1

ggplot(datos.km.sn) + aes(x = grp, y = intrusion, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Intrusión",
       title = "Intrusión") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g2; g2

ggplot(datos.km.sn) + aes(x = grp, y = evitacion, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Evitación",
       title = "Evitación") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g3; g3

ggplot(datos.km.sn) + aes(x = grp, y = hiperactivacion, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Hiperactivación",
       title = "Hiperactivación") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g4; g4

ggplot(datos.km.sn) + aes(x = grp, y = F1, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "F1",
       title = "F1") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g5; g5

ggplot(datos.km.sn) + aes(x = grp, y = F2, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "F2",
       title = "F2") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g6; g6

ggplot(datos.km.sn) + aes(x = grp, y = SintomasCOVID, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Síntomas COVID",
       title = "Síntomas COVID") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g7; g7

ggplot(datos.km.sn) + aes(x = grp, y = PHQ9TOTAL, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "PHQ9 TOTAL",
       title = "PHQ9 TOTAL") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g8; g8

ggplot(datos.km.sn) + aes(x = grp, y = GAD7TOTAL, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "GAD7 TOTAL",
       title = "GAD7 TOTAL") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g9; g9

ggplot(datos.km.sn) + aes(x = grp, y = IESRTOTAL, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "IESR TOTAL",
       title = "IESR TOTAL") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g10; g10

(g1 + g2) / (g3 + g4) / (g5 + g6 + g7) / (g8 + g9 + g10)


# 6.3 Diagrama de líneas de promedios por cluster -------------

gathered_datos.km %>% 
  dplyr::filter(!variable %in% c("edad","evitacion","hiperactivacion",
                                 "intrusion","GAD7TOTAL","IESRTOTAL",
                                 "PHQ9TOTAL")) %>%
ggplot() + aes(x=variable,y=valor,color=grp) + 
  geom_point() + geom_line(aes(group = grp)) +
  scale_y_continuous() +
  theme_elegante() +
  theme(legend.position = "bottom") +
  labs(title="Diagrama de líneas de cluster por variable - método K-means",
       x="Variable") +
  scale_colour_discrete("Cluster")

# 6.4 Gráficos apilados para las variables categóricas --------
ggplot(datos.km) + aes(sexo, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Sexo") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g1; g1

ggplot(datos.km) + aes(civil, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Civil") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g2; g2

ggplot(datos.km) + aes(estudios, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Estudios") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")-> g3; g3

ggplot(datos.km) + aes(labor, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Labor") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")-> g4; g4

ggplot(datos.km) + aes(religion, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Religión") +
  theme_elegante() +
  theme(axis.text.x=element_text(angle=0),
        legend.position = "right") -> g5; g5

ggplot(datos.km) + aes(ansiedad, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Ansiedad") +
  theme_elegante() +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g6; g6

ggplot(datos.km) + aes(depresion, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Depresión") +
  theme_elegante() +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g7; g7

ggplot(datos.km) + aes(grp, fill = grp) + geom_bar() + 
  labs(x = "Cluster", y = "Frecuencia",
       title = "Cluster") +
  theme_elegante() +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "none") -> g8 ; g8

(g1 + g5 + g3) / (g4 + g2) / (g6 + g7) / (g8)

################
# III. X-MEANS #
################

# III. CLUSTER X-MEANS ----------------------------------------

### 1. X-means con el paquete RWeka ----

WPM("refresh-cache") # Build Weka package metadata cache
WPM("install-package", "XMeans") # Install XMeans package if not previously installed

weka_ctrl <- Weka_control( # Create a Weka control object to specify our parameters
  I = 100, # max no iterations overall
  M = 100, # max no iterations in the kmeans loop
  L = 2,   # min no clusters
  H = 5,   # max no clusters
  D = "weka.core.EuclideanDistance", # distance metric
  C = 0.4, S = 1)
x_means <- XMeans(data, control = weka_ctrl) # run algorithm on data
x_means

### 2. X-means con el paquete mlr3 ----

# 2.1. Guardar nueva data en el diccionario ----

data.xmeans <- data

task_data <- TaskUnsupervised$new("data.xmeans", task_type = "clust", backend = data.xmeans)

mlr_tasks
mlr_tasks$add("data.xmeans",task_data)
learner = mlr_learners$get("clust.xmeans")
learner$train(task_data)
preds = learner$predict(task = task_data)
preds

# mlr_measures$keys("clust")

# 2.2. Hyperparametros ----

learner$param_set

learner$param_set$values = list(I = 100,
                                M = 100, # max no iterations in the kmeans loop
                                L = 2,   # min no clusters
                                H = 5,   # max no clusters
                                D = "weka.core.EuclideanDistance", # distance metric
                                C = 0.4,
                                S = 10)

# 2.3. Entrenamiento y predicción ----

train_set = sample(task_data$nrow, 0.8 * task_data$nrow)
test_set = setdiff(seq_len(task_data$nrow), train_set)

learner = mlr_learners$get("clust.xmeans")
learner$train(task_data, row_ids = train_set)

preds = learner$predict(task_data, row_ids = test_set)
preds

# 2.4. Benchmarking y Evaluación ----

# Diseñar un experimento especificando por task(s), learner(s), resampling method(s)
design = benchmark_grid(
  tasks = task_data,
  learners = list(
    lrn("clust.kmeans", centers = 3L),
    lrn("clust.xmeans"),
    lrn("clust.cmeans", centers = 3L)),
  resamplings = rsmp("holdout"))
print(design)

# Ejecutar benchmark ----

bmr = benchmark(design)

# Define métrica ----

measures = list(msr("clust.silhouette"))

bmr$aggregate(measures)

# 2.5. Definir lista de parámetros. ----

task = mlr_tasks$get("data.xmeans")
mlr_learners
learner = mlr_learners$get("clust.xmeans")
learner$param_set$values = list(I = 100, # max no iterations overall
                                M = 100, # max no iterations in the kmeans loop
                                #L = 2,   # min no clusters
                                #H = 5,   # max no clusters
                                D = "weka.core.EuclideanDistance", # distance metric
                                C = 0.4, S = 10)
learner$train(task)
preds = learner$predict(task)
preds$partition


# 3. CARACTERIZANDO A LOS CLUSTERS ----

grp <- preds$partition

datos.xm <- data.clust %>%
  dplyr::bind_cols(Database%>%
                     dplyr::select(edad,
                                   sexo,civil,estudios,labor,religion,
                                   intrusion,evitacion,hiperactivacion,
                                   ansiedad,depresion,estres,
                                   F1,F2,SintomasCOVID,
                                   PHQ9TOTAL,GAD7TOTAL,IESRTOTAL)) %>%
  mutate_if(is.numeric, normalize) %>%
  bind_cols(grp=as.factor(grp))

datos.xm.sn <- data.clust %>%
  mutate_if(is.numeric, normalize) %>%
  dplyr::bind_cols(Database%>%dplyr::select(edad,
                                            sexo,civil,estudios,labor,religion,
                                            intrusion,evitacion,hiperactivacion,
                                            ansiedad,depresion,estres,
                                            F1,F2,SintomasCOVID,
                                            PHQ9TOTAL,GAD7TOTAL,IESRTOTAL)) %>%
  bind_cols(grp=as.factor(grp))


datos.xm.sn %>% 
  select_if(is.numeric) %>%
  bind_cols(grp=as.factor(grp)) %>%
  group_by(grp) %>% 
  summarise_all(list(mean)) -> medias
medias

datos.xm.sn %>%  summarise_if(is.numeric,mean) -> general
general
general <- cbind(grp="general",general)
general


medias  <- as.data.frame(rbind(medias,general))

# Convirtiendo la data formato tidy (gather y spread)

gathered_datos.xm <- pivot_longer(data  = medias, 
                                  -grp,
                                  names_to = "variable",
                                  values_to = "valor")%>%
  dplyr::filter(!is.na(valor))

gathered_datos.xm

# 3.1 Describiendo los clusters usando la función summary() ----
xm_results <- data.final %>%
  mutate(cluster = grp) %>%
  group_by(cluster) %>%
  do(resumen = summary(.))
xm_results$resumen

# 3.2 Diagrama de Cajas de cada variable numérica según Cluster ----
#     usando el paquete ggplot2

ggplot(datos.xm.sn) + aes(x = grp, y = edad, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Edad",
       title = "Edad") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g1; g1

ggplot(datos.xm.sn) + aes(x = grp, y = intrusion, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Intrusión",
       title = "Intrusión") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g2; g2

ggplot(datos.xm.sn) + aes(x = grp, y = evitacion, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Evitación",
       title = "Evitación") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g3; g3

ggplot(datos.xm.sn) + aes(x = grp, y = hiperactivacion, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Hiperactivación",
       title = "Hiperactivación") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g4; g4

ggplot(datos.xm.sn) + aes(x = grp, y = F1, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "F1",
       title = "F1") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g5; g5

ggplot(datos.xm.sn) + aes(x = grp, y = F2, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "F2",
       title = "F2") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g6; g6

ggplot(datos.xm.sn) + aes(x = grp, y = SintomasCOVID, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "Síntomas COVID",
       title = "Síntomas COVID") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g7; g7

ggplot(datos.xm.sn) + aes(x = grp, y = PHQ9TOTAL, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "PHQ9 TOTAL",
       title = "PHQ9 TOTAL") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g8; g8

ggplot(datos.xm.sn) + aes(x = grp, y = GAD7TOTAL, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "GAD7 TOTAL",
       title = "GAD7 TOTAL") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g9; g9

ggplot(datos.xm.sn) + aes(x = grp, y = IESRTOTAL, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "IESR TOTAL",
       title = "IESR TOTAL") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g10; g10

(g1 + g2) / (g3 + g4) / (g5 + g6 + g7) / (g8 + g9 + g10)


# 3.3 Diagrama de líneas de promedios por cluster -------------

gathered_datos.xm %>% 
  dplyr::filter(!variable %in% c("edad","evitacion","hiperactivacion",
                                 "intrusion","GAD7TOTAL","IESRTOTAL",
                                 "PHQ9TOTAL"),
                !grp %in% "general") %>%
  ggplot() + aes(x=variable,y=valor,color=grp) + 
  geom_point() + geom_line(aes(group = grp)) +
  scale_y_continuous() +
  theme_elegante() +
  theme(legend.position = "bottom") +
  labs(title="Diagrama de líneas de cluster por variable - método X-means",
       x="Variable") +
  scale_colour_discrete("Cluster")

# 3.4 Gráficos apilados para las variables categóricas --------
ggplot(datos.xm) + aes(sexo, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Sexo") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g1; g1

ggplot(datos.xm) + aes(civil, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Civil") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g2; g2

ggplot(datos.xm) + aes(estudios, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Estudios") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")-> g3; g3

ggplot(datos.xm) + aes(labor, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Labor") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")-> g4; g4

ggplot(datos.xm) + aes(religion, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Religión") +
  theme_elegante() +
  theme(axis.text.x=element_text(angle=0),
        legend.position = "right") -> g5; g5

ggplot(datos.xm) + aes(ansiedad, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Ansiedad") +
  theme_elegante() +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g6; g6

ggplot(datos.xm) + aes(depresion, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Cluster", y = "Proporción",
       title = "Depresión") +
  theme_elegante() +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g7; g7

ggplot(datos.xm) + aes(grp, fill = grp) + geom_bar() + 
  labs(x = "Cluster", y = "Frecuencia",
       title = "Cluster") +
  theme_elegante() +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "none") -> g8 ; g8

(g1 + g5 + g3) / (g4 + g2) / (g6 + g7) / (g8)

