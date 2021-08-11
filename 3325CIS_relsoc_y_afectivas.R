library(haven)
library(tidyverse)
library(moderndive)
library(nortest)
library(hrbrthemes)


data <- read_sav("3325.sav")

glimpse(data)

data <- data %>% 
  mutate(EDAD_REC = (EDAD*1))
data$EDAD_REC


data$EDAD_REC[data$EDAD_REC < 25] <- 1
data$EDAD_REC[data$EDAD_REC >= 25 & data$EDAD_REC <= 34] <- 2
data$EDAD_REC[data$EDAD_REC >= 35 & data$EDAD_REC <= 44] <- 3
data$EDAD_REC[data$EDAD_REC >= 45 & data$EDAD_REC <= 54] <- 4
data$EDAD_REC[data$EDAD_REC >= 55 & data$EDAD_REC <= 64] <- 5
data$EDAD_REC[data$EDAD_REC >= 65] <- 6

data$EDAD_REC <- as.factor(data$EDAD_REC)
table(data$EDAD_REC)

data <- data %>% 
  select(P1_1:P1_3, CLASESUB,SITLAB, TAMUNI, P4_1, EDAD, EDAD_REC, P20)



data <- data %>% 
  filter(!(P1_1 >96 | P1_2 >96 | P1_3 >96 | P4_1 >96 | P20 >97))
data


#------ TAMAÑO MUNICIPIO * AYUDA VECINO --------


bartlett.test(P4_1 ~ TAMUNI, 
              data = data ) # mayor de 0.05, no podemos rechazar la hipótesis nula. Por lo tanto suponemos homogeneidad de varianzas.

modelo1 <- aov(P4_1 ~ TAMUNI,
               data = data) # menor de 0.05 hay diferencia en al menos una de las medias de los grupos de edad


summary(modelo1)

pairwise.t.test(data$P4_1,data$TAMUNI, 
                p.adj = "bonferroni")



library(showtext)
font_add_google("Poppins", "Poppins")
font_add_google("Roboto Mono", "Roboto Mono")
showtext_auto()

p1 <-
  ggplot(data, aes(x = as.factor(TAMUNI), y = P4_1)) +
  scale_y_continuous(name="Confianza en que el vecino le ayudaría en caso de necesitarlo", limits=c(1, 10), breaks = seq(0, 10, by = 1))+
  scale_x_discrete(name = "Nº de habitantes del municipio", labels=c("1" = "<=2.000", 
                                                                     "2" = "2.001 a 10.000",
                                                                     "3" = "10.001 a 50.000",
                                                                     "4" = "50.001 a 100.000",
                                                                     "5" = "100.001 a 400.000",
                                                                     "6" = "400.001 a 1.000.000",
                                                                     "7" = ">1.000.000")) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()
  )

p1

p1 + geom_jitter(size = 3, alpha = 0.15)

p1 +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_jitter(size = 3, alpha = 0.15)

set.seed(2019)

p1 + geom_jitter(size = 2, alpha = 0.25, width = 0.2)


p1 + geom_jitter(position = position_jitter(seed = 2019, width = 0.2), size = 2, alpha = 0.25)


p1 +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 5)


p1 <- p1 +
  geom_hline(aes(yintercept = mean(data$P4_1)), color = "yellow", size = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 5, color = "red") +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2, height = 0.2) + 
  annotate(
    "text", x = 1.5, y = 6.70, family = "Poppins", size = 0.8, color = "yellow",
    label = "") +
  annotate(
    "text", x = 6.25, y = 6.89, family = "Poppins", size = 0.8, color = "red",
    label = "") +
  theme_modern_rc() +
  theme(axis.title.x = element_text(hjust = 0.5, size = 10, color = "darkorange"),
        axis.title.y = element_text(hjust = 0.5, size = 10, color = "darkorange"), 
        plot.title = element_text(hjust = 0.5, size = 8, color = "darkorange"),
        axis.text.x = element_text(size = 6.25, color = "darkorange"),
        axis.text.y = element_text(size = 10, color = "darkorange"),
        legend.position = "none")
p1

mean(data$P4_1)



#---- BOX PLOT EDAD * AYUDA VECINO -------


data$EDAD_REC <- as.numeric(data$EDAD_REC)

bartlett.test(P4_1 ~ EDAD_REC, 
              data = data ) # mayor de 0.05, no podemos rechazar la hipótesis nula. Por lo tanto suponemos homogeneidad de varianzas.

modelo1 <- aov(P4_1 ~ EDAD_REC, 
               data = data) # menor de 0.05 hay diferencia en al menos una de las medias de los grupos de edad

summary(modelo1)

pairwise.t.test(data$P4_1,data$EDAD_REC, 
                p.adj = "bonferroni")



p3 <-
  ggplot(data, aes(x = as.factor(EDAD_REC), y = P4_1)) +
  scale_y_continuous(name="Confianza en que el vecino le ayudaría en caso de necesitarlo", limits=c(1, 10), breaks = seq(0, 10, by = 1))+
  scale_x_discrete(name = "Franja etaria del entrevistado", labels=c("1" = "18-24", 
                                                                    "2" = "25-34",
                                                                    "3" = "35-44",
                                                                    "4" = "45-54",
                                                                    "5" = "55-64",
                                                                    "6" = ">=65")) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()
  )

p3

p3 + geom_jitter(size = 3, alpha = 0.15)

p3 +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_jitter(size = 3, alpha = 0.15)

set.seed(2019)

p3 + geom_jitter(size = 2, alpha = 0.25, width = 0.2)


p3 + geom_jitter(position = position_jitter(seed = 2019, width = 0.2), size = 2, alpha = 0.25)


p3 +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 5)


p3 <- p3 +
  geom_hline(aes(yintercept = mean(data$P4_1)), color = "yellow", size = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 5, color = "red") +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2, height = 0.2) + 
  annotate(
    "text", x = 4.5, y = 6.20, family = "Poppins", size = 2.3, color = "yellow",
    label = "") +
  annotate(
    "text", x = 1.25, y = 3.89, family = "Poppins", size = 2.3, color = "red",
    label = "") +
  theme_modern_rc() +
  theme(axis.title.x = element_text(hjust = 0.5, size = 10, color = "darkorange"),
        axis.title.y = element_text(hjust = 0.5, size = 10, color = "darkorange"), 
        plot.title = element_text(hjust = 0.5, size = 8, color = "darkorange"),
        axis.text.x = element_text(size = 10, color = "darkorange"),
        axis.text.y = element_text(size = 10, color = "darkorange"),
        legend.position = "none")
p3


mean(data$P4_1)

#----arrange AYUDA DEL VECINO------
library(ggpubr)


title <- expression(atop(bold("Confianza en que el vecino te ayudaría en caso de necesitarlo"), 
                         scriptstyle("En núcleos grandes de población y jóvenes están por debajo de la media")))

figure <- ggarrange(
  p1, p3,
   nrow = 1, common.legend = TRUE, legend="top")


figure<- annotate_figure(figure,
                         top = text_grob(title,
                                         color = "green", face = "bold", size = 16),
                         bottom = text_grob("CIS Estudio nº 3325 | @dataR_amateur",
                                            hjust = 1, x = 1, face = "italic", size = 7,
                                            color = "white")) +
  
  theme_modern_rc()
figure

figure + ggsave("ayuda_vecino.png", width = 13, height = 8.5, dpi = 500)



# ---- PERFIL DE LOS QUE NO MANTIENEN NINGÚN TIPO DE RELACIÓN SEXUAL NI SENTIMENTAL CON NADIE -----

data <- read_sav("3325.sav")

glimpse(data)

data <- data %>% 
  mutate(EDAD_REC = (EDAD*1))
data$EDAD_REC


data$EDAD_REC[data$EDAD_REC < 25] <- 1
data$EDAD_REC[data$EDAD_REC >= 25 & data$EDAD_REC <= 34] <- 2
data$EDAD_REC[data$EDAD_REC >= 35 & data$EDAD_REC <= 44] <- 3
data$EDAD_REC[data$EDAD_REC >= 45 & data$EDAD_REC <= 54] <- 4
data$EDAD_REC[data$EDAD_REC >= 55 & data$EDAD_REC <= 64] <- 5
data$EDAD_REC[data$EDAD_REC >= 65] <- 6
data$P15[data$P15 == 1] <- 1
data$P15[data$P15 >= 2 & data$EDAD_REC <= 6] <- 0
data$P15[data$P15 >7] <- NULL


data$EDAD_REC <- as.factor(data$EDAD_REC)
table(data$EDAD_REC)

data <- data %>% 
  select(P1_1:P1_3, CLASESUB,SITLAB, TAMUNI, P4_1, EDAD, EDAD_REC, P20, SITCONVIVEN, P15)



data <- data %>% 
  filter(!(P1_1 >96 | P1_2 >96 | P1_3 >96 | P4_1 >96 | P20 >97 | P15>98))
data

summary(data$P15)


#situación sentimental vs escala de felicidad

p8 <-
  ggplot(data, aes(x = as.factor(P15), y = P20)) +
  scale_y_continuous(name="Escala de felicidad del entrevistado", limits=c(1, 10), breaks = seq(0, 10, by = 1))+
  scale_x_discrete(name = "Situación sentimental y sexual",  labels=c("0" = "Mantiene", 
                                                                      "1" = "No mantiene")) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()
  )

p8

p8 + geom_jitter(size = 3, alpha = 0.15)

p8 +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_jitter(size = 3, alpha = 0.15)

set.seed(2019)

p8 + geom_jitter(size = 2, alpha = 0.25, width = 0.2)


p8 + geom_jitter(position = position_jitter(seed = 2019, width = 0.2), size = 2, alpha = 0.25)


p8 +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 5)


p8 <- p8 +
  geom_hline(aes(yintercept = mean(data$P20)), color = "yellow", size = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 5, color = "red") +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2, height = 0.2) + 
  annotate(
    "text", x = 1.5, y = 8.70, family = "Poppins", size = 2.8, color = "yellow",
    label = "") +
  annotate(
    "text", x = 2.25, y = 7.89, family = "Poppins", size = 2.8, color = "purple",
    label = "") +
  theme_modern_rc() +
  theme(axis.title.x = element_text(hjust = 0.5, size = 10, color = "darkorange"),
        axis.title.y = element_text(hjust = 0.5, size = 10, color = "darkorange"), 
        plot.title = element_text(hjust = 0.5, size = 8, color = "darkorange"),
        axis.text.x = element_text(size = 10, color = "darkorange"),
        axis.text.y = element_text(size = 10, color = "darkorange")) 

p8




# PLOT reg logistica situacion sentimental * felicidad

logit <- glm(P15 ~ P20,
             data = data,
             family = "binomial")

summary(logit)


coef(logit) %>%
  as_tibble() %>%
  mutate(odds = exp(value))

coef(logit) %>%
  as_tibble() %>%
  mutate(
    odds = exp(value),
    prob = plogis(value)
  )


xfelicidad <- 1:10
y_logito <- predict(logit, list(P20 = xfelicidad))
y_prob<- predict(logit, list(P20 = xfelicidad), type= "response")

results_m0<-cbind(y_logito, y_prob, xfelicidad)
results_m0<-as.data.frame(results_m0)

p9 <- ggplot(data=results_m0, aes(x=xfelicidad, y=y_prob)) +
  geom_line(color = "red") +
  labs(title = "") +
  scale_y_continuous(name="Probabilidad de no tener relaciones sexuales ni afectivas con nadie", limits=c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(name = "Escala de felicidad", limits=c(1, 10), breaks = seq(1, 10, by = 1)) +
  theme_modern_rc() +
  theme(axis.title.x = element_text(hjust = 0.5, size = 10, color = "darkorange"),
        axis.title.y = element_text(hjust = 0.5, size = 10, color = "darkorange"), 
        plot.title = element_text(hjust = 0.5, size = 8, color = "darkorange"),
        axis.text.x = element_text(size = 10, color = "darkorange"),
        axis.text.y = element_text(size = 10, color = "darkorange")) 

p9



figure <- ggarrange(
  p8, p9, 
  ncol = 2, common.legend = TRUE, legend="top")


figure<- annotate_figure(figure,
                         top = text_grob("Diferencia de medias en mantiene relaciones sexuales o afectivas con alguien vs no mantiene por Escala de felicidad",
                                         color = "green", face = "bold", size = 16),
                         bottom = text_grob("CIS Estudio nº 3325 | @dataR_amateur",
                                            hjust = 1, x = 1, face = "italic", size = 7,
                                            color = "white")) +
  theme_modern_rc()
figure

figure + ggsave("afectivas_felicidad.png", width = 13, height = 8.5, dpi = 500)

mean(data$P20)





#--------- Se pueden tener relaciones sexuales con alguien sin querer a esa persona * ESCIDEOL ----
data <- read_sav("3325.sav")
data

glimpse(data)

data <- data %>% 
  mutate(RECUVOTOGR_REC = (RECUVOTOGR*1))
data$RECUVOTOGR_REC

## 1 derecha 2 izquirda
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC ==1 ] <- 1
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC ==2 ] <- 2
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 18 ] <- 1
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 21 ] <- 2
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 6 ] <- 2
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 67 ] <- 2
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 4 ] <- 1
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 50 ] <- 2
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 8 ] <- 2
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 9 ] <- 1
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 11 ] <- 1
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 13 ] <- 1
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC ==14 ] <- 1
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 7 ] <- 2
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 68 ] <- 2
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 17 ] <- 2
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 12 ] <- 2
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 19 ] <- 2
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 24 ] <- 2
data$RECUVOTOGR_REC[data$RECUVOTOGR_REC == 43 ] <- 2


data$RECUVOTOGR_REC[data$RECUVOTOGR_REC >76 ] <- NA


#data$RECUVOTOGR_REC[data$RECUVOTOGR_REC ==1 ] <- 1
#data$RECUVOTOGR_REC[data$RECUVOTOGR_REC ==2 ] <- 0
#data

data





data <- data %>% 
  select(P13_7, RECUVOTOGR_REC, ESCIDEOL, P20)

data <- data %>% 
  filter(!(ESCIDEOL >10 | P13_7 >5 | RECUVOTOGR_REC >76))





ggplot(na.omit(data), aes(x = as.factor(P13_7), y = ESCIDEOL)) +
  geom_boxplot(varwidth = T, fill = "yellow")  +
  stat_summary(fun.y=mean, geom="point", color="red") +
  scale_y_continuous(name="Escala ideológica del entrevistado", limits=c(1, 10), breaks = seq(0, 10, by = 1))+
  scale_x_discrete(name = "Se pueden tener relaciones sexuales con alguien sin querer a esa persona", labels=c("1" = "Muy de acuerdo", 
                                                                                                               "2" = "De acuerdo",
                                                                                                               "3" = "Ni de ac ni en desac",
                                                                                                               "4" = "En desacuerdo",
                                                                                                               "5" = "Muy en desacuerdo")) +
  theme_modern_rc() +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 7))



bartlett.test(ESCIDEOL ~ P13_7, 
              data = data ) # mayor de 0.05, no podemos rechazar la hipótesis nula. Por lo tanto suponemos homogeneidad de varianzas.

modelo1 <- aov(ESCIDEOL ~ P13_7, 
               data = data) # menor de 0.05 hay diferencia en al menos una de las medias de los grupos de edad

summary(modelo1)

pairwise.t.test(data$ESCIDEOL,data$P13_7, 
                p.adj = "bonferroni")





str(data)
data$P13_7 <- as.numeric(data$P13_7)
data$ESCIDEOL <- as.numeric(data$ESCIDEOL)



modelo1 <- lm(data$ESCIDEOL~data$P13_7)
summary(modelo1)
p30 <-
  ggplot(data, aes(x = as.factor(P13_7), y = ESCIDEOL, color = as.factor(RECUVOTOGR_REC))) +
  scale_y_continuous(name="Escala ideológica del entrevistado", limits=c(1, 10), breaks = seq(0, 10, by = 1))+
  scale_x_discrete(name = "Se pueden tener relaciones sexuales con alguien sin querer a esa persona", labels=c("1" = "Muy de acuerdo", 
                                                                                                               "2" = "De acuerdo",
                                                                                                               "3" = "Ni de ac ni en desac",
                                                                                                               "4" = "En desacuerdo",
                                                                                                               "5" = "Muy en desacuerdo")) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()
  )

p30

p30 + geom_jitter(size = 3, alpha = 0.15,
                  aes(color = as.factor(RECUVOTOGR_REC)))

p30 +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_jitter(size = 3, alpha = 0.15,
              aes(color = as.factor(RECUVOTOGR_REC)))

set.seed(2019)

p30 + geom_jitter(size = 2, alpha = 0.25, width = 0.2,
                  aes(color = as.factor(RECUVOTOGR_REC)))


p30 + geom_jitter(position = position_jitter(seed = 2019, width = 0.2), size = 2, alpha = 0.25,
                  aes(color = as.factor(RECUVOTOGR_REC)))


p30 +
  geom_jitter(size = 2,  width = 0.2, alpha = 0.25,
              aes(color = as.factor(RECUVOTOGR_REC))) +
  stat_summary(fun = mean, geom = "point", size = 5)



p30 <- p30 +
  geom_hline(aes(yintercept = mean(data$ESCIDEOL)), color = "yellow", size = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 5, color = "white") +
  geom_jitter(size = 2,  width = 0.2, height = 0.2, alpha = 0.75, 
              aes(color = as.factor(RECUVOTOGR_REC))) + 
  labs(title = "Diferencia de medias en el grado de acuerdo en poder mantener relaciones sexuales con alguien sin querer a esa persona por ubicación ideológica",
       caption = "CIS Estudio nº 3325 | @dataR_amateur") +
  theme_modern_rc() +
  theme(axis.title.x = element_text(hjust = 0.5, size = 12, color = "darkorange"),
        axis.title.y = element_text(hjust = 0.5, size = 12, color = "darkorange"), 
        plot.title = element_text(hjust = 0.5, color = "green", size = 12),
        axis.text.x = element_text(size = 12, color = "darkorange"),
        axis.text.y = element_text(size = 12, color = "darkorange"),
        plot.caption = element_text(hjust = 1, size = 7),
        legend.position = "top") +
  scale_color_manual(name = "Recuerdo de voto en las elecciones generales", labels = c("Izquierda", "Derecha"),
                     values=c("2" = "red",
                              "1" = "blue"))

p30

p30 + ggsave("relsex_escideol_g.pdf", width = 13, height = 8.5, dpi = 500)


















