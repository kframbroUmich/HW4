
library(tidyr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(patchwork)
cars93 <- MASS::Cars93

#Question #3A (plot variables = Question#+Plot#)

q3a1<-ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = FALSE, method = "lm",formula = y ~ x, color = "#0072B2") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")+
  ggtitle('LM Smothing') 
q3a2<-q3a1 +geom_smooth(se=FALSE,method="glm")+
  ggtitle('GLM Smoothing')
q3a3<-q3a1 +geom_smooth(se=FALSE,method="gam")+
  ggtitle('GAM Smoothing')
q3a1+q3a2+q3a3

#Question3B Using the variables from the previous question
q3b1<-q3a1+geom_smooth(se=TRUE,method="lm")
q3b2<-q3a2 +geom_smooth(se=TRUE,method="glm")
q3b3<-q3a3 +geom_smooth(se=TRUE,method="gam")

q3b1+q3b2+q3b3

#Question3C Using the variables from the previous question
q3c1<-q3a1+geom_smooth(se=TRUE,method="lm",color='#8fe388')
q3c2<-q3a2 +geom_smooth(se=TRUE,method="glm",color='#fe8d6d')
q3c3<-q3a3 +geom_smooth(se=TRUE,method="gam",color='#7c6bea')

q3c1+q3c2+q3c3

#Question3D Using the variables from the previous question
q3d1<-q3c1+ggtitle('Liner Regression Smoothing')
q3d2<-q3c2+ggtitle('Generalized Liner Model Smoothing')
q3d3<-q3c3+ggtitle('Generalized Additive Model Smoothing')

q3d1+q3d2+q3d3

#Question3e Using the variables from the previous question
q3e1<-q3d1+theme(plot.title=element_text(size=14,color='#8fe388'))
q3e2<-q3d2+theme(plot.title=element_text(size=14,color='#fe8d6d'))
q3e3<-q3d3+theme(plot.title=element_text(size=14,color='#7c6bea'))

q3e1+q3e2+q3e3

#Question #4
library(tidyverse)
library(ggplot2)
install.packages('ggridges')
library(ggridges)
library(lubridate)
library(ggrepel)
library(colorspace)

#put your folder's path inside quotes below
folder_location='C:/Users/k_fra/OneDrive/UM-Flint/CSC302/Data'
setwd(folder_location)
load("./preprint_growth.rda") #please change the path if needed
head(preprint_growth)
preprint_growth %>% filter(archive == "bioRxiv") %>%
  filter(count > 0) -> biorxiv_growth
preprints<-preprint_growth %>% filter(archive %in%
                                        c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))
1
preprints_final <- filter(preprints, date == ymd("2018-01-01"))
ggplot(preprints) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis( #this part is for the second y axis
      breaks = preprints_final$count, #and we use the counts to position our labels
      labels = c("arXivq-bio", "PeerJPreprints", "bioRxiv"),
      name = NULL)
  ) +
  scale_x_date(name = "year",
               limits = c(min(biorxiv_growth$date), ymd("2018-01-01"))) +
  scale_color_manual(values = c("#dc23b2", "#B2DC23", "#23B2DC"),
                     name = NULL) +
  theme(legend.position = "none")
