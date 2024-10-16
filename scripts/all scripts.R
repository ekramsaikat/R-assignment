#install and load necessary packages
install.packages("usethis")
install.packages("tidyverse")
install.packages("gtsummary")
install.packages("easystats")
install.packages("gt")
install.packages("ggplot2")
install.packages("naniar")
install.packages("ggthemes")
install.packages("RColorBrewer")
install.packages("rio")
install.packages("sjPlot")
install.packages("MASS")
install.packages("flextable")
install.packages("report")


install.packages("broom.helpers")
library(broom.helpers)
library(report)
library(tidyverse)
library(gtsummary)
library(easystats)
library(gt)
library(ggplot2)
library(naniar)
library(ggthemes)
library(RColorBrewer)
library(rio)
library(sjPlot)
library(tidyr)
library(MASS)
library(dplyr)
library(scales)
#Import data
KAP1<-readxl::read_xlsx("data/AMR_KAP_Data.xlsx",sheet = 1)
KAP2<-readxl::read_xlsx("data/AMR_KAP_Data.xlsx",sheet = 3)

#Check missing values
gg_miss_var(KAP2)

#Check Duplicated rows
sum(duplicated(data))

#Table 1. Demographic characteristics of study participants

KAP2 |>
  select(1:11)|>
  tbl_summary()|>
  as_gt()|>
  gtsave("C:/Users/HUAWEI/Downloads/R assignment Amr/tables/demographic_tbl.docx")

#Table 2. Major sources of information about antibiotic of parents 

KAP2 |>
  select(48:56)|>
  tbl_summary() |>
  as_gt()|>
  gtsave("C:/Users/HUAWEI/Downloads/R assignment Amr/tables/sources_tbl2.docx")


#Table 3.  Level of knowledge, attitudes, and practices

KAP2 <- KAP2|>
  mutate(Knowledge_level  = case_when(
    `pct of knowledge` < 25 ~ "Poor",
    `pct of knowledge` <= 50 ~ "Moderate",
    `pct of knowledge`>= 50 ~ "Good",
  ))|>
  mutate(Attitude_level = case_when(
    `pct of attitude` < 25 ~ "Poor",
    `pct of attitude` <= 50 ~ "Moderate",
    `pct of attitude` >= 50 ~ "Good"
  )) |>
  mutate(Practice_level = case_when(
    `pct of practices` < 50 ~ "misuse",
   `pct of practices` >= 50 ~  "Good"
  )) |>
  select(70:72)|>
  tbl_summary() |>
  as_gt()|>
  gtsave("C:/Users/HUAWEI/Downloads/R assignment Amr/tables/level_of_KAP_tbl.docx")

#Table 4. Factors associated with the level of knowledge


KAP2$`Parent’s sex`<-factor(KAP2$`Parent’s sex`)
KAP2$`Parent’s education level`  <-factor(KAP2$`Parent’s education level` )
KAP2$`Employment status`<-factor(KAP2$`Employment status`)
KAP2$`Family type`<-factor(KAP2$`Family type`)
KAP2$`Your average household income per month (BDT)`<-factor(KAP2$`Your average household income per month (BDT)`)
KAP2$`Child’s sex`<-factor(KAP2$`Child’s sex`)
KAP2$`Child’s sex`<-factor(KAP2$`Child’s sex`)
KAP2$`Number of children`<-factor(KAP2$`Number of children`)
  
#regression_tbl knowledge level 
colnames(KAP2)
 data_knowledge<-KAP2[,c(1:9,70)]
 colnames(data_knowledge)
 
#convert to factor 
 
 data_knowledge$Knowledge_level  <-factor(data_knowledge$Knowledge_level)
 
 
tbl_uvreg<-data_knowledge|>
  tbl_uvregression(
    method = polr,
    y=Knowledge_level,
    exponentiate = TRUE
  )|>
  add_global_p()|>
  bold_p(t=0.10)|>
  as_gt()|>
  gtsave("C:/Users/HUAWEI/Downloads/R assignment Amr/tables/regression_tbl1.docx")
  
  
  
 
#regression_tbl attitude
colnames(KAP2)
data_knowledge<-KAP2[,c(1:9,71)]
colnames(data_knowledge)

data_knowledge$Attitude_level  <-factor(data_knowledge$Attitude_level)

tbl_uvreg2<-data_knowledge|>
  tbl_uvregression(
    method = polr,
    y=Attitude_level,
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits=2),
  )|>
  add_global_p()|>
  bold_p(t=0.10)|>
  as_gt()|>
  gtsave("C:/Users/HUAWEI/Downloads/R assignment Amr/tables/regression_tbl2.docx")

#regression table practice

colnames(KAP2)
data_knowledge<-KAP2[,c(1:9,72)]
colnames(data_knowledge)

data_knowledge$Practice_level  <-factor(data_knowledge$Practice_level)

tbl_uvreg3<-
  data_knowledge|>
  tbl_uvregression(
    method = glm,
    y=Practice_level,
    method.args =list(family=binomial(link="logit")),
    exponentiate = TRUE)|>
  as_gt()|>
  gtsave("C:/Users/HUAWEI/Downloads/R assignment Amr/tables/regression_tbl3.docx")





 
# Figure 1.  Distribution of knowledge of antibiotic resistance 
fig_data1 <- KAP1|>
  select(13:24)

#data reshape
long_fig_data <- fig_data1|>
  pivot_longer(cols = 1:12,
               names_to ="question",
               values_to = "response")
summary_data<-long_fig_data|>
  group_by(question,response)|>
  summarise(count=n(),.groups = 'drop')|>
  mutate(Percentage = count/sum(count)*100)

#plot
plot1<-ggplot(summary_data,aes(x=question,y=Percentage,fill = response))+
  geom_bar(stat = "identity",position ="fill")+
  coord_flip()+
  scale_y_continuous(labels =scales::percent)+
  scale_fill_manual(values =c("Yes"= "#1E7F7F",
                              "No"="#D3D3D3",
                              "Don't Know" ="#D6C48A"))+
  labs(title = "Figure 1.  Distribution of knowledge of antibiotic resistance
        among parents of school-going children (N = 704).",
       x="",
       y="Percentage",
       fill = "response")+theme_minimal()+
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 12,face = "bold",hjust = 0.5))

ggsave("C:/Users/HUAWEI/Downloads/R assignment Amr/figures/figure1.png",
       plot = plot1,
       width = 10,
       height = 6,
       dpi = 600) 
    
#Figure 2.Attitude towards antibiotic resistance and the misuse of antibiotics
fig_data2<-KAP1|>
  select(24:33)

#wide to long format
long_fig_data2<-fig_data2|>
  pivot_longer(
    cols = 1:10,
    names_to = "question",
    values_to = "response"
  )
  

#Count the responses for each question
summary_data2<-long_fig_data2|>
  group_by(question,response)|>
  summarise(count=n(),.groups ='drop')|>
  mutate(Percentage=count/sum(count)*100)

#plot creation
plot2<-ggplot(summary_data2,aes(x=question,y=Percentage,fill=response))+
  geom_bar(stat = "identity",position = "fill")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("Agree"="#1E7F7F",
                               "Disagree" = "#D3D3D3",        
                               "Neutral" = "#D6C48A" ))+
labs(title = " Figure 2. Attitude towards antibiotic resistance
  and the misuse of antibiotics among parents of school-going children (N = 704).",
     x="",
     y="Percentage",
     fill="response")+
  theme_minimal()+
  theme(axis.text.y = element_text(size =10),
        plot.title = element_text(size = 15,face = "bold",hjust = 0.5))

ggsave("C:/Users/HUAWEI/Downloads/R assignment Amr/figures/figure2.png", 
       plot = plot2, 
       width = 15, 
       height =  8, 
       dpi =  600)

  
## Figure 3.  Practices among parents
fig_data3<-KAP1|>
  select(34:39)

#reshaping data
long_fig_data3<-fig_data3|>
  pivot_longer(
    cols = 1:6,
    names_to ="question",
    values_to ="response"
  )

# Count the responses for each question
  summary_data3<-long_fig_data3|>
    group_by(question,response)|>
    summarise(count=n(),.groups = 'drop')|>
    mutate(Percentage=count/sum(count)*100)
  
  #plot creation
  plot3<-ggplot(summary_data3,aes(x=question,y=Percentage,fill=response))+
    geom_bar(stat = "identity",position = "fill")+
    coord_flip()+
    scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = c("yes"="#1E7F7F",        
                                 "No" = "#D6C48A"))+
    labs(title = "Figure 3. Practices among parents of school-going children
        regarding antibiotic resistance (N = 704).",
    x="",
    y="Percentage",
    fill="response")+
    theme_minimal()+
    theme(axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 16,face = "bold",hjust = 0.5))
ggsave("C:/Users/HUAWEI/Downloads/R assignment Amr/figures/figure3.png",
       plot = plot3,
       width = 15,
       height = 8,
       dpi = 600)
    
         
  
  
  
  
  
  
