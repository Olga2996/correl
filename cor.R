# Фареева Ольга Сергеевна - для дневных данных за лето 2019 года, постройте регрессионную зависимость
# скорости сокотечения (переменная Flux)  для деревьев вида Acer platanoides и визуальной оценкой меньше 3

data = read.csv("data.csv")
summary(data)


data_filtered_1 = data[data$doy > 151 & data$doy < 244,]
data_filtered_2 = data_filtered_1[data_filtered_1$hour < 19 | data_filtered_1$hour > 7,]
data_filtered_3 = data_filtered_2[data_filtered_2$Species == "Acer platanoides" ,]
data_filtered_4 = data_filtered_3[data_filtered_3$VTA_score < 3 ,]

library(dplyr)

data_filtered_4=select(data_filtered_4,-c("id", "Species","age_group_index","time", "antrop_load", 
                                          "in_site_antrop_load") )
library(ggplot2)

library(ggcorrplot)

corr = cor(data_filtered_4,use = "na.or.complete")^2

ggcorrplot(corr,
           tl.cex=4
)
flux_corr=corr[ ,"Flux"]
flux_corr=flux_corr[flux_corr>0.1]
flux_corr

formula8 = Flux ~ tair+VPD+b_B_500c+b_W_860c+b_V_810c+b_U_760c+hour+dT+dTa+u+TTair+TTR_650c+TTR_500c+
  +TTR_450c+TTR_650+TTR_500+TTR_450+T+E1+G1

formula9 = Flux ~ tair+VPD+b_B_500c+b_W_860c+b_V_810c+b_U_760c+hour+dT+dTa+u+TTair+TTR_650c+TTR_500c+
+TTR_450c+TTR_650+TTR_500+TTR_450+T+E1+G1+tair:VPD:b_B_500c:b_W_860c:b_V_810c:b_U_760c:hour:dT
:dTa:u:TTair:TTR_650c:TTR_500c:TTR_450c:TTR_650:TTR_500:TTR_450:T:E1:G1

model8 = lm(data=data_filtered_4, formula8)
model9 = lm(data=data_filtered_4, formula9)

summary(model8)
summary(model9)
