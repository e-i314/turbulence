library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)

dist <- read_csv("ooba_result.csv", show_col_types = FALSE)
lambda_14494_Big_safetydata <- read_csv("D:/Dropbox/Reserch/0.2Data/FOQA_Summary/lambda_14494_Big.csv", show_col_types = FALSE)
lambda_14494_Big_safetydata <- dplyr::left_join(lambda_14494_Big_safetydata,dist,by="dateflt")

lambda_14494_Big_safetydata$lambda.level <- "3.0-"
lambda_14494_Big_safetydata$lambda.level[lambda_14494_Big_safetydata$lambda_m_1000_50 < 3 ] <- "2.0-3.0"
lambda_14494_Big_safetydata$lambda.level[lambda_14494_Big_safetydata$lambda_m_1000_50 < 2 ] <- "1.0-2.0"
lambda_14494_Big_safetydata$lambda.level[lambda_14494_Big_safetydata$lambda_m_1000_50 < 1 ] <- "0.5-1.0"
lambda_14494_Big_safetydata$lambda.level[lambda_14494_Big_safetydata$lambda_m_1000_50 < .5] <- "0.0-0.5"


palette <- c("#009E73","#56B4E9","#F0E442","#D55E00","grey30") #"#E69F00"
lambda_14494_Big_safetydata$quantile_group <- cut(lambda_14494_Big_safetydata$laplace_b, breaks = quantile(lambda_14494_Big_safetydata$laplace_b, probs = seq(0, 1, by = 0.2),na.rm=T), 
                                                  labels =c(" 0-20percentile",
                                                            "20-40percentile",
                                                            "40-60percentile",
                                                            "60-80percentile",
                                                            "80-100percentile") , include.lowest = TRUE)
apt_summary <- lambda_14494_Big_safetydata %>%
  mutate(hour = hour(with_tz(utc,"Asia/Tokyo"))) %>%
  dplyr::group_by(airport_id,
                  hour
  ) %>%
  dplyr::filter(length(unique(dateflt)) >50)


#Graph
# laplace_b% airport hour except landing < 50
ggplot(data=apt_summary %>%
         dplyr::group_by(dateflt) %>%
         slice(1), 
       aes(x=hour(with_tz(utc,"Asia/Tokyo")), y= laplace_b , fill= quantile_group))+
  geom_col(position = "fill")+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(.~LDG.airport)+
  scale_fill_manual(values = palette)+
  ylab("laplace_b（％）")+
  xlab("Hour")+
  theme(legend.position = c(.85,.25))+
  labs(fill = "laplace_b")+
  scale_x_continuous(breaks=seq(8,22, by = 1))

# Flightcount airport hour 
ggplot(data=lambda_14494_Big_safetydata %>%
         dplyr::group_by(dateflt) %>%
         slice(1), 
       aes(x=hour(with_tz(utc,"Asia/Tokyo")), fill= quantile_group))+
  geom_bar()+
  scale_y_continuous()+
  facet_wrap(.~LDG.airport)+
  scale_fill_manual(values = palette)+
  ylab("Flightcount")+
  xlab("Hour")+
  theme(legend.position = c(.85,.25))+
  labs(fill = "laplace_b")+
  scale_x_continuous(breaks=seq(8,22, by = 1))

# Flightcount airport month
ggplot(data=lambda_14494_Big_safetydata %>%
         dplyr::group_by(dateflt) %>%
         slice(1), 
       aes(x=month(with_tz(utc,"Asia/Tokyo")), fill= quantile_group))+
  geom_bar()+
  scale_y_continuous()+
  facet_wrap(.~LDG.airport)+
  scale_fill_manual(values = palette)+
  ylab("Flightcount")+
  xlab("Hour")+
  theme(legend.position = c(.85,.25))+
  labs(fill = "laplace_b")+
  scale_x_continuous(breaks=seq(1,12, by = 1))



# laplace_b % airport month
ggplot(data=lambda_14494_Big_safetydata %>%
         dplyr::group_by(dateflt) %>%
         slice(1), 
       aes(x=month(with_tz(utc,"Asia/Tokyo")), y= laplace_b , fill= quantile_group))+
  geom_col(position = "fill")+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(.~LDG.airport)+
  scale_fill_manual(values = palette)+
  ylab("laplace_b（％）")+
  xlab("Month")+
  theme(legend.position = c(.85,.25))+
  labs(fill = "laplace_b")+
  scale_x_continuous(breaks=seq(1,12, by = 1))

# wind direction speed laplace_b windshear airport
plotdata <- lambda_14494_Big_safetydata %>%
  group_by(dateflt) %>%
  mutate(ws = sum(windshear_warning =="On") > 0) %>%
  slice(1)

ggplot()+
  ylim(0,35)+
  xlim(0,360)+
  xlab("Wind direction at the airport(deg)")+
  ylab("Wind speed at the airport(kt)")+
  scale_x_continuous(limits = c(0, 360), breaks = seq(45, 360, by = 45)) +
  scale_size_manual(values = c("FALSE" = .7,"TRUE" = 2))+
  geom_jitter(data = plotdata %>%
                group_by(dateflt) %>%
                filter(ws != TRUE) %>%
                slice(1),
              aes(x=LDG.metar.WindDir,
                  y=LDG.metar.WindSpd,
                  color = quantile_group,
                  shape = ws
              ) ,
              height=.5, width =5,size=.8)+
  geom_point(data = plotdata %>%
               group_by(dateflt) %>%
               filter(ws == TRUE) %>%
               slice(1),
             aes(x=LDG.metar.WindDir,
                 y=LDG.metar.WindSpd,
                 color = quantile_group,
                 shape = ws
             ),
             size=3
             
  ) +
  geom_point(data = plotdata %>%
               group_by(dateflt) %>%
               filter(ws == TRUE) %>%
               slice(1),
             aes(x=LDG.metar.WindDir,
                 y=LDG.metar.WindSpd,
                 shape = ws),
             #             color = "white",
             size=3) +
  coord_polar()+
  scale_color_manual(values = palette)+
  labs(color = "laplace_b")+
  labs(size = "Windshear")+
  facet_wrap(.~LDG.airport)+
  theme(legend.position = c(.92,.25))
