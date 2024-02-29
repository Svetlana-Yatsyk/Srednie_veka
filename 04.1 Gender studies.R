
#Gender studies------

library(ggplot2)
library(ggridges)
library(dplyr)
library(readr)

metadata <- read.csv("metadata.csv")

# смотрим, сколько всего женщин, мужчин и соавторств
table(metadata_only$Gender)

# группируем данные по году и гендеру и считаем количество записей
df_summary <- metadata %>%
  group_by(Year, Gender) %>%
  summarise(Count = n(), .groups = 'drop')

# разделяем данные по группам
data_male <- df_summary[df_summary$Gender == "м", ]
data_female <- df_summary[df_summary$Gender == "ж", ]
data_group <- df_summary[df_summary$Gender == "—", ]

# строим базовый график для женщин
plot(data_female$Count ~ data_female$Year, 
     type="b", 
     bty="l", 
     xlab="Year", 
     ylab="Count", 
     col="#13d9b4", 
     lwd=2, 
     #pch=22, # квадрат
     cex=0.6, # Размер точек
     ylim=c(1, max(df_summary$Count)),
     main="Количество статей, написанных мужчинами и женщинами" # Название графика
)

# Добавляем линии и точки для мужчин
points(data_male$Count ~ data_male$Year, 
       col="#b29e89",
       lwd=2, 
       #pch=25, # перевернутый треугольник
       cex=0.6,
       type="b"
)

# Добавляем линии и точки для коллективов
points(data_group$Count ~ data_group$Year, 
       col="#fc393e", 
       lwd=1, 
       #pch=21, # круг
       cex=0.6,
       type="b"
)

# Добавляем подписи к пиковым точкам для каждой группы
# with(data_female, text(Year, Count, labels=Count, pos=3, cex=0.7))
# with(data_male, text(Year, Count, labels=Count, pos=3, cex=0.7))
# with(data_group, text(Year, Count, labels=Count, pos=3, cex=0.7))

# Добавляем легенду в левый верхний угол
legend("topleft", 
       legend = c("мужчины", "женщины", "коллектив авторов"), 
       col = с("#b29e89","#13d9b4", "#fc393e"),
       pt.cex = 1.2, # Размер символов в легенде
       cex = 1.2, # Размер текста в легенде
       bty = "n", # Убираем границу вокруг легенды
       text.col = "black"
)


# joyplot=====
# Преобразование года в фактор для упорядочивания на графике
df_filtered <- df_summary %>% 
  filter(Gender %in% c("м", "ж"))

df_filtered$Year <- as.numeric(as.character(df_filtered$Year))

# График с фасетированием по гендеру
ggplot(df_filtered, aes(x = Year, y = Count, fill = Gender)) +
  geom_col() +
  facet_wrap(~ Gender, scales = "free_y") +
  scale_x_continuous(breaks = seq(min(df_filtered$Year), max(df_filtered$Year), by = 5)) +
  scale_fill_manual(values = c("м" = "#b29e89", "ж" = "#13d9b4")) +
  labs(title = 'Распределение статей по годам и гендеру', x = 'Год', y = 'Количество статей') +
  theme_minimal() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, hjust = 1))

