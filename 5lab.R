library(readxl)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(patchwork)
library(countrycode)

total_alcohol <- read_excel("C:/Users/MSI/Desktop/total-alcohol-consumption-per-capita-litres-of-pure- ahcool new.xlsx")

# Pritaikyk regiono priskyrimą:
total_alcohol$continent <- countrycode(total_alcohol$Entity, origin = 'country.name', destination = 'continent')

europe <- total_alcohol[total_alcohol$continent == "Europe", ]
head(europe)

names(europe)[4] <- "total_alcohol_con"
europe$total_alcohol_con <- as.numeric(europe$total_alcohol_con)
europe <- na.omit(europe)



df_2000 <- europe %>% filter(Year == 2000) %>% select(Entity, total_alcohol_con) %>% rename(alc_2000 = total_alcohol_con)
df_2018 <- europe %>% filter(Year == 2018) %>% select(Entity, total_alcohol_con) %>% rename(alc_2018 = total_alcohol_con)

df_joined <- left_join(df_2000, df_2018, by = "Entity") %>%
  mutate(tooltip = paste0(Entity, "\n2000 m.: ", alc_2000, " l\n2018 m.: ", alc_2018, " l"))

p7 <- ggplot(df_joined, 
            aes(y = reorder(Entity, alc_2000),
                x = alc_2000,
                tooltip = tooltip,
                data_id = Entity)) +
  geom_bar_interactive(stat = "identity", fill = "orchid4",
                       hover_css = "fill:pink;") +
  labs(x = "2000 m.", y = "") +
  theme_minimal()

p8 <- ggplot(df_joined, 
            aes(y = reorder(Entity, alc_2018),
                x = alc_2018,
                tooltip = tooltip,
                data_id = Entity)) +
  geom_bar_interactive(stat = "identity", fill = "orchid4",
                       hover_css = "fill:pink;") +
  labs(x = "2018 m.", y = "") +
  theme_minimal()

p9 <- (p7 | p8) +
  plot_annotation(title = "Alkoholio suvartojimas litrais Europos šalyse 2000 m. ir 2018 m.") &
  theme(plot.title = element_text(hjust = 0.5))
girafe(code = print (p9))