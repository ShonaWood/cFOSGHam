library(tidyverse)
library(viridis)
library(readxl)
library(plotly)
library(lubridate)

bm <- read_excel("./bm/bodymass_hibexp1.xlsx", 
                 col_types = c("text", "numeric", "text", 
                               "text"))
bm$date <- as_date(bm$date, format = "%d.%m.%Y")

bm <- na.omit(bm)
summary(bm)

unique(bm$status)


pbm <- ggplot(bm, aes(reorder(status, date), y=bm, fill= status))+
  geom_violin(position = position_dodge(width = 0.7))+
  geom_point( size=0.4, alpha=0.9, position = position_jitterdodge(dodge.width=0.5, jitter.width = 0.4))+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option = "F", direction = 1) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.9)) +
  xlab("")+ ylab("Body mass (g)")+
  ylim(c(40,150))

pbm




pbm2 <- ggplot(bm, aes(x=date, y=bm, color=status))+
  geom_violin( size=1, alpha=1)+
  scale_color_viridis(discrete = TRUE, alpha=0.6, option = "H", direction = 1) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.9)) +
  xlab("")+ ylab("Body mass (g)")+
  scale_y_continuous(n.breaks = 20)
pbm2


