library(tidyverse)
library(cowplot)
library(png)
library(reshape2)
library(dbscan)
library(gganimate)

input_text <- "come to WES!"

plot_text <- ggdraw() + 
  draw_label(input_text, fontface='bold', hjust = 1, size = 30)

png("text.png")
plot_text
dev.off()

png <- readPNG("text.png")

# take a layer of the 3-channel
# smooth out the value (i.e. make it 0,1)
# melt to get coordinates
# do this for two layers
matrix_list <- lapply(1:2, function(x) {
  png[,,x] %>% 
  round() %>%
  melt() %>%
  filter(value == 0)
})

# combine the matrix within the list
matrix <- do.call('rbind', matrix_list)

# add grouping for timing
dbscan_res <-dbscan(matrix, 2, 5)
dbscan_res$cluster %>% table()
matrix$group <- dbscan_res$cluster

# add timing by groups
matrix <- matrix %>%
  group_by(group) %>%
  mutate(within_time = sample(0:9, n(), replace = T),
         between_time = within_time + (10 * group)) %>%
  ungroup()

# extra stuff for Go Wes
matrix <- matrix %>%
  mutate(color = ifelse(group <= 6, "go", "wes"))

# plot
p <- ggplot(matrix) +
  geom_jitter(aes(Var2, Var1, frame = between_time, cumulative = T, col = color), 
              width = 0.8, 
              alpha = 0.2) +
  scale_y_continuous(limits = c(max(matrix$Var1), min(matrix$Var1)), trans = "reverse") +
  scale_x_continuous(limits = c(min(matrix$Var2), max(matrix$Var2))) +
  scale_color_manual(values = c("go" = "black", "wes" = "#BA0C2F")) +
  coord_fixed() +
  theme(line = element_blank(), 
        text = element_text(family = "", 
                            face = "plain", colour = "black", size = 11, lineheight = 0.9, 
                            hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
                            debug = FALSE), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        legend.text = element_text(size = rel(0.8)), 
        legend.title = element_text(hjust = 0), 
        strip.text = element_text(size = rel(0.8)), 
        plot.margin = unit(c(0, 0, 0, 0), "lines"), 
        plot.background = element_rect("white"),
        complete = TRUE,
        legend.position = "none",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"))


animation::ani.options(ani.width=1000, ani.height=400)
gganimate(p, interval = 0.07, title_frame = F, filename = "text.gif")
