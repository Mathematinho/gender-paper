library(tidyverse)
library(readxl)
library("ggsignif") 
library(gridExtra)
library(cowplot)

first_lsmeans <- read_xlsx('tables/first lsmeans.xlsx')
first_diffs <- read_xlsx('tables/first diffs.xlsx') %>% filter(stpbon_p < 0.05) %>% mutate(p_print = format.pval(stpbon_p,digits=2, eps = .001))

########################################################################
fig_first_lsmeans_qtsource <- ggplot(first_lsmeans %>% filter(Effect == 'qt*source'),
                                     aes(x = source, y = Mu, fill = qt)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_signif(y_position = c(0.45 + 0.03 * (1:5)),
              xmin = c(0.65, 0.65, 0.9, 1.1, 1.4), xmax = c(1.1, 1.4, 1.4, 2.1, 2.4),
              map_signif_level = TRUE,
              annotations = first_diffs$p_print[first_diffs$Effect == 'qt*source'])+
  xlab("Database") + ylab("% female first authorship") +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.6),breaks = seq(0,0.6, by=0.1)) +
  scale_fill_discrete(name = "JIF Quartile") +
  annotate("text", x = -Inf, y = Inf, label = "a)", size=5,vjust = 1, hjust = 0)+
  theme(legend.position = c(1,0.9),
        legend.justification = c(1,1),    # Move the legend to the top
        legend.background = element_rect(fill = alpha("white", 0.3)),
        legend.title.align = 0.5)  # Center-align the legend title


fig_first_lsmeans_qtsource

########################################################################

fig_first_lsmeans_subtype <- ggplot(first_lsmeans %>% filter(Effect == 'subtype'),
                            aes(x = subtype, y = Mu))+
  geom_bar(stat = "identity",position = position_dodge(width=0.9))+
  geom_signif(y_position = 0.45,
              xmin=1,xmax=2,
              map_signif_level = TRUE,
              annotations = first_diffs$p_print[first_diffs$Effect=='subtype'])+
  xlab("Document Type")+ylab("% female first authorship")+ 
  scale_y_continuous(labels = scales::percent, limits = c(0,0.6),breaks = seq(0,0.6, by=0.1))+
  annotate("text", x = -Inf, y = Inf, label = "b)", size=5,vjust = 1, hjust = 0)+
  theme(legend.position = "top",    # Move the legend to the top
        legend.title.align = 0.5)     # Center-align the legend title
fig_first_lsmeans_subtype

########################################################################
first_lsmeans <- read_xlsx('tables/first lsmeans.xlsx')
first_diffs <- read_xlsx('tables/first diffs.xlsx') %>% filter(stpbon_p < 0.05) %>% mutate(p_print = format.pval(stpbon_p,digits=2, eps = .001))
fig_first_lsmeans_seniorsource <- ggplot(first_lsmeans %>% filter(Effect == 'source*Senior_Author'),
                                     aes(x = source, y = Mu, fill = Senior_Author))+
  geom_bar(stat = "identity",position = position_dodge(width=0.9))+
  geom_signif(y_position = 0.5,
              xmin=c(0.8,1.8),xmax=c(1.2,2.2),
              map_signif_level = TRUE,
              annotations = first_diffs$p_print[first_diffs$Effect=='source*Senior_Author'])+
  geom_signif(y_position = 0.55,
              xmin=c(0.8,1.8),xmax=c(1.2,2.2),
              map_signif_level = TRUE,
              annotations = "")+
  geom_signif(y_position = 0.555,
              xmin=1,xmax=2,
              map_signif_level = TRUE,
              annotations = "DID: p = 0.004")+
  xlab("Database")+ylab("% female first authorship")+ 
  scale_y_continuous(labels = scales::percent, limits = c(0,0.6),breaks = seq(0,0.6, by=0.1))+
  annotate("text", x = -Inf, y = Inf, label = "c)", size=5,vjust = 1, hjust = 0)+
  scale_fill_discrete(name = "Senior Author Gender")+
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),    # Move the legend to the top
        legend.background = element_rect(fill = alpha("white", 0.3)),
        legend.title.align = 0.5)


source("code/align legend.r")

fig_first_lsmeans_seniorsource <- ggdraw(align_legend(fig_first_lsmeans_seniorsource,hjust=1))

fig_first_lsmeans_seniorsource



grid.arrange(fig_first_lsmeans_qtsource, fig_first_lsmeans_subtype, fig_first_lsmeans_seniorsource, ncol = 3)



png("figures/first author.png",width = 14, height=7, units = "in",res = 300)
grid.arrange(fig_first_lsmeans_qtsource, fig_first_lsmeans_subtype, fig_first_lsmeans_seniorsource, ncol = 3)
dev.off()

