

###cor plots bars####

#make bar correlation plots for crit III (cor with econ indicators etc., from Pintar and Essletzbicher, 2022)

#load data
load(file =  '..\\Data\\data.rda')

cor_regions_indic_bar_df <- data


#calculate se (sd / sqrt(n)), n= 18 periods
cor_regions_indic_bar_df <- cor_regions_indic_bar_df %>% mutate(se = std/sqrt(18) )



#make levels make sense
cor_regions_indic_bar_df <- cor_regions_indic_bar_df %>% droplevels() 

y_comb_levels <- cor_regions_indic_bar_df %>% select(y_comb, y) %>% distinct() %>% arrange(desc(y)) %>% pull(y_comb)

cor_regions_indic_bar_df$y_comb <- factor(cor_regions_indic_bar_df$y_comb, levels = y_comb_levels)

x_axis_levels <- c('div.', 'no. pat.', 'R&D', 'GDP', 'GDP(p.e.)',  
                   'GDP gr.', 
                   "GDP(p.e.) gr.",
                   'pat. gr.')
  
cor_regions_indic_bar_df$x_axis <- factor(cor_regions_indic_bar_df$x_axis, levels = x_axis_levels)


#add xmin and xmax
cor_regions_indic_bar_df <- cor_regions_indic_bar_df %>% mutate(xmin1 = mean - std, xmax1 = mean +  std, xmin2 = mean - 2*std, xmax2 = mean + 2* std, 
            xmin1 = case_when(
            xmin1< -1 ~ -1,
            TRUE ~ xmin1), xmax1 = case_when(xmax1 > 1 ~ 1,
            TRUE ~ xmax1),xmin2 = case_when(xmin2< -1 ~ -1,
            TRUE ~ xmin2),xmax2 = case_when(xmax2 > 1 ~ 1,
            TRUE ~ xmax2))


                    
#colours measures, purple, blue and orange from other plots                                            
colors_measures  <-  c('#542788', '#FDAE6B', '#9ECAE1')



#make graph without growths
cor_bar_plot <- cor_regions_indic_bar_df %>% filter(!(grepl('gr', x_axis, fixed = T))) %>%
  ggplot(aes(x = mean, y = y_comb, color = measure, fill = measure)) +
  #add zero line
  geom_vline(aes(xintercept = 0), color = 'lightcyan4', size = 1) +
  #add points and error bars
  geom_pointrange(aes(xmin = xmin1, xmax = xmax1), size = 1.75, fatten = 2)+
  #sideplot density
  geom_xsidedensity(alpha = .3, position = "identity", color = 'grey35') +
  scale_x_continuous(limits = c(-1,1), n.breaks = 6) +
  scale_color_manual(values = colors_measures) +
  scale_fill_manual(values = colors_measures) +
  
  #theme etc
  theme(
    text = element_text(family = "Calibri", colour = 'grey35'),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 0.5, unit = 'cm')),
    # panel.grid.major = element_blank(),
    #  panel.border = element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    strip.background = element_rect(fill = 'azure3'),
    plot.title = element_text(size = 12),
    #legend.position = 'none',
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.direction = "vertical") +  
  #theme for sideplot
  theme(ggside.panel.scale = .3,
        ggside.panel.border = element_blank(),
        ggside.panel.grid = element_blank(),
        ggside.panel.background = element_blank()) +
  
  guides(color = 'none', fill = 'none') +
  
  labs(y = 'Variant') +
  
  
  facet_wrap(vars(x_axis), nrow = 1)


cor_bar_plot

#save
save(cor_bar_plot, file = '..\\plots\\cor_bar_plot.rda')
ggsave(filename  = paste0(getwd(),"./Plots/cor_bar_plot.png" ), plot = cor_bar_plot, device = "png", dpi = 1000, scale = 1,height = 30, width = 30, units = 'cm')




