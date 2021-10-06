print(hawksSmall)
dim(hawksSmall)
head(hawksSmall)

#1.3
hawk_wt_plot <- ggplot(data = hawksSmall, aes(x = Weight)) + xlab("Weight (gm)")
hawk_wt_plot + geom_histogram(binwidth = 100) + ylab("Count")

#1.4.1
hawk_tail_plot <- ggplot(data = hawksSmall, aes(x = Tail)) + xlab("Tail (mm)")
hawk_tail_plot + geom_density(adjust = 0.5) + ylab("Density")
hawk_tail_plot + geom_density(adjust = 1) + ylab("Density")

#1.4.2
hawk_tail_species_plot <- ggplot(data = hawksSmall, aes(x = Tail, 
                                 color = Species)) + xlab("Tail (mm)")
hawk_tail_species_plot + geom_density(adjust = 1) + ylab("Density")

#1.4.3
hawk_tail_species_vioplot <- ggplot(data = hawksSmall, aes(x = Tail, 
                                    y = Species, fill = Species)) + xlab("Tail (mm)") 
                                                                  # + ylab("Species")
hawk_tail_species_vioplot + geom_violin() + ylab("Species")

#1.5
hawk_scatter_plot <- ggplot(hawksSmall, aes(Tail, Weight, shape = Species, color = Species)) + 
                            xlab("Tail (mm)") + ylab("Weight (gm)")
hawk_scatter_plot + geom_point() 

#1.6
hawk_scatter_facet_plot <- ggplot(hawksSmall, aes(Tail, Weight, color = Species)) + 
  xlab("Tail (mm)") + ylab("Weight (gm)")
hawk_scatter_facet_plot + geom_point() + facet_wrap(~Species) + geom_smooth(method = "lm")


# max(filter(hawksSmall, Species == "CH")$Weight,na.rm = TRUE)
# hawk_scatter_facet_plot + geom_point() + geom_smooth(method = "lm") +
#   geom_curve(x=220, xend=209, y=1140, yend=1125, arrow=arrow(length=unit(0.5,"cm")),curvature = 0.1) +
#   geom_text(x=225, y=1140,label="Heaviest hawk in CH is weighed 1119")
  

#2.1
data("Hawks")
hsF <- select(filter(Hawks,Species == "RT" & Weight>1000),Species,Wing,Weight,Tail)
head(hsF)
dim(hsF)

#With pipe operator
Hawks %>%
  filter(Species == "RT" & Weight>1000) %>%
  select(Wing,Weight,Tail)

##Just to confirm if 2.1 is right:
hsF_conf <- select(filter(Hawks,Species == "RT" & Weight>1000),Species,Wing,Weight,Tail)
head(hsF_conf)

#2.2
head(hsF %>% arrange(Wing))

#2.3
species_code <- unique(Hawks$Species)
species_name_full <- c("Red-tailed", "Cooper's", "Sharp-shinned")
species_name_df <- data.frame(species_code, species_name_full)
species_name_df

speciesdf <- species_name_df %>% rename(Species = species_code)
head(speciesdf)
hawksFullName <- Hawks %>% left_join(speciesdf) %>% 
  select(species_name_full, Wing, Weight) %>%
  rename(Species = species_name_full)
head(hawksFullName)

#2.4
bird_bmi_df <- Hawks %>% mutate(bird_bmi = 1000*Weight/Wing^2) %>% select(Species,bird_bmi) %>%
  arrange(desc(bird_bmi))
head(bird_bmi_df)

filtered_bird_bmi_df <- bird_bmi_df %>% filter(bird_bmi <= 100)
head(filtered_bird_bmi_df)

#Plotting a violin plot for above df
birdbmi_vioplot <- ggplot(data = filtered_bird_bmi_df, aes(x = bird_bmi, 
                                                           y = Species, fill = Species)) + xlab("Bird BMI") 
# + ylab("Species")
birdbmi_vioplot + geom_violin() + ylab("Species")

#2.5
Hawks %>% 
  group_by(Species) %>%
  summarise(num_rows=n(), mn_wing=mean(Wing,na.rm=TRUE), md_wing=median(Wing,na.rm=TRUE), 
            t_mn_wing=mean(Wing,trim=0.1,na.rm=TRUE), 
            tail_wing_ratio=mean(Wing/Tail,na.rm=TRUE))

hawks_summary_df <- Hawks %>%
  group_by(Species) %>%
  select(Wing, Weight, Culmen, Hallux, Tail, StandardTail, Tarsus, Crop) %>%
  summarise(is.na())


