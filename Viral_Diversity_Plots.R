###########################
# MOTHERS VS INFANTS PLOTS
###########################

# Plot the richness and diversity in mothers and infants
pdf('2_DIVERSITY/Plots/Shannon_Index_comp.pdf', width=3, height=3.3)
ggplot(Sample_metadata[!is.na(Sample_metadata$Timepoint_categorical),], aes(x=Type, y=shannon)) +
  geom_violin(aes(fill=Type), alpha=0.5, width=0.8) +
  geom_jitter(aes(color=Type), alpha=0.7) +  
  geom_boxplot(width=0.2, fill="white", color="black") + 
  labs(x = 'Sample type', y = 'Shannon Index') + 
  scale_fill_manual(values=c("#00A087", "#3366CC")) +  
  scale_color_manual(values=c("#00A087", "#3366CC")) +  
  theme_classic() +
  theme(
    plot.title = element_text(size=18, hjust = 0.5, face="bold"), 
    axis.text = element_text(size=14),
    axis.title = element_text(size=16), 
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    legend.position = "none"  
  )
dev.off()

pdf('2_DIVERSITY/Plots/Richness_comp.pdf', width=3, height=3.3)
ggplot(Sample_metadata[!is.na(Sample_metadata$Timepoint_categorical),], aes(x=Type, y=richness)) +
  geom_violin(aes(fill=Type), alpha=0.5, width=0.8) +
  geom_jitter(aes(color=Type), alpha=0.7) +  
  geom_boxplot(width=0.2, fill="white", color="black") + 
  labs(x = 'Sample type', y = 'Viral Richness') + 
  scale_fill_manual(values=c("#00A087B2", "#3366CC")) + 
  scale_color_manual(values=c("#00A087", "#3366CC")) + 
  theme_classic() + 
  theme(plot.title = element_text(size=18, hjust = 0.5, face="bold"), 
        axis.text = element_text(size=14),
        axis.title = element_text(size=16), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "none") 
dev.off()

# Generate NMDS plot with maternal (blue) and infant (green) samples
pdf('2_DIVERSITY/Plots/Viral_vOTUs_ALL_Bray_NMDS_without_outliers.pdf', width=10/2.54, height=10/2.54)
NMDS_plot_all <- ggplot(data = data.scores_all, aes(x = NMDS1, y = NMDS2, color=Type)) + 
  geom_point(size = 1.5, alpha=0.7) + 
  geom_point(data=centroids_all, aes(fill=Type),shape=NA, size=4, color='black') + 
  stat_ellipse(geom = "polygon", alpha = 0.0, aes(group = Type, color=Type), linetype = 2, linewidth = 1) +
  theme_bw()+
  scale_color_manual(values=c("#00A087B2", "#3366CC")) +
  theme(axis.text=element_text(size=13),
        axis.title = element_text(size = 15), 
        panel.background = element_blank(), 
        panel.grid = element_blank(), 
        panel.border = element_rect(fill = NA, linewidth =1.2, colour = "grey30"), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.title = element_text(size = 16, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 16, colour = "grey30"),
        legend.position = "bottom") 
ggMarginal(NMDS_plot_all, type="densigram", groupFill=T)
dev.off()

# Plot the proportion and relative abundances of temperate viruses in mothers and infants
pdf('3_LIFESTYLE_PREDICTION//Plots/Prop_temperates_comp.pdf', width=3, height=3.3)
ggplot(Sample_metadata[!is.na(Sample_metadata$Timepoint_categorical),], aes(x=Type, y=prop_temperates)) +
  geom_violin(aes(fill=Type), alpha=0.7, width=0.8) +
  geom_boxplot(width=0.2, fill="white", color="black") + 
  labs(x = 'Sample type', y = 'Proportion (%)') + 
  scale_fill_manual(values=c("#00A087B2", "#3366CC")) + 
  theme_classic() +
  theme(plot.title = element_text(size=18, hjust = 0.5, face="bold"), 
        axis.text = element_text(size=14),
        axis.title = element_text(size=16), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "none") 
dev.off()

pdf('3_LIFESTYLE_PREDICTION//Plots/Relab_temperates_comp.pdf', width=3, height=3.3)
ggplot(Sample_metadata[!is.na(Sample_metadata$Timepoint_categorical),], aes(x=Type, y=Viral_relab_temperates)) +
  geom_violin(aes(fill=Type), alpha=0.7, width=0.8) +
  geom_boxplot(width=0.2, fill="white", color="black") + 
  labs(x = 'Sample type', y = 'Relative abundance (%)') + 
  scale_fill_manual(values=c("#00A087B2", "#3366CC")) + 
  theme_classic() +
  theme(plot.title = element_text(size=18, hjust = 0.5, face="bold"), 
        axis.text = element_text(size=14),
        axis.title = element_text(size=16), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "none") 
dev.off()

###################
# INFANT PLOTS
###################

# Generate INFANT plots for richness and shannon index over time
pdf('2_DIVERSITY/Plots/Shannon_Index_Infants.pdf', width=4.2, height=3.3)
ggplot(Sample_metadata[Sample_metadata$Type=="infant" & !is.na(Sample_metadata$Timepoint_categorical),], 
       aes(x=Timepoint_categorical, y=shannon)) +
  geom_violin(aes(fill=Type), alpha=0.7, width=0.8) +
  geom_boxplot(width=0.2, fill="white", color="black") + 
  labs(x = 'Timepoint', y = 'Shannon Index') + 
  scale_fill_manual(values=c("#00A087B2")) + 
  theme_classic() + 
  ylim(0,6) +
  theme(plot.title = element_text(size=18, hjust = 0.5, face="bold"), 
        axis.text = element_text(size=14),
        axis.title = element_text(size=16), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "none") 
dev.off()

pdf('2_DIVERSITY/Plots/Richness_Infants.pdf', width=4.2, height=3.3)
ggplot(Sample_metadata[Sample_metadata$Type=="infant" & !is.na(Sample_metadata$Timepoint_categorical),],
       aes(x=Timepoint_categorical, y=richness)) +
  geom_violin(aes(fill=Type), alpha=0.7, width=0.8) +
  geom_boxplot(width=0.2, fill="white", color="black") + 
  labs(x = 'Timepoint', y = 'Viral richness') + 
  scale_fill_manual(values=c("#00A087B2")) + 
  theme_classic() + 
  theme(plot.title = element_text(size=18, hjust = 0.5, face="bold"), 
        axis.text = element_text(size=14),
        axis.title = element_text(size=16), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "none") 
dev.off()

# Generate NMDS plot of INFANT samples over time
pdf('2_DIVERSITY/Plots/Viral_vOTU_INFANTS_Bray_NMDS_without_outliers.pdf', width=10/2.54, height=10/2.54)
NMDS_plot_infants <- ggplot(data = data.scores_infants, aes(x = NMDS1, y = NMDS2, color=Timepoint_categorical)) + 
  geom_point(size = 1.5, alpha=0.7) + 
  geom_point(data=centroids_infants, aes(fill=Timepoint_categorical),shape=NA, size=4, color='black') + 
  stat_ellipse(geom = "polygon", alpha = 0.0, 
               aes(group = Timepoint_categorical, color=Timepoint_categorical), linetype = 2, linewidth = 0.8) +
  scale_color_manual(values=c("#0055AA","#C40003","#EAC862", "#7FD2FF", "#007ED3", "#FF9D1E", "#FFACAA")) +
  theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title = element_text(size = 15), 
        panel.background = element_blank(), 
        panel.grid = element_blank(),  
        panel.border = element_rect(fill = NA, linewidth =1.2, colour = "grey30"), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.title = element_text(size = 16, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 16, colour = "grey30"),
        legend.position = "bottom") 
ggMarginal(NMDS_plot_infants, type= "boxplot", groupFill=T)
dev.off()

# Generate plots for the proportion and cumulative relative abundance of temperate phages in INFANTS
pdf('3_LIFESTYLE_PREDICTION/Plots/Prop_temperates_infants.pdf', width=4.2, height=3.3)
ggplot(Sample_metadata[Sample_metadata$Type=="infant" & !is.na(Sample_metadata$Timepoint_categorical),],
       aes(x=Timepoint_categorical, y=prop_temperates, fill = Type)) +
  stat_halfeye(
    width = 0.9, position = "dodge", adjust = 0.3, alpha = 0.7, 
    justification=-.2, .width = 0, point_colour = NA) + 
  geom_boxplot(width=.25, outlier.color = NA, alpha = 0.9) +
  stat_histinterval(slab_color = "gray45",slab_linewidth = 0.3, outline_bars = T,  width = 0.9,
                    position = "dodge",alpha = 0.5, fill = "white",
                    justification=-.2,  breaks = 105, .width = 0) + 
  labs(y="Proportion (%)", x="Timepoint") +
  scale_fill_manual(values=c("#00A087B2")) + 
  theme_classic()  +
  theme(plot.title = element_text(size=18, hjust = 0.5, face="bold"), 
        axis.text = element_text(size=14),
        axis.title = element_text(size=16), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "none") 
dev.off()

pdf('3_LIFESTYLE_PREDICTION/Plots/Relab_temperates_infants.pdf', width=4.2, height=3.3)
ggplot(Sample_metadata[Sample_metadata$Type=="infant" & !is.na(Sample_metadata$Timepoint_categorical),], 
       aes(x=Timepoint_categorical, y=Viral_relab_temperates, fill = Type)) +
  stat_halfeye(
    width = 0.9, position = "dodge", adjust = 0.3, alpha = 0.7, 
    justification=-.2, .width = 0, point_colour = NA) +
  geom_boxplot(width=.25, outlier.color = NA, alpha = 0.9) +
  #geom_point(color = "gray45", alpha = 0.05) +
  stat_histinterval(slab_color = "gray45",slab_linewidth = 0.3, outline_bars = T,  width = 0.9,
                    position = "dodge",alpha = 0.5, fill = "white",
                    justification=-.2,  breaks = 70, .width = 0) +  
  labs(y="Relative abundance (%)", x="Timepoint") +
  scale_fill_manual(values=c("#00A087B2")) + 
  theme_classic()  +
  theme(plot.title = element_text(size=18, hjust = 0.5, face="bold"), 
        axis.text = element_text(size=14),
        axis.title = element_text(size=16), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "none") 
dev.off()

###################
# MATERNAL PLOTS
###################

# Generate MATERNAL plots for richness and shannon index over time 
pdf('2_DIVERSITY/Plots/Shannon_Index_Mothers.pdf', width=4.2, height=3.3)
ggplot(Sample_metadata[Sample_metadata$Type=="mother" & !is.na(Sample_metadata$Timepoint_categorical),],
       aes(x=Timepoint_categorical, y=shannon)) +
  geom_violin(aes(fill=Type), alpha=0.7, width=0.8) +
  geom_boxplot(width=0.2, fill="white", color="black") + 
  labs(x = 'Timepoint', y = 'Shannon Index') + 
  scale_fill_manual(values=c("#3366CC")) + 
  theme_classic() + 
  ylim(0,6) +
  theme(plot.title = element_text(size=18, hjust = 0.5, face="bold"), 
        axis.text = element_text(size=14),
        axis.title = element_text(size=16), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "none") # Remove legend
dev.off()

pdf('2_DIVERSITY/Plots/Richness_Mothers.pdf', width=4.2, height=3.3)
ggplot(Sample_metadata[Sample_metadata$Type=="mother" & !is.na(Sample_metadata$Timepoint_categorical),], 
       aes(x=Timepoint_categorical, y=richness)) +
  geom_violin(aes(fill=Type), alpha=0.7, width=0.8) +
  geom_boxplot(width=0.2, fill="white", color="black") + 
  labs(x = 'Timepoint', y = 'Viral richness') + 
  scale_fill_manual(values=c("#3366CC")) + 
  theme_classic() + 
  theme(plot.title = element_text(size=18, hjust = 0.5, face="bold"), 
        axis.text = element_text(size=14),
        axis.title = element_text(size=16), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "none") # Remove legend
dev.off()

# Generate NMDS plot of MATERNAL samples over time
pdf('2_DIVERSITY/Plots/Viral_vOTU_MOTHERS_Bray_NMDS_without_outliers.pdf', width=10/2.24, height=10/2.54)
NMDS_plot_mothers <- ggplot(data = data.scores_mothers, aes(x = NMDS1, y = NMDS2, color=Timepoint_categorical)) + 
  geom_point(size = 1.7, alpha=0.7) + 
  geom_point(data=centroids_mothers, aes(fill=Timepoint_categorical),shape=NA, size=4, color='black') + 
  stat_ellipse(geom = "polygon", alpha = 0.0, 
               aes(group = Timepoint_categorical, color=Timepoint_categorical), linetype = 2, linewidth = 0.8)+
  scale_color_manual(values=c("#DB7093", "#008000", "#9F79EE", "#7FD2FF")) +
  theme_bw()+
  theme(axis.text=element_text(size=14.5),
        axis.title = element_text(size = 16.5), 
        panel.background = element_blank(), 
        panel.grid = element_blank(), 
        panel.border = element_rect(fill = NA, linewidth =1.2, colour = "grey30"), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.title = element_text(size = 17.5, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 17.5, colour = "grey30"),
        legend.position = "bottom") 
ggMarginal(NMDS_plot_mothers, type= "boxplot", groupFill=T)
dev.off()

# Generate plots for the proportion and cumulative relative abundance of temperate phages in MOTHERS
pdf('3_LIFESTYLE_PREDICTION/Plots/Prop_temperates_mothers.pdf', width=4.2, height=3.3)
ggplot(Sample_metadata[Sample_metadata$Type=="mother" & !is.na(Sample_metadata$Timepoint_categorical),], 
       aes(x=Timepoint_categorical, y=prop_temperates, fill = Type)) +
  stat_halfeye(
    width = 0.9, position = "dodge", adjust = 0.3, alpha = 0.7, 
    justification=-.2, .width = 0, point_colour = NA) + 
  geom_boxplot(width=.25, outlier.color = NA, alpha = 0.9) +
  stat_histinterval(slab_color = "gray45",slab_linewidth = 0.3, outline_bars = T,  width = 0.9,
                    position = "dodge",alpha = 0.5, fill = "white",
                    justification=-.2,  breaks = 75, .width = 0) + 
  labs(y="Proportion (%)", x="Timepoint") +
  scale_fill_manual(values=c("#3366CC")) + 
  theme_classic()  +
  theme(plot.title = element_text(size=18, hjust = 0.5, face="bold"), 
        axis.text = element_text(size=14),
        axis.title = element_text(size=16), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "none") 
dev.off()

pdf('3_LIFESTYLE_PREDICTION/Plots/Relab_temperates_mothers.pdf', width=4.2, height=3.3)
ggplot(Sample_metadata[Sample_metadata$Type=="mother" & !is.na(Sample_metadata$Timepoint_categorical),], 
       aes(x=Timepoint_categorical, y=Viral_relab_temperates, fill = Type)) +
  stat_halfeye(
    width = 0.9, position = "dodge", adjust = 0.3, alpha = 0.7, 
    justification=-.2, .width = 0, point_colour = NA) + 
  geom_boxplot(width=.25, outlier.color = NA, alpha = 0.9) +
  stat_histinterval(slab_color = "gray45",slab_linewidth = 0.3, outline_bars = T,  width = 0.9,
                    position = "dodge",alpha = 0.5, fill = "white",
                    justification=-.2,  breaks = 70, .width = 0) + 
  labs(y="Relative Abundance (%)", x="Timepoint") +
  scale_fill_manual(values=c("#3366CC")) + 
  theme_classic()  +
  theme(plot.title = element_text(size=18, hjust = 0.5, face="bold"), 
        axis.text = element_text(size=14),
        axis.title = element_text(size=16), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "none") 
dev.off()
