library(tidyverse)
library(lidR)
library(lidRmts)

low <- readLAS("viz/low_density.las")
med <- readLAS("viz/med_density.las")
high <- readLAS("viz/high_density.las")


vox.list = list()
for(i in 1:5){
  low.vox <- std_voxel_all(low, i)
  low.vox$den <- "low"
  med.vox <- std_voxel_all(med, i)
  med.vox$den <- "med"
  high.vox <- std_voxel_all(high, i)
  high.vox$den <- "high"
  all.den <- rbind(low.vox, med.vox, high.vox)
  vox.list[[i]] <- cbind(vox_size = i, all.den)
}

all_vox_df <- do.call(rbind, vox.list)

pal = wesanderson::wes_palette("Zissou1")

## with zero

pdf("voxel_histograms_0.pdf")
for(i in 1:ncol(all_vox_df)){
  for(j in 1:5){
    df <- all_vox_df[all_vox_df$vox_size == j, -c(1:4,22:24)]
    hist <- ggplot(df) + geom_histogram(aes(df[,i]), fill = pal[1]) +
      facet_grid(~den) +
      xlab(paste0("res",j,"-", colnames(df)[i])) +
      theme_bw()
    print(hist)
  }
}
dev.off()


## no zero

pdf("voxel_histograms_no0.pdf")
for(i in 1:ncol(all_vox_df)){
  for(j in 1:5){
    df <- all_vox_df[all_vox_df$vox_size == j & all_vox_df$SVi > 0, -c(1:4,22:24)]
    hist <- ggplot(df) + geom_histogram(aes(df[,i]), fill = pal[1]) +
      facet_grid(~den) +
      xlab(paste0("res",j,"-", colnames(df)[i])) +
      theme_bw()
    print(hist)
  }
}
dev.off()

## seperated by Z
pdf("voxel_violin_0.pdf")
for(i in 1:ncol(all_vox_df)){
  for(j in 1:5){
    df <- all_vox_df[all_vox_df$vox_size == j, -c(1:3,22:24)]
    plot <- ggplot(df) + geom_violin(aes(x = as.factor(Z), y = df[,i]), fill = pal[1]) +
      geom_boxplot(width=0.1) +
      coord_flip()+
      facet_grid(~den) +
      ylab(paste0("res",j,"-", colnames(df)[i])) + xlab("Voxel height (m)") +
      theme_bw()
    print(plot)
  }
}
dev.off()






plot(low, colorPalette = viridis::magma(n=50), bg = "white")

