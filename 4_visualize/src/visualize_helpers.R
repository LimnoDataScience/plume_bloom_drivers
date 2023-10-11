
make_sediment_heatmap <- function(in_file, out_file, mission, lake_sf) {
  
  common_crs <- 'epsg:4326'
  
  # Load sediment data & reproject to a common crs
  sediment_raster <- load_terraqs(in_file)
  sediment_raster_proj <- project(sediment_raster, common_crs)
  
  # Reproject lake watershed shape to a common crs
  lake_sup_ws <- st_transform(lake_sf, crs=common_crs)
  
  p<-ggplot() +
    tidyterra::geom_spatraster(data = sediment_raster_proj) +
    geom_sf(data = lake_sup_ws, fill = '#9ba68e', color = NA) +
    # Not plotting NAs of the raster
    tidyterra::scale_fill_whitebox_c(
      palette = "arid", 
      direction = -1,
      n.breaks = 5
    ) +
    labs(fill="sediment") +
    theme_void() +
    theme(plot.background = element_rect(fill='#c3d2e0', color=NA),
          panel.background = element_rect(fill='#c3d2e0', color=NA)) +
    ggtitle(sprintf('Sediment heatmap for %s data', mission))
  
  ggsave(out_file, p, dpi=100, width=800, height=600, units='px')
  return(out_file)
}
