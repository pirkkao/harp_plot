# 
# A plotting interface to harp
#

# Do not load libraries if running interactively
if (!base::interactive()) {
    library(harp)
    library(dplyr)
    library(patchwork)
    library(stringr)
    library(yaml)
    library(scico)
    
    # Get config name from sys
    config_file<-Sys.getenv("config_file")

} else {
    # Give config name manually
    config_file<-"example1"
}
# Load local functions
source("coref.R")

# Load exp specific and common templates
case <- yaml.load_file(paste0("configs/config.",config_file,".yaml"))
tmpl <- yaml.load_file("configs/config.common.yaml")

# Parse config file from case + tmpl
config <- f_parse_yaml(case,tmpl)

# Create data structure
f <- f_data_layer(config)
# Do decumulation of field values if requested
fd <- f_decum(f,config)
# Do data differences if requested
fd <- f_data_diff(fd,config)

# Plot every figure
fh <- p_plot_master(fd,config)
# Arrange figures into rows and columns
sh <- p_plot_arrange(fh,config)
# Save the produced figure
p_save_fig(sh,config)
