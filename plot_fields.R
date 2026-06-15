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
    library(ggnewscale)
    library(devtools)
    
    # Get config name from sys
    config_file<-Sys.getenv("config_file")

} else {
    # Give config name manually
    config_file<-"example_det1"
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
# Expand config if multiple parameters requested per data source
config<-f_copy_config(config)
# Do decumulation of field values if requested
fd <- f_decum(f,config)
# Do data operations if requested
fd <- f_data_oper(fd,config)
# Do data differences if requested
fd <- f_data_diff(fd,config)

# Plot every figure
fh <- p_plot_master(fd,config)
# Arrange figures into rows and columns
sh <- p_plot_arrange(fh,config)
# Save the produced figure
p_save_fig(sh,config)
