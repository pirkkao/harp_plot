library(harp)
library(dplyr)
library(patchwork)
library(stringr)
library(yaml)
library(scico)

# Get local functions
source("coref.R")

# Get config name or give one locally
config_file<-Sys.getenv("config_file")
if(config_file==""){config_file<-"example1"}

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
sh <- p_plot_arrange(fh)
# Save the produced figure
p_save_fig(sh,config)
