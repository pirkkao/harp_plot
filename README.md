This is an R-package designed to make harp-plotting quicker and easier 
via adding a yaml controlled configuration. You don't need to know 
R in order to use this, all that is done behind the scenes via 
R-functions and programs. Some example plots are provided after the 
instructions.

In order to use this, the following R-packages need to be installed
in your local R-installation:
library(harp)
library(dplyr)
library(patchwork)
library(stringr)
library(yaml)
library(scico)

See https://harphub.github.io/harp_training_2024/get-started.html
for full harp installation guide, in short on ATOS:
module load R/4.3.3 or module load R/4.4.3
R
install.packages("remotes")
library(remotes)
install_github("harphub/harp")

All(?) of the other packages can be installed in R by:
install.packages("NAME")
where NAME is one of dplyr, patchwork,...

Alternatively, when running on ATOS, you can try to change the 
R_LIBS_USER in master.bash to "/home/fi3/" instead of "$HOME".
This *might* allow you to use my locally installed R-packages
if you are in the "accord" group.

After all the necessary libraries are present, you can run
this program either from command line (1) or from R (2):
1) ./master.bash CONFIG_NAME
   where CONFIG_NAME is the specific name you have
   given to an experiment setup config-file. The config-file
   names should be constructed as configs/config.CONFIG_NAME.yaml.
2) R
   config_file<-"CONFIG_NAME"
   source("plot_fields.R")

Note for option 2 you need to manually load in the libraries first,
this choice was made so that when rerunning the scripts in R
no library loading happens.

Some example configurations are provided in configs. These
use data from /perm/fi3/data_examples/..." that should
be visible for "accord" user group.

The example configuration headers provide a bit of information
what the example is trying to illustrate, try running all 7
examples and see for yourself what happens and try to understand
why by comparing the different configs. You can go through them:
./master.bash example_det1
./master.bash example_det2
...

The figures are put into fig-folder, for comparison a ready made
set of the same figures is provided in fig_examples-folder.

When you want to start exploring data from your own experiments,
try getting them from ECFS using the get_files.bash script. It
constructs the file naming so that the harp_plot-scripts know
what they are searching for.

Example plots (included in the example config-files):

**Visualizing determistic forecasts example 1**
![Visualizing determistic forecasts example 1](https://github.com/pirkkao/harp_plot/blob/main/fig_examples/example_det1.png)

**Visualizing determistic forecasts example 2**
![Visualizing determistic forecasts example 2](https://github.com/pirkkao/harp_plot/blob/main/fig_examples/example_det2.png)

**Visualizing determistic forecasts example 3**
![Visualizing determistic forecasts example 3](https://github.com/pirkkao/harp_plot/blob/main/fig_examples/example_det3.png)

**Visualizing determistic forecasts example 1**
![Visualizing determistic forecasts example 4](https://github.com/pirkkao/harp_plot/blob/main/fig_examples/example_det4.png)

**Visualizing ensemble products example 1**
![Visualizing ensemble products example 1](https://github.com/pirkkao/harp_plot/blob/main/fig_examples/example_eps1.png)

**Visualizing ensemble products example 2**
![Visualizing ensemble products example 2](https://github.com/pirkkao/harp_plot/blob/main/fig_examples/example_eps2.png)

**Visualizing ensemble products example 3**
![Visualizing ensemble products example 3](https://github.com/pirkkao/harp_plot/blob/main/fig_examples/example_eps3_2025012112+12h.png)
