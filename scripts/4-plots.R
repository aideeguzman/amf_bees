## *****************************************************************************
## plots #######################################################################
## *****************************************************************************

# 1. plant traits
# 2. AMF traits
# 3. floral traits
# 4. bee visitation



## *****************************************************************************
## color settings ##############################################################
## *****************************************************************************

amf_csr_colors <- c("#727272","#A06600","#DD9300","#FCBE42","#004D40") 

## *****************************************************************************
## 1. plant traits ###########################################################
## *****************************************************************************

plant_grid_csr <- plot_grid_function(variables=c("shoot (g)", "root (g)", "root:shoot"), 
                                     subset=c("amf_csr_p"),
                                     data=mean_se,
                                     colors=amf_csr_colors)

ggsave("Figure 2.pdf", plot=plant_grid_csr, path=fig.path, width=9.75, height=3.5, useDingbats=FALSE)

## *****************************************************************************
## 2. amf traits ##############################################################
## *****************************************************************************

amf_grid_csr <- plot_grid_function(variables=c("hyphal length (m/g)", "% root colonization",
                                               "root colonization:hyphae", "spore count (grains/ml)"), 
                                   subset=c("amf_csr_p"),
                                   data=mean_se,
                                   colors=amf_csr_colors, rows = 2)

ggsave("Figure 3.pdf", plot=amf_grid_csr, path=fig.path, width=7, height=6, useDingbats=FALSE)


## *****************************************************************************
## 3. floral traits ###########################################################
## *****************************************************************************

floral_grid_csr <- plot_grid_function(variables=c("flower size (cm)", "number of flowers", 
                                                  "nectar volume (µl)", "nectar sugar (%brix)",
                                                  "pollen density (grains/µl)", "pollen protein (mg/l)"), 
                                      subset=c("amf_csr_p"),
                                      data=mean_se,
                                      colors=amf_csr_colors, rows = 3)


ggsave("Figure 4.pdf", plot=floral_grid_csr, path=fig.path, width=7, height=8.25, useDingbats=FALSE)

## *****************************************************************************
## 4. bee visitation ##########################################################
## *****************************************************************************

bv_grid_csr <- plot_grid_function(variables=c("number of bee visits", 
                                              "bee visitation time (s)"), 
                                  subset=c("amf_csr_p"),
                                  data=mean_se,
                                  colors=amf_csr_colors, rows = 1)

ggsave("Figure 5.pdf", plot=bv_grid_csr, path=fig.path, width=7, height=3.5, useDingbats=FALSE)
