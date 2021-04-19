library(lodown)
lodown( "brfss" , output_dir = file.path( path.expand( "~" ) , "BRFSS" ) )
# examine all available BRFSS microdata files
brfss_cat <-
  get_catalog( "brfss" ,
               output_dir = file.path( path.expand( "~" ) , "BRFSS" ) )

# 2016 only
brfss_cat <- subset( brfss_cat , year == 2016 )
# download the microdata to your local computer
brfss_cat <- lodown( "brfss" , brfss_cat )

options( survey.lonely.psu = "adjust" )

library(survey)

brfss_df <- 
  readRDS( file.path( path.expand( "~" ) , "BRFSS" , "2016 main.rds" ) )

variables_to_keep <-
  c( 'one' , 'xpsu' , 'xststr' , 'xllcpwt' , 'genhlth' , 'medcost' , 
     'xstate' , 'xage80' , 'nummen' , 'numadult' , 'hlthpln1' )

brfss_df <- brfss_df[ variables_to_keep ] ; gc()

brfss_design <-
  svydesign(
    id = ~ xpsu ,
    strata = ~ xststr ,
    data = brfss_df ,
    weight = ~ xllcpwt ,
    nest = TRUE
  )