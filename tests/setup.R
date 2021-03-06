if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
lodown( "gss" , output_dir = file.path( getwd() ) )

library(survey)

gss_files <-
	list.files(
		file.path( getwd() ) ,
		full.names = TRUE 
	)

gss_rds <-
	grep( 
		"cross sectional cumulative(.*)([0-9][0-9][0-9][0-9])\\.rds$" , 
		gss_files , 
		value = TRUE 
	)
	
gss_df <- readRDS( gss_rds )

# keep only the variables you need
keep_vars <- 
	c( "vpsu" , "vstrat" , "compwt" , "polviews" , 
		"born" , "adults" , "hompop" , "race" , "region" ,
		"age" , "sex" , "one" )
		
gss_df <- gss_df[ keep_vars ] ; gc()
# this step conserves RAM

# https://gssdataexplorer.norc.org/pages/show?page=gss%2Fstandard_error
gss_design <- 
	svydesign( 
		~ vpsu , 
		strata = ~ vstrat , 
		data = gss_df , 
		weights = ~ wtssall , 
		nest = TRUE 
	)
gss_design <- 
	update( 
		gss_design , 

		polviews = 
			factor( polviews ,
				labels = c( "Extremely liberal" , "Liberal" ,
				"Slightly liberal" , "Moderate, middle of the road" ,
				"Slightly conservative" , "Conservative" ,
				"Extremely conservative" )
			) ,
		
		born_in_usa = ifelse( born %in% 1:2 , as.numeric( born == 1 ) , NA ) ,
		
		adults_in_hh = ifelse( adults > 8 , NA , adults ) ,
		
		persons_in_hh = ifelse( hompop > 11 , NA , hompop ) ,
		
		race = factor( race , labels = c( "white" , "black" , "other" ) ) ,
		
		region = 
			factor( region , 
				labels = c( "New England" , "Middle Atlantic" ,
					"East North Central" , "West North Central" ,
					"South Atlantic" , "East South Central" ,
					"West South Central" , "Mountain" , "Pacific" )
			)

	)
sum( weights( gss_design , "sampling" ) != 0 )

svyby( ~ one , ~ region , gss_design , unwtd.count )
svytotal( ~ one , gss_design )

svyby( ~ one , ~ region , gss_design , svytotal )
svymean( ~ age , gss_design , na.rm = TRUE )

svyby( ~ age , ~ region , gss_design , svymean , na.rm = TRUE )
svymean( ~ race , gss_design , na.rm = TRUE )

svyby( ~ race , ~ region , gss_design , svymean , na.rm = TRUE )
svytotal( ~ age , gss_design , na.rm = TRUE )

svyby( ~ age , ~ region , gss_design , svytotal , na.rm = TRUE )
svytotal( ~ race , gss_design , na.rm = TRUE )

svyby( ~ race , ~ region , gss_design , svytotal , na.rm = TRUE )
svyquantile( ~ age , gss_design , 0.5 , na.rm = TRUE )

svyby( 
	~ age , 
	~ region , 
	gss_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ adults_in_hh , 
	denominator = ~ persons_in_hh , 
	gss_design ,
	na.rm = TRUE
)
sub_gss_design <- subset( gss_design , sex == 2 )
svymean( ~ age , sub_gss_design , na.rm = TRUE )
this_result <- svymean( ~ age , gss_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ age , 
		~ region , 
		gss_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( gss_design )
svyvar( ~ age , gss_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ age , gss_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ age , gss_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ born_in_usa , gss_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( age ~ born_in_usa , gss_design )
svychisq( 
	~ born_in_usa + race , 
	gss_design 
)
glm_result <- 
	svyglm( 
		age ~ born_in_usa + race , 
		gss_design 
	)

summary( glm_result )
library(srvyr)
gss_srvyr_design <- as_survey( gss_design )
gss_srvyr_design %>%
	summarize( mean = survey_mean( age , na.rm = TRUE ) )

gss_srvyr_design %>%
	group_by( region ) %>%
	summarize( mean = survey_mean( age , na.rm = TRUE ) )
