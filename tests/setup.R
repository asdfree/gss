# chat about who will
# be allowed marriage, children.
# first date questionnaire
library(haven)

zip_tf <- tempfile()

zip_url <- "https://gss.norc.org/Documents/sas/GSS_sas.zip"
	
download.file( zip_url , zip_tf , mode = 'wb' )

unzipped_files <- unzip( zip_tf , exdir = tempdir() )

gss_tbl <- read_sas( grep( '\\.sas7bdat$' , unzipped_files , value = TRUE ) )

gss_df <- data.frame( gss_tbl )

names( gss_df ) <- tolower( names( gss_df ) )

gss_df[ , 'one' ] <- 1
# gss_fn <- file.path( path.expand( "~" ) , "GSS" , "this_file.rds" )
# saveRDS( gss_df , file = gss_fn , compress = FALSE )
# gss_df <- readRDS( gss_fn )
library(survey)

options( survey.lonely.psu = "adjust" )

gss_design <- 
	svydesign( 
		~ vpsu , 
		strata = ~ interaction( year , vstrat ) , 
		data = subset( gss_df , year >= 1975 & !is.na( wtssnrps ) ) , 
		weights = ~ wtssnrps , 
		nest = TRUE 
	)
gss_design <- 
	update( 
		gss_design , 

		polviews = 
			factor( polviews , levels = 1:7 ,
				labels = c( "Extremely liberal" , "Liberal" ,
				"Slightly liberal" , "Moderate, middle of the road" ,
				"Slightly conservative" , "Conservative" ,
				"Extremely conservative" )
			) ,
		
		born_in_usa = as.numeric( born == 1 ) ,
		
		race = factor( race , levels = 1:3 , labels = c( "white" , "black" , "other" ) ) ,
		
		region = 
			factor( region , levels = 1:9 ,
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
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ adults , 
	denominator = ~ hompop , 
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
stopifnot( nrow( subset( gss_design , year == 2021 ) ) == 4032 )
library(srvyr)
gss_srvyr_design <- as_survey( gss_design )
gss_srvyr_design %>%
	summarize( mean = survey_mean( age , na.rm = TRUE ) )

gss_srvyr_design %>%
	group_by( region ) %>%
	summarize( mean = survey_mean( age , na.rm = TRUE ) )
