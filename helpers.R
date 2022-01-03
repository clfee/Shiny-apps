## data extracted from Johns Hopkins data obtained from following Github repository
## Some ideas come from https://github.com/eparker12/, Edward Parker and Quentic Leclerc, London School of Hygiene & Tropical Medicine
# https://github.com/CSSEGISandData/COVID-19


# load libraries
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")



# function to update jhu input data according to mapping base format
update_jhu = function(input_df, tag) {
  names(input_df)[1:2] = c("Province", "Country")
  input_df$Country[input_df$Province=="Hong Kong"] = "Hong Kong"
  input_df$Country[input_df$Province=="Macau"] = "Macao"
  input_df$Country[input_df$Country=="Taiwan*"] = "Taiwan"
  input_df$Country[input_df$Country=="Korea, South"] = "RepublicofKorea"
  input_df$Country[input_df$Country=="Congo (Brazzaville)" | input_df$Country=="Republic of the Congo"] = "Congo"
  input_df$Country[input_df$Country=="Congo (Kinshasa)"] = "Democratic Republic of the Congo"
  input_df$Country[input_df$Country=="Cote d'Ivoire"] = "CotedIvoire"
  input_df$Country[input_df$Country=="Gambia, The"] = "TheGambia"
  input_df$Country[input_df$Country=="Bahamas, The"] = "TheBahamas"
  input_df$Country[input_df$Country=="Cabo Verde"] = "CapeVerde"
  input_df$Country[input_df$Country=="Timor-Leste"] = "TimorLeste"
  input_df$Country[input_df$Country=="Guinea-Bissau"] = "GuineaBissau"
  input_df$Country = input_df$Country %>% str_replace_all(., " ", "") 
  dates = names(input_df)[which(names(input_df)=="1/22/20"):ncol(input_df)]
  input_df = input_df %>% 
    dplyr::select(-c(Province, Lat, Long)) %>% 
    group_by(Country) %>% 
    summarise_each(funs(sum))%>%
    data_frame()
  #rownames(input_df) = input_df$Country
  df_colnames = input_df$Country
  #rownames(input_df) = paste0(input_df$Country,"_",tag)
  input_df = input_df %>% dplyr::select(-c(Country)) %>% t()
  input_df = data.frame(input_df)
  colnames(input_df) = df_colnames
  input_df$Date = dates
  rownames(input_df) = 1:nrow(input_df)
  input_df$Date = format(as.Date(input_df$Date,"%m/%d/%y"))
  input_df
}

#----------------- organised daily data -------------------

daily_data <- function(data,days = 100){
  daily_case <- function(x){c(NA,diff(x,1))}
  df <- reshape2::dcast(data, Date ~  country , value.var="case")
  df <- na.omit(df)
  n     <- nrow(df)
  n_c   <- ncol(df)-3
  df    <- df[(n-days):n, ]
  info <- na.omit(as.data.frame(sapply(df[2:n_c], daily_case)))#%>% t()
  info <- data.frame(info)
  info$Date <- df$Date[-1]
  info
}

data_mapx <- function(data = data_globe,days = 7){
  map_info <- daily_data(data, days)
  countryx <- colnames(map_info )
  map_info <- map_info %>% t()
  map_info <- data.frame(map_info)
  map_info <- as.data.frame(lapply(map_info,as.numeric))
  colnames(map_info) <- paste0('day_', c(1:days))
  map_info           <- map_info %>%
    mutate(Past_ndays = rowSums(.))%>%
    mutate(Past_ndays = ifelse(Past_ndays < 0, 0,as.numeric(Past_ndays)/days))%>%
    mutate(country = countryx)%>% 
    left_join(.,case_region[,c('country','latitude','longitude','population' )], by = 'country')%>%
    mutate(pop_infect = Past_ndays/population )%>%
    mutate(Past_ndays_count = ifelse(Past_ndays > 10000,1, 
                                     ifelse(Past_ndays > 5000, 2,
                                            ifelse(Past_ndays > 1000, 3, 
                                                   ifelse(Past_ndays > 500, 4, 5)))))
  map_info
}


# load latest Covid-2019 data: confirmed cases
url_jhu   <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
jhu_cases <- as.data.frame(data.table::fread(url_jhu))
jhu_cases = subset(jhu_cases, !is.na(Lat))
jhu_cases[is.na(jhu_cases)]=0
total_cases <- sum(jhu_cases[,ncol(jhu_cases)])
jhu_cases = update_jhu(jhu_cases, "cases")
jhu_cases <- jhu_cases %>% mutate(Date = as.Date(Date))
if (total_cases!=sum(jhu_cases[nrow(jhu_cases),1:(ncol(jhu_cases)-1)])) { stop(paste0("Error: incorrect processing - total counts do not match")) }

# ------------------------------------ ------------------------------------ ------------------------------------ ------------------------------------

# map region to the dataset
regions <- 'https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/countries_codes_and_coordinates.csv'
case_region <- as.data.frame(data.table::fread(regions))[,-c(1:2)]
case_region <- case_region %>%
               mutate(country = jhu_ID)

data_globex <- reshape2::melt(jhu_cases , id = c('Date'))%>% 
  mutate(country = variable)%>% 
  mutate(case = value)%>% 
  dplyr::select(-c(value,variable))

data_globe <- left_join(data_globex, case_region,  by = 'country' )%>% 
  dplyr::select(c(Date, case, country, population,latitude,longitude,continent_level))


map_info <- data_mapx(data = data_globe, days = 7)
daily           <- daily_data(data = data_globe, days = 1)
#---------------------- histogram -------------------------
World_new_cases <- sum(as.numeric(na.omit(daily[1,])))
row_nmes <- colnames(daily)
daily    <- data.frame(t(daily))
#daily    <- na.omit(daily)
#is.na(daily) <- 0
colnames(daily)[1] <- 'case'  
daily$country   <- row_nmes
daily$case      <- ifelse(as.numeric(daily$case) < 1, 0, as.numeric(daily$case))
daily           <- arrange(daily, desc(as.numeric(daily$case)))
tw1             <- which(daily$country=='Taiwan')

max_country     <- daily$country[c(1:14,tw1)]  
max_country_no  <- which(map_info$country %in% max_country)
change_pct      <- round(100*((map_info$day_7[max_country_no]/map_info$Past_ndays[max_country_no])-1),1) 
daily1         <- data.frame(cbind(daily[c(1:14,tw1),],map_info$Past_ndays[max_country_no],change_pct))
colnames(daily1)[3:4] <- c('week_avg','change')
daily1          <- arrange(daily1, desc(as.numeric(case)))

# ----------------- value box outputs ---------------------- 
#World_new_cases <- sum(as.numeric(daily$case))
top_pct     <- daily1[c(1:4,15),]  


# ------------------------------------ ------------------------------------ ------------------------------------ ------------------------------------
# Function used bellow is copied from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}