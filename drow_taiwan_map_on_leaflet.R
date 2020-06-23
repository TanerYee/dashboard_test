#0==========================================================
#<parameter>================================================
options(stringsAsFactors = FALSE)
options(scipen = 999)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(gganimate) 
library(gifski)
library(plotly)
library(mefa4)
library(dplyr)
library(gapminder)
library(countrycode)
library(tmap)
library(sp)
library(rworldmap)

library(flexdashboard)
library(shiny)
library(shinydashboard)
library(leaflet)
library(RColorBrewer)
library(highcharter)
library(shinyWidgets)

library(spdep)
library(maptools)
library(rgeos)
library(sf)
#1==========================================================
#<data import>==============================================
tw.map <- sf::st_read(dsn = 'C:\\Users\\Taner\\Dropbox\\covid_19\\20200619_create_ui_server\\TaiwanMap\\COUNTY_MOI_1081121.shp')
taiwan_covid <- read_csv(url('https://data.cdc.gov.tw/zh_TW/download?resourceid=3c1e263d-16ec-4d70-b56c-21c9e2171fc7&dataurl=https://od.cdc.gov.tw/eic/Day_Confirmation_Age_County_Gender_19CoV.csv'),col_names = F,skip = 1)
city_name_convert_df <- data.frame(city_name_ch = c("台北市", "新北市", "桃園市", "高雄市",
                                                    "台中市", "彰化縣", "台南市", "屏東縣",
                                                    "新竹市", "新竹縣", "基隆市", "雲林縣",
                                                    "嘉義市", "苗栗縣", "南投縣", "宜蘭縣",
                                                    "嘉義縣"),
                                   city_name_eng = c('Taipei City','New Taipei City','Taoyuan City','Kaohsiung City',
                                                     'Taichung City','Changhua County','Tainan City','Pingtung County',
                                                     'Hsinchu City','Hsinchu County','Keelung City','Yunlin County',
                                                     'Chiayi City','Miaoli County','Nantou County','Yilan County',
                                                     'Chiayi County'))
#2==========================================================
#<data clean>===============================================
taiwan_covid <- taiwan_covid[,c(2,3,7)]
names(taiwan_covid) <- c('date','city','new_cases')
taiwan_summary_df <- taiwan_covid %>%
  group_by(date,city) %>%
  summarise(new_cases = sum(new_cases))

taiwan_summary_df <- taiwan_summary_df %>%
  group_by(city) %>% mutate(total_case = cumsum(new_cases))

taiwan_summary_df <- taiwan_summary_df %>% arrange(date,city)

final_data <- taiwan_summary_df[rev(!duplicated(rev(taiwan_summary_df$city))),]
final_data <- final_data %>% arrange(-total_case)



final_data <- merge.data.frame(x = final_data,y = city_name_convert_df,
                               by.x = 'city',by.y = 'city_name_ch',all.x = T)
final_data <- final_data %>% select(date,city_name_eng,total_case)

test_df <- merge.data.frame(x = tw.map,y = final_data,
                            by.x = 'COUNTYENG',by.y = 'city_name_eng',all.x = T)

test_df$total_case[is.na(test_df$total_case)] <- 0

test_df[['color']] <- ifelse(test_df$total_case < 10,"#FDDBC7",
                             ifelse(test_df$total_case < 50,"#F4A582",
                                    ifelse(test_df$total_case < 100,"#D6604D",
                                           ifelse(test_df$total_case < 200,"#B2182B","#8B0000"))))
test_df <- st_sf(test_df)
#3==========================================================
#<ui>=======================================================
ui1 <- fluidPage(titlePanel("Hello Shiny!"),
                 mainPanel(leafletOutput(outputId = 'test_map')))
#4==========================================================
#<server>===================================================
mybins <- c(0,10,50,100,200,Inf)
mytext <- paste(
  "Area: ", test_df$COUNTYENG, "<br/>", 
  "Confirmed cases: ", test_df$total_case, 
  sep="") %>%
  lapply(htmltools::HTML)

server1 <- function(input,output){
  output$test_map <- renderLeaflet({
    leaflet(data = test_df,options = leafletOptions(zoomControl = FALSE,minZoom = 7, maxZoom = 7)) %>%
      addTiles() %>% setView(lng = 120.8,lat = 23.5,zoom = 7) %>% 
      addProviderTiles(leaflet::providers$CartoDB.Positron) %>% 
      addPolygons(fillColor = ~color,
                  fillOpacity = 0.9,
                  weight = 0.6,
                  opacity = 1,
                  smoothFactor = 0.5,
                  color = 'black',
                  dashArray = "3",
                  highlightOptions = highlightOptions(weight = 3,
                                                      color = "white",
                                                      dashArray = "",
                                                      fillOpacity = 0.7,
                                                      bringToFront = TRUE),
                  label = mytext,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", direction = "auto")) %>% 
      addEasyButton(
        easyButton(
          icon="fa-globe",
          title="BACK",
          onClick=JS("function(btn, map) {map.setView([23.5,120.8],7);btn.state('zoom-to-forest');}")
          )
        ) %>% 
      addLegend(bins = rev(mybins[1:4]),colors = c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7"),
                labels = c('100-200','50-100','10-50','0-10'),
                opacity=0.9, title = "Confirmed cases", position = "bottomright")
    })
}
#5==========================================================
#<run app>==================================================
shinyApp(ui1,server1)
