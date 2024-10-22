# 패키지, 데이터, 함수, 모듈 불러오기
source('function_module.R')

#### Define UI for application ----------------
ui <- fluidPage(
  tabsetPanel(
    # 지상 대피소 지도 시각화 
    tabPanel('지상 대피소 지도 시각화',
             theme=bslib::bs_theme(bootswatch='cerulean'),    # cerulean 부트스와치 테마 사용
             titlePanel(h3("24년 8월 생활인구 및 지상 대피소 현황", style='background-color:lightsteelblue', align = "center")),
             geoUI('geo_ground'),
             
             titlePanel(h3("지역별 지상 대피소 수용인원 및 포화율", style='background-color:lightsteelblue', align = "center")),
             tableUI('table_ground')
             
    ),
    # 지하 대피소 지도 시각화 
    tabPanel('지하 대피소 지도 시각화',
             theme=bslib::bs_theme(bootswatch='cerulean'),    # cerulean 부트스와치 테마 사용
             titlePanel(h3("24년 8월 생활인구 및 지상 대피소 현황", style='background-color:lightsteelblue', align = "center")),
             geoUI('geo_underground'),
             
             titlePanel(h3("지역별 지상 대피소 수용인원 및 포화율", style='background-color:lightsteelblue', align = "center")),
             tableUI('table_underground')
    )
  )
)

#### Define server logic required to draw plots ----------------
server <- function(input, output, session) {
  # 지상 대피소 지도 및 테이블 모듈 호출
  geoServer("geo_ground", shelter_type = "ground", 'arrow-circle-up')
  tableServer("table_ground", data = gnd_shelter_strtn_rate)
  
  # 지하 대피소 지도 및 테이블 모듈 호출
  geoServer("geo_underground", shelter_type = "underground", 'arrow-circle-down')
  tableServer("table_underground", data = und_shelter_strtn_rate)
}

# Run the application 
shinyApp(ui = ui, server = server)
