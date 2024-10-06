# 패키지, 데이터, 함수, 모듈 불러오기
source('function_module.R')

#### Define UI for application ----
ui <- fluidPage(
  tabsetPanel(
    # 지상 대피소 지도 시각화  -----------------------------------------------------------------------------------  
    tabPanel('지상 대피소 지도 시각화',
             theme=bslib::bs_theme(bootswatch='cerulean'),    # cerulean 부트스와치 테마 사용
             titlePanel(h3("24년 8월 생활인구 및 지상 대피소 현황", style='background-color:lightsteelblue', align = "center")),
             geoUI('geo_ground'),
             
             titlePanel(h3("지역별 지상 대피소 수용인원 및 포화율", style='background-color:lightsteelblue', align = "center")),
             tableUI('table_ground')
             
    ),
    
    # 지하 대피소 지도 시각화  -----------------------------------------------------------------------------------  
    tabPanel('지하 대피소 지도 시각화',
             theme=bslib::bs_theme(bootswatch='cerulean'),    # cerulean 부트스와치 테마 사용
             titlePanel(h3("24년 8월 생활인구 및 지상 대피소 현황", style='background-color:lightsteelblue', align = "center")),
             geoUI('geo_underground'),
             
             titlePanel(h3("지역별 지상 대피소 수용인원 및 포화율", style='background-color:lightsteelblue', align = "center")),
             tableUI('table_underground')
             
    )
  )
)

#### Define server logic required to draw plots ----
server <- function(input, output, session) {
  
  # 지상 대피소 지도 및 테이블 모듈 호출
  geoServer("geo_ground", shelter_type = "ground", school_icon)
  tableServer("table_ground", data = abv_shelter_strtn_rate)
  
  # 지하 대피소 지도 및 테이블 모듈 호출
  geoServer("geo_underground", shelter_type = "underground", building_icon)
  tableServer("table_underground", data = und_shelter_strtn_rate)
}


# Run the application 
shinyApp(ui = ui, server = server)


# #### Define UI for application ----
# ui <- fluidPage(
#   tabsetPanel(
#     # 지상 대피소 지도 시각화  -----------------------------------------------------------------------------------  
#     tabPanel('지상 대피소 지도 시각화',
#              theme=bslib::bs_theme(bootswatch='cerulean'),  # cerulean 부트스와치 테마 사용
#              titlePanel(h3("24년 8월 생활인구 및 지상 대피소 현황", style='background-color:lightsteelblue', align = "center")),
#              fluidRow( 
#                column(9, leafletOutput("plot_ground", height = '700px')),             # plotOutput(outputId = 'plot', height = '500px', width = '800px')
#                column(3, selectInput('ppl1', '인구수/인구밀도', choices = ppl, selected = 'avg_people'),
#                          selectInput('holiday1_1', '평일/공휴일', choices = holiday, selected = '평일'),
#                          selectInput('times1_1', '시간대구분', choices = times, selected = '12'),
#                          # 대피소 표시를 위한 체크박스
#                          checkboxInput('show_shelters1', "대피소 표시", FALSE),
#                          # 대피소 표시 시 반경 선택 메뉴 표시
#                          conditionalPanel(
#                            condition = "input.show_shelters1 == true",
#                            selectInput('radius1', '반경', choices = radius, selected = 0),
#                            # 대피소와 집계구 연결을 위한 체크박스
#                            checkboxInput('connect1', "집계구 중심점과 대피소 연결", FALSE)
#                          )
#                       )
#              ),
#              titlePanel(h3("지역별 지상 대피소 수용인원 및 포화율", style='background-color:lightsteelblue', align = "center")),
#              fluidRow(
#                column(4, selectInput('gu_nm1', '지역구', choices = gu_nm, selected = '중구')),
#                column(4, selectInput('holiday1_2', '평일/공휴일', choices = holiday, selected = '평일')),
#                column(4, selectInput('times1_2', '시간대구분', choices = times, selected = '12')),
#                column(12, dataTableOutput('table1'))
#              )
#   
#     ),
#     
#     # 지하 대피소 지도 시각화  -----------------------------------------------------------------------------------  
#     tabPanel('지하 대피소 지도 시각화',
#              theme=bslib::bs_theme(bootswatch='cerulean'),  # cerulean 부트스와치 테마 사용
#              titlePanel(h3("24년 8월 생활인구 및 지상 대피소 현황", style='background-color:lightsteelblue', align = "center")),
#              fluidRow( 
#                column(9, leafletOutput("plot_underground", height = '700px')),             # plotOutput(outputId = 'plot', height = '500px', width = '800px')
#                column(3, selectInput('ppl2', '인구수/인구밀도', choices = ppl, selected = 'avg_people'),
#                       selectInput('holiday2_1', '평일/공휴일', choices = holiday, selected = '평일'),
#                       selectInput('times2_1', '시간대구분', choices = times, selected = '12'),
#                       # 대피소 표시를 위한 체크박스
#                       checkboxInput('show_shelters2', "대피소 표시", FALSE),
#                       # 대피소 표시 시 반경 선택 메뉴 표시
#                       conditionalPanel(
#                         condition = "input.show_shelters2 == true",
#                         selectInput('radius2', '반경', choices = radius, selected = 0),
#                         # 대피소와 집계구 연결을 위한 체크박스
#                         checkboxInput('connect2', "집계구 중심점과 대피소 연결", FALSE)
#                       )
#                )
#              ),
#              titlePanel(h3("지역별 지상 대피소 수용인원 및 포화율", style='background-color:lightsteelblue', align = "center")),
#              fluidRow(
#                column(4, selectInput('gu_nm2', '지역구', choices = gu_nm, selected = '중구')),
#                column(4, selectInput('holiday2_2', '평일/공휴일', choices = holiday, selected = '평일')),
#                column(4, selectInput('times2_2', '시간대구분', choices = times, selected = '12')),
#                column(12, dataTableOutput('table2'))
#              )
#              
#     )
#   )
# )
# 
# #### Define server logic required to draw plots ----
# server <- function(input, output, session) {
#   
#   # 공간 데이터 반응형 변수 정의
#   ppl1 <- reactive(input$ppl1)
#   holiday1_1 <- reactive(input$holiday1_1)
#   times1_1 <- reactive(input$times1_1)
#   radius1 <- reactive(as.numeric(input$radius1))
#   
#   # 테이블 데이터 반응형 변수 정의
#   gu_nm1 <- reactive(input$gu_nm1)
#   holiday1_2 <- reactive(input$holiday1_2)
#   times1_2 <- reactive(input$times1_2)
#   
#   # 반응형 데이터 필터링(지상)
#   filtered_data1 <- reactive({
#     living_people_abv_grid_4326 |> 
#       filter(평일_휴일 == holiday1_1(), 시간대구분 == times1_1())
#   })
#   
#   # 지도 렌더링(지상)
#   output$plot_ground <- renderLeaflet({
#     req(filtered_data1())   # 필터링된 데이터가 존재하는지 확인
#     
#     pal1 <- colorNumeric(palette = "Reds", domain = filtered_data1()[[ppl1()]])
#     legend_title1 <- names(ppl[ppl == ppl1()])
#     
#     leaflet(filtered_data1()) %>%
#       addTiles() %>%
#       setView(lng = 126.9779451, lat = 37.5662952, zoom = 14) %>%
#       addPolygons(
#         fillColor = ~pal1(get(ppl1())),
#         color = "green",
#         weight = 0.5,
#         opacity = 0.5,
#         fillOpacity = 0.6,
#         popup = ~paste0(시간대구분, "시간대<br>평균인구: ", round(avg_people, 0), "명")
#       ) %>% 
#       addLegend(
#         pal = pal1, 
#         values = ~get(ppl1()), 
#         opacity = 0.5,
#         title = legend_title1,
#         position = "bottomright"
#       )
#   })
#   
#   # 지상 대피소 표시 토글
#   observeEvent(input$show_shelters1, {
#     if (!input$show_shelters1) {
#       # 대피소 표시 해제 시, 반경과 연결선 초기화
#       resetShelterInputs(session, "ground")
#       # 맵에서 대피소, 반경, 연결선 제거
#       updateShelters(FALSE, FALSE, 0, "ground")
#     } else {
#       # 대피소 표시 활성화 시, 기본값으로 설정 (반경 없음, 연결선 없음)
#       updateShelters(TRUE, FALSE, 0, "ground")
#     }
#   })
#   
#   # 지상 반경 변경 시 대피소 반경 업데이트
#   observeEvent(radius1(), {  
#     if (input$show_shelters1) {
#       updateShelters(TRUE, input$connect1, radius1(), "ground")
#     }
#   })
#   
#   # 지상 집계구 연결 토글
#   observeEvent(input$connect1, {
#     updateShelters(input$show_shelters1, input$connect1, radius1(), "ground")
#   })
#   
#   # 데이터 테이블 렌더링 (지상)
#   output$table1 <- renderDataTable({
#     shelter_table <- abv_shelter_strtn_rate %>%
#       filter(
#         shelter_gu_nm == gu_nm1(),
#         평일_휴일 == holiday1_2(),
#         시간대구분 == times1_2()
#       ) %>%
#       select(-geometry)
#     
#     datatable(shelter_table, options = list(pageLength = 10))
#   })
#   
#   
#   
#   
#   # 공간 데이터 반응형 변수 정의
#   ppl2 <- reactive(input$ppl2)
#   holiday2_1 <- reactive(input$holiday2_1)
#   times2_1 <- reactive(input$times2_1)
#   radius2 <- reactive(as.numeric(input$radius2))
#   
#   # 테이블 데이터 반응형 변수 정의
#   gu_nm2 <- reactive(input$gu_nm2)
#   holiday2_2 <- reactive(input$holiday2_2)
#   times2_2 <- reactive(input$times2_2)
#   
#   # 반응형 데이터 필터링 (지하)
#   filtered_data2 <- reactive({
#     living_people_abv_grid_4326 |> 
#       filter(평일_휴일 == holiday2_1(), 시간대구분 == times2_1())
#   })
#   
#   # 지도 렌더링(지하)
#   output$plot_underground <- renderLeaflet({
#     req(filtered_data2())   # 필터링된 데이터가 존재하는지 확인
#     
#     pal2 <- colorNumeric(palette = "Reds", domain = filtered_data2()[[ppl2()]])
#     legend_title2 <- names(ppl[ppl == ppl2()])
#     
#     leaflet(filtered_data2()) %>%
#       addTiles() %>%
#       setView(lng = 126.9779451, lat = 37.5662952, zoom = 14) %>%
#       addPolygons(
#         fillColor = ~pal2(get(ppl2())),
#         color = "green",
#         weight = 0.5,
#         opacity = 0.5,
#         fillOpacity = 0.6,
#         popup = ~paste0(시간대구분, "시간대<br>평균인구: ", round(avg_people, 0), "명")
#       ) %>% 
#       addLegend(
#         pal = pal2, 
#         values = ~get(ppl2()), 
#         opacity = 0.5,
#         title = legend_title2,
#         position = "bottomright"
#       )
#   })
#   
#   
#   # 지하 대피소 표시 토글
#   observeEvent(input$show_shelters2, {
#     if (!input$show_shelters2) {
#       # 대피소 표시 해제 시, 반경과 연결선 초기화
#       resetShelterInputs(session, "underground")
#       # 맵에서 대피소, 반경, 연결선 제거
#       updateShelters(FALSE, FALSE, 0, "underground")
#     } else {
#       # 대피소 표시 활성화 시, 기본값으로 설정 (반경 없음, 연결선 없음)
#       updateShelters(TRUE, FALSE, 0, "underground")
#     }
#   })
#   
#   # 지하 반경 변경 시 대피소 반경 업데이트
#   observeEvent(radius2(), {  
#     if (input$show_shelters2) {
#       updateShelters(TRUE, input$connect2, radius2(), "underground")
#     }
#   })
#   
#   # 지하 집계구 연결 토글
#   observeEvent(input$connect2, {
#     updateShelters(input$show_shelters2, input$connect2, radius2(), "underground")
#   })
#   
#   
#   # 데이터 테이블 렌더링 (지하)
#   output$table2 <- renderDataTable({
#     shelter_table <- und_shelter_strtn_rate %>%
#       filter(
#         shelter_gu_nm == gu_nm2(),
#         평일_휴일 == holiday2_2(),
#         시간대구분 == times2_2()
#       ) %>%
#       select(-geometry)
#     
#     datatable(shelter_table, options = list(pageLength = 10))
#   })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
