# 필요한 패키지 로드
library(tidyverse)
library(sf)
library(sp)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(shinythemes)
library(DT) 
library(bslib)

#### 1. 데이터 불러오기
load("../data/munging_data.RData")

#### 2. Setting Up Named Vectors for UI Elements
# 인구수/인구밀도
ppl <- setNames(c('avg_people', 'avg_ppl_per_ar'), c('인구수', '인구밀도'))
# 평일 공휴일 지정
holiday <- setNames(c('평일', '휴일'), c('평일', '휴일'))
# 시간대 지정
times <- setNames(c('00', '06', '12', '18'), paste0(seq(0, 18, 6), '시간대'))
# 반경
radius <- setNames(c(0, 250, 500, 1000, 1500), c('None', paste0(c(250, 500, 1000, 1500), 'M')))
# 지역구 이름
gu_nm <- unique(abv_shelters_4326$gu_nm)

#### 3.Defining Awesome Icons for Map Markers
# 지상 대피소를 위한 Awesome Icons 정의
school_icon <- awesomeIcons(
  icon = 'university',             # 아이콘 이름만 사용 (Font Awesome 아이콘)
  iconColor = 'white',             # 아이콘 색상
  library = 'fa',                  # 아이콘 라이브러리 ('glyphicon' 또는 'fa')
  markerColor = 'blue')  # 마커색상

# 지상 대피소를 위한 Awesome Icons 정의
building_icon <- awesomeIcons(
  icon = 'building',           # 건물 아이콘 사용
  iconColor = 'white',         # 아이콘 색상
  library = 'fa',              # Font Awesome 라이브러리 사용
  markerColor = 'blue')    # 마커 색상


#### 4.대피소 및 반경 업데이트 함수
updateShelters <- function(show, connect, gu, radius, plot_id, shelter_type, icons) {
  # shelter_type: "ground" 또는 "underground"
  
  # 그룹명 설정
  shelters_group <- paste0("shelters_", shelter_type)
  circles_group <- paste0("circles_", shelter_type)
  polylines_group <- paste0("polylines_", shelter_type)
  
  # 데이터셋 설정
  if (shelter_type == "ground") {
    shelters_data <- abv_shelters_4326 |> filter(gu_nm == gu)
    lines_data <- abv_lines_sf |> filter(gu_nm == gu)
  } else if (shelter_type == "underground") {
    shelters_data <- und_shelters_4326 |> filter(gu_nm == gu)
    lines_data <- und_lines_sf |> filter(gu_nm == gu)
  } else {
    stop("Invalid shelter_type. Must be 'ground' or 'underground'.")
  }
  
  tryCatch({
    message(sprintf("updateShelters called with show=%s, connect=%s, radius=%s, shelter_type=%s", 
                    show, connect, radius, shelter_type))
    
    proxy <- leafletProxy(plot_id)
    
    if (show) {
      # 대피소 마커 추가 ----------
      proxy |>
        addAwesomeMarkers(
          data = shelters_data,
          icon = icons,
          label = ~fclty_nm,
          popup = ~paste("수용면적:", fclty_ar, "<br>수용인원:", psbl_num),
          clusterOptions = markerClusterOptions(maxClusterRadius = 30),
          group = shelters_group
        )
      
      # 반경 그룹 먼저 제거 후 새로 추가
      proxy %>%
        clearGroup(circles_group)
      
      # 반경이 0보다 클 때만 추가 ----------
      if (radius > 0) {
        proxy |>
          addCircles(
            data = shelters_data,
            radius = radius,
            color = "blue",
            fillColor = "blue",
            stroke = FALSE,
            fillOpacity = 0.2,
            group = circles_group
          )
      } else {
        proxy |> clearGroup(circles_group)
      }
      
      # 집계구 중심과 대피소 연결 ----------
      if (connect) {
        proxy |>
          # clearGroup(polylines_group) %>%  # 기존 polylines 그룹 제거
          addPolylines(
            data = lines_data,
            color = "purple",
            weight = 1,
            opacity = 0.7,
            group = polylines_group
          )
      } else {
        proxy |> clearGroup(polylines_group)
      }
    
    # # 대피소 마커 및 반경 제거 ----------    
    } else {   
      proxy %>%
        clearGroup(shelters_group) %>%
        clearGroup(circles_group) %>%
        clearGroup(polylines_group)
    }
    # 성공 시 사용자에게 알림
    # showNotification(paste0(shelter_type, " 대피소가 성공적으로 업데이트되었습니다."), type = "message")
  }, error = function(e) {
    showNotification(paste0(shelter_type, " 대피소 업데이트 중 오류가 발생했습니다: ", e$message), type = "error")
    message("Error in updateShelters: ", e$message)
  })
}

#### 5. 모듈 UI 
# 지도 UI 모듈 함수 
geoUI <- function(id){
  ns <- NS(id)
  fluidRow( 
    column(9, leafletOutput(ns("plot"), height = '700px')),           
    column(3, selectInput(ns('gu_nm'), '지역구', choices = gu_nm, selected = '중구'),
              selectInput(ns('ppl'), '인구수/인구밀도', choices = ppl, selected = 'avg_people'),
              selectInput(ns('holiday'), '평일/공휴일', choices = holiday, selected = '평일'),
              selectInput(ns('times'), '시간대구분', choices = times, selected = '12'),
              # 대피소 표시를 위한 체크박스
              checkboxInput(ns('show_shelters'), "대피소 표시", FALSE),
              # 대피소 표시 시 반경 선택 메뉴 표시
              conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("show_shelters")),
                selectInput(ns('radius'), '반경', choices = radius, selected = 0),
              # 대피소와 집계구 연결을 위한 체크박스
              checkboxInput(ns('connect'), "집계구 중심점과 대피소 연결", FALSE)
              )
           )
  )
}

# 테이블 UI 모듈 함수
tableUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(4, selectInput(ns('gu_nm'), '지역구', choices = gu_nm, selected = '중구')),
    column(4, selectInput(ns('holiday'), '평일/공휴일', choices = holiday, selected = '평일')),
    column(4, selectInput(ns('times'), '시간대구분', choices = times, selected = '12')),
    column(12, dataTableOutput(ns('table')))
  )
}

#### 6. 모듈 server 
# 지도 server 모듈 함수
geoServer <- function(id, shelter_type = 'ground', icons) {
  moduleServer(id, function(input, output, session){ 
    ns <- session$ns
    
    # 반응형 표현식
    gu_input <- reactive(input$gu_nm)
    ppl_input <- reactive(input$ppl)
    holiday_input <- reactive(input$holiday)
    times_input <- reactive(input$times)
    radius_input <- reactive(as.numeric(input$radius))
    
    # 대피소 입력 초기화 함수 모듈 내 정의
    resetShelterInputs <- function() {
      updateSelectInput(session, "radius", selected = 0)
      updateCheckboxInput(session, "connect", value = FALSE)
    }
    
    # 데이터 필터링
    filtered_data <- reactive({
      req(living_people_abv_grid_4326)
      req(gu_input(), holiday_input(), times_input())
      
      living_people_abv_grid_4326 |> 
        filter(SIGUNGU_NM == gu_input(), 평일_휴일 == holiday_input(), 시간대구분 == times_input())
    })
    
    # Leaflet 지도 렌더링
    output$plot <- renderLeaflet({
      req(filtered_data())
      
      # 팔레트 설정 (인구 수/인구밀도에 따른 색상 변화)
      pal <- colorNumeric(palette = "Reds", domain = filtered_data()[[ppl_input()]])
      
      # 범례 설정 (인구 수/인구밀도에 따른 색상 변화)
      legend_title <- names(ppl[ppl == ppl_input()])
      
      # 경도 위도 설정
      longitude <- gu_coord |> filter(자치구 == gu_input()) |> select(경도) |> as.numeric()
      latitude <- gu_coord |> filter(자치구 == gu_input()) |> select(위도) |> as.numeric()
      
      # 단위 설정
      unit <- ifelse(ppl_input() == 'avg_people', "명", "명/m²")  # 실제 데이터 단위에 맞게 수정
      round_num <- ifelse(ppl_input() == 'avg_people', 0, 5)  # 실제 데이터 단위에 맞게 수정
      
      leaflet(filtered_data()) %>%
        addTiles() %>%
        #  setView(lng = 126.9779451, lat = 37.5662952, zoom = 14) %>%
        setView(lng = longitude, lat = latitude, zoom = 13) %>%
        addPolygons(
          fillColor = ~pal(get(ppl_input())),
          color = "chocolate",               # 폴리곤의 테두리 색
          weight = 1,                        # 경계선 테두리 두께
          opacity = 0.9,                     # 경계선의 투명도 조절
          fillOpacity = 0.6,                 # 폴리곤 채우기 투명도 조절
          popup = ~paste0(시간대구분, "시간대<br>", legend_title, ": ", round(get(ppl_input()), round_num), unit)
        ) %>% 
        addLegend(
          pal = pal, 
          values = ~get(ppl_input()), 
          opacity = 0.5,
          title = legend_title,
          position = "bottomright"
        )
    })
    
    # 대피소 체크박스 업데이트 관찰
    observe({
      if (input$show_shelters) {
        updateShelters(TRUE, input$connect, gu_input(), radius_input(), ns("plot"), shelter_type, icons)
      } else {
        updateShelters(FALSE, FALSE, '중구', 0, ns("plot"), shelter_type, icons)
        resetShelterInputs()
      }
    })

    # 반경 selectInput 변경 시
    observeEvent(radius_input(), {
      if (input$show_shelters) {
        updateShelters(TRUE, input$connect, gu_input(), radius_input(), ns("plot"), shelter_type, icons)
      }
    })
    # 
    # 중심점 대피소 연결 체크박스 관찰
    observeEvent(input$connect, {
      updateShelters(input$show_shelters, input$connect, gu_input(), radius_input(), ns("plot"), shelter_type, icons)
    })
    
    # 'ppl', 'holiday', 'times' 변경 시 대피소 설정 초기화**
    observeEvent({
      gu_input()
      ppl_input()
      holiday_input()
      times_input()
    }, {
      if (input$show_shelters) {
        updateCheckboxInput(session, "show_shelters", value = FALSE)
        resetShelterInputs()
      }
    })
    
  })
}


# 지도 server 모듈 함수
tableServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # 테이블 데이터 반응형 변수 정의
    gu_nm <- reactive(input$gu_nm)
    holiday <- reactive(input$holiday)
    times <- reactive(input$times)
    
    # 데이터 테이블 렌더링 
    output$table <- renderDataTable({
      shelter_table <- data |> 
        filter(
          지역구 == gu_nm(),
          평일_휴일 == holiday(),
          시간대 == times()
        ) |> 
        select(-좌표)
      
      datatable(shelter_table, options = list(pageLength = 10))
    })
  })
}



