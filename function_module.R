# 필요한 패키지 목록 정의
required_packages <- c("tidyverse", "ggplot2", "plotly", "sf", "sp", "leaflet", "leaflet.extras", "shiny", "shinythemes", "DT", "bslib", "plotly")

# 패키지 설치 및 로드 함수
install_and_load <- function(packages) {
  for (pkg in packages) {
    # 패키지가 설치되어 있는지 확인
    if (!require(pkg, character.only = TRUE)) {
      # 패키지가 설치되어 있지 않으면 설치
      install.packages(pkg, dependencies = TRUE)
      # 설치 후 패키지 로드
      library(pkg, character.only = TRUE)
    }
  }
}

# 필요한 패키지 설치 및 로드
install_and_load(required_packages)

#### 1. 데이터 불러오기
load("munging_data.RData")

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
gu_nm <- unique(gnd_shelters_4326$gu_nm)

#### 3. 히스토그램, 박스플롯 
## 여백 설정을 위한 변수 설정
margins_R <- list(t = 50, b = 25, l = 25, r = 25)

# 최단거리 시각화 함수
min_dist_plot <- function(data){
  ggplot(data, aes(x = min_dist)) +
    geom_histogram(aes(y = ..density..), fill = "skyblue", color = "gray", alpha = 0.8) +
    geom_density(color = "red", size = 0.8) +
    labs(
      title = "중심점 최단거리 분포 및 밀도",
      x = "최단거리 (미터)",
      y = "밀도"
    ) +
    theme_minimal()  +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text.x = element_text(size = 14),  # x축 레이블 크기 변경
      axis.text.y = element_text(size = 14),  # y축 레이블 크기 변경
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      plot.margin = margin(t =margins_R$t/5 , r = margins_R$r/5, b = margins_R$b/5 , l = margins_R$l/5)
    )
}

# 포화율 시각화 함수
strtn_plot <- function(data){
  plot_ly(data) |> add_boxplot(x = ~시간대, y = ~`대피소 포화율(%)`, color = ~`평일_휴일`, colors = "Set2") |> 
    layout(boxmode = "group", title = list(text = "<b>대피소 포화율(%) 분포</b>", x = 0.5), yaxis = list(title = "대피소 포화율(%)"), margin = margins_R)
}


#### 4. 대피소 아이콘 색상 설정
# 포화율에 따라 색상을 지정하는 함수 (벡터화된 조건문)
get_color <- function(saturation) {
  ifelse(is.na(saturation), "gray",        # NA 값을 "gray"로 지정
         ifelse(saturation > 1000, "red",
                ifelse(saturation > 500, "orange",
                       ifelse(saturation > 100, "beige", "lightblue"))))
}

# addControl을 사용하여 대피소 포화율을 별도의 커스텀 범례 HTML 추가
shelter_legend_html <- '
<div id="shelterLegend" style="background: white; padding: 10px; border-radius: 5px;">
  <strong>대피소 포화율 (%)</strong><br>
  <i style="background: lightblue; width: 18px; height: 18px; display: inline-block;"></i> 0~100(양호)<br>
  <i style="background: beige; width: 18px; height: 18px; display: inline-block;"></i> 101~500(주의)<br>
  <i style="background: orange; width: 18px; height: 18px; display: inline-block;"></i> 501~1000(경고)<br>
  <i style="background: red; width: 18px; height: 18px; display: inline-block;"></i> 1000초과(긴급) <br>
  <i style="background: gray; width: 18px; height: 18px; display: inline-block;"></i> NA
</div>
'
#### 5.대피소 및 반경 업데이트 함수
updateShelters <- function(show, connect, gu, hday, tm,radius, plot_id, shelter_type, icons) {
  # shelter_type: "ground" 또는 "underground"
  
  # 그룹명 설정
  shelters_group <- paste0("shelters_", shelter_type)
  circles_group <- paste0("circles_", shelter_type)
  polylines_group <- paste0("polylines_", shelter_type)
  
  # 데이터셋 설정
  if (shelter_type == "ground") {
    shelters_data <- gnd_shelters_4326 |> filter(gu_nm == gu, (평일_휴일 == hday|is.na(평일_휴일)), (시간대 == tm|is.na(시간대)))
    lines_data <- gnd_lines_sf |> filter(gu_nm == gu)
  } else if (shelter_type == "underground") {
    shelters_data <- und_shelters_4326 |> filter(gu_nm == gu, (평일_휴일 == hday|is.na(평일_휴일)), (시간대 == tm|is.na(시간대)))
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
          icon = awesomeIcons(
            icon = icons,            # 아이콘 (여기서는 글리프 아이콘 사용)
            iconColor = 'white',            # 아이콘 색상
            library = 'fa',          # 글리프 아이콘 라이브러리
            markerColor = ~get_color(`대피소 포화율(%)`)  # 포화율에 따른 색상 설정
          ),
          label = ~fclty_nm,
          popup = ~paste(
            "수용인원: ", psbl_num, 
            "<br>포화율: ", `대피소 포화율(%)`, "%", 
            paste0("<br><b>과부족인원: ", round(psbl_num * (1 - `대피소 포화율(%)` * 0.01)*-1), "명", "</b>")
          ),
          clusterOptions = markerClusterOptions(maxClusterRadius = 30),
          group = shelters_group
        ) |> 
        addControl(html = shelter_legend_html, position = "topright", layerId = "shelterLegend")
      
      # 반경 그룹 먼저 제거 후 새로 추가
      proxy |> 
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
      proxy |>
        clearGroup(shelters_group) |>
        clearGroup(circles_group) |>
        clearGroup(polylines_group) |> 
        removeControl("shelterLegend")  # 범례도 제거
    }
    # 성공 시 사용자에게 알림
    # showNotification(paste0(shelter_type, " 대피소가 성공적으로 업데이트되었습니다."), type = "message")
  }, error = function(e) {
    showNotification(paste0(shelter_type, " 대피소 업데이트 중 오류가 발생했습니다: ", e$message), type = "error")
    message("Error in updateShelters: ", e$message)
  })
}

#### 6. 모듈 UI 
# 6-1 지도 UI 모듈 함수 
geoUI <- function(id){
  ns <- NS(id)
  tagList(
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
                tagList(
                selectInput(ns('radius'), '반경', choices = radius, selected = 0),
              # 대피소와 집계구 연결을 위한 체크박스
              checkboxInput(ns('connect'), "집계구 중심점과 대피소 연결", FALSE)
                )
              )
           )
  ),
  fluidRow(
    column(5, plotOutput(ns("plot2"))),
    column(5, plotlyOutput(ns("plot3"), inline = TRUE)),
    column(2, tags$div(
      h5(HTML("최단거리 1km 이상 &<br>인구밀집 지역(90분위수)"), style = "font-weight: bold;"),
      tableOutput(ns("location"))
    ))
  ),
  fluidRow(
    column(5, tags$div(
      h5("미도달 집계구 비율", style = "font-weight: bold; text-align: center;"),
      tableOutput(ns("dist_rate"))
    )),
    column(7, tags$div(
      h5("미도달 인구 비율", style = "font-weight: bold; text-align: center;"),
      tableOutput(ns("dist_ppl"))
    ))
  )
  )
}

# 6-2. 테이블 UI 모듈 함수
tableUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(4, selectInput(ns('gu_nm'), '지역구', choices = gu_nm, selected = '중구')),
    column(4, selectInput(ns('holiday'), '평일/공휴일', choices = holiday, selected = '평일')),
    column(4, selectInput(ns('times'), '시간대구분', choices = times, selected = '12')),
    column(12, dataTableOutput(ns('table')))
  )
}

#### 7. 모듈 server 
# 7-1. 지도 server 모듈 함수
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
      req(living_people_grid_4326)
      req(gu_input(), holiday_input(), times_input())
      
      living_people_grid_4326 |> 
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
      
      leaflet(filtered_data()) |> 
        addTiles() |>
        setView(lng = longitude, lat = latitude, zoom = 13) |>
        addPolygons(
          fillColor = ~pal(get(ppl_input())),
          color = "chocolate",               # 폴리곤의 테두리 색
          weight = 1,                        # 경계선 테두리 두께
          opacity = 0.9,                     # 경계선의 투명도 조절
          fillOpacity = 0.6,                 # 폴리곤 채우기 투명도 조절
          popup = ~paste0(ADM_NM, "<br>", legend_title, ": ", round(get(ppl_input()), round_num), unit)
        ) |> 
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
        updateShelters(TRUE, input$connect, gu_input(), holiday_input(), times_input(), radius_input(), ns("plot"), shelter_type, icons)
      } else {
        updateShelters(FALSE, FALSE, '중구', '평일', '12', 0, ns("plot"), shelter_type, icons)
        resetShelterInputs()
      }
    })
    
    # 반경 selectInput 변경 시
    observeEvent(radius_input(), {
      if (input$show_shelters) {
        updateShelters(TRUE, input$connect, gu_input(), holiday_input(), times_input(), radius_input(), ns("plot"), shelter_type, icons)
      }
    })
    # 
    # 중심점 대피소 연결 체크박스 관찰
    observeEvent(input$connect, {
      updateShelters(input$show_shelters, input$connect, gu_input(), holiday_input(), times_input(), radius_input(), ns("plot"), shelter_type, icons)
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
    
    
    # 히스토그램 및 밀도
    selected_data <- reactive({
      if(shelter_type == 'ground'){
        list(
          min_dist_data = grid_shelters |> filter(SIGUNGU_NM == gu_input()) |> st_drop_geometry() |> rename(min_dist = gnd_min_dist),
          strtn_rate_data = gnd_shelter_strtn_rate |> filter(지역구 == gu_input()),
          dist_rate_data = grid_shelters |> filter(SIGUNGU_NM == gu_input()) |> 
            st_drop_geometry() |> select(gnd_min_dist) |> rename(min_dist = gnd_min_dist),
          dist_ppl_data = living_people_grid_4326 |> filter(SIGUNGU_NM == gu_input()) |>  
            # 지오메트리 열 제거 (sf 객체의 경우)
            st_drop_geometry() |> select(평일_휴일, 시간대구분, ADM_NM, gnd_min_dist, avg_people) |> 
            rename(지역동 = ADM_NM, min_dist = gnd_min_dist)
        )
      } else{
        list(
          min_dist_data = grid_shelters |> filter(SIGUNGU_NM == gu_input()) |> st_drop_geometry() |> rename(min_dist = und_min_dist),
          strtn_rate_data = und_shelter_strtn_rate |> filter(지역구 == gu_input()),
          dist_rate_data = grid_shelters |> filter(SIGUNGU_NM == gu_input()) |> 
            st_drop_geometry() |> select(und_min_dist) |> rename(min_dist = und_min_dist),
          dist_ppl_data = living_people_grid_4326 |> filter(SIGUNGU_NM == gu_input()) |> 
            # 지오메트리 열 제거 (sf 객체의 경우)
            st_drop_geometry() |> select(평일_휴일, 시간대구분, ADM_NM, und_min_dist, avg_people) |> 
            rename(지역동 = ADM_NM, min_dist = und_min_dist)
        )
      }
    })
    
    # 히스토그램 및 밀도
    output$plot2 <- renderPlot({
      req(selected_data()$min_dist_data)  
      min_dist_plot(selected_data()$min_dist_data)
    })
    
    # boxplot
    output$plot3 <- renderPlotly({
      req(selected_data()$strtn_rate_data)  
      strtn_plot(selected_data()$strtn_rate_data)
    })
    
    # 거리별 미도달인구 비율
    output$dist_ppl <- renderTable({
      req(selected_data()$dist_ppl_data)  
      selected_data()$dist_ppl_data |> group_by(평일_휴일, 시간대구분) |> 
        summarise(
          `전체인구` = round(sum(avg_people, na.rm = TRUE), 0),                                   # 전체 인구수
          `미도달인구(500m이상)` = round(sum(avg_people[min_dist >= 500], na.rm = TRUE), 0),      # min_dist >= 500인 인구수
          `비율(500m이상)` = round(`미도달인구(500m이상)` / `전체인구` * 100, 2),                 # 비율 계산
          `미도달인구(1000m이상)` = round(sum(avg_people[min_dist >= 1000], na.rm = TRUE), 0),    # min_dist >= 1000인 인구수
          `비율(1000m이상)` = round(`미도달인구(1000m이상)` / `전체인구` * 100, 2), .groups = "drop")                # 비율 계산
    })
    
    
    # 지역구 취약지역 입지 
    output$location <- renderTable({
      req(selected_data()$dist_ppl_data) 
      avg_ppl9 <- selected_data()$dist_ppl_data |> 
        summarise(
          deciles = list(quantile(avg_people, probs = seq(0, 1, by = 0.1), na.rm = TRUE))
        ) |> 
        pull(deciles) |> unlist()
      selected_data()$dist_ppl_data |> filter(min_dist > 1000, avg_people > avg_ppl9[10]) |>
        group_by(지역동) |> count(name = "집계구 개수")
    })
    
    # 거리별 집계구 비율
    output$dist_rate <- renderTable({
      req(selected_data()$dist_rate_data)  
      selected_data()$dist_rate_data |> 
        summarise(`전체집계구` = n(),
                  `미도달집계구(500m이상)` = sum(min_dist >= 500, na.rm = TRUE),
                  `미도달비율(%)(500m이상)` = round(mean(min_dist >= 500, na.rm = TRUE)*100, 2),
                  `미도달집계구(1000m이상)` = sum(min_dist >= 1000, na.rm = TRUE),
                  `미도달비율(%)(1000m이상)` = round(mean(min_dist >= 1000, na.rm = TRUE)*100, 2))
      
    })
    
  })
}

# 7-2. 지도 server 모듈 함수
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
