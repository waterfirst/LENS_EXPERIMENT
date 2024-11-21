library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("곡률 계수에 따른 2D 반구 단면 시뮬레이션"),

  sidebarLayout(
    sidebarPanel(
      # a 값을 선택하는 슬라이더
      sliderInput("a_value",
                  "곡률 계수 (a):",
                  min = 2,
                  max = 4,
                  value = 2,
                  step = 0.1),
      # 데이터 다운로드 버튼
      downloadButton("downloadData", "CSV로 다운로드")
    ),
    mainPanel(
      plotlyOutput("crossSectionDisplay", height = "400px")
    )
  )
)

server <- function(input, output) {
  # 반응형 데이터 생성
  data_reactive <- reactive({
    x <- seq(-10, 10, length.out = 500)  # X 값 범위
    max_value <- 10^input$a_value       # Z 최대값 (10^a)

    # Z 값 계산
    z <- ifelse(abs(x)^input$a_value <= max_value, 
                (max_value - abs(x)^input$a_value)^(1 / input$a_value), 
                NA)

    # 데이터프레임 생성
    data.frame(x = x, z = z)
  })

  # 단면 플롯 생성
  output$crossSectionDisplay <- renderPlotly({
    df <- data_reactive()

    # Plotly 그래프 생성
    plot_ly(df, x = ~x, y = ~z, type = 'scatter', mode = 'lines',
            line = list(color = 'blue', width = 2)) %>%
      layout(
        title = paste("X-Z 평면: X^", input$a_value, " + Z^", input$a_value, " = 10^", input$a_value),
        xaxis = list(title = "X", range = c(-10, 10), zeroline = TRUE),
        yaxis = list(title = "Z", range = c(0, max(df$z, na.rm = TRUE)), zeroline = TRUE),
        showlegend = FALSE
      )
  })

  # 데이터 다운로드 핸들러
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("cross_section_a", input$a_value, ".csv")
    },
    content = function(file) {
      write.csv(data_reactive(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
