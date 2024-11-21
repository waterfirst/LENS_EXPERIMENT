# app.R
library(shiny)
library(plotly)
library(tidyverse)

ui <- fluidPage(
  titlePanel("곡률계수에 따른 반구 Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      # a 값을 선택하는 슬라이더
      sliderInput("a_value",
                  "a 값:",
                  min = 2,
                  max = 4,
                  value = 2,step = 0.1),
      
      # # Z 값을 선택하는 슬라이더
      # sliderInput("z_value",
      #             "Z 값:",
      #             min = 6,
      #             max = 9,
      #             value = 6,
      #             step = 0.1)
    ),
    
    mainPanel(
      #plotlyOutput("hemisphereDisplay", height = "400px"),
      plotlyOutput("crossSectionDisplay", height = "300px")
    )
  )
)

server <- function(input, output) {
  
  # 3D 반구 플롯
  output$hemisphereDisplay <- renderPlotly({
    points <- 50  # 격자점 수 고정
    x <- seq(-10, 10, length.out = points)
    y <- seq(-10, 10, length.out = points)  # y축 범위를 대칭으로 변경
    
    # z 값을 저장할 매트릭스 생성
    z <- matrix(NA, nrow = length(y), ncol = length(x))
    
    # 각 격자점에 대해 z 값 계산
    for (i in 1:length(y)) {
      for (j in 1:length(x)) {
        if ((abs(x[j])^input$a_value + abs(y[i])^input$a_value) <= input$z_value) {
          z[i,j] <- sqrt(input$z_value - abs(x[j])^input$a_value - abs(y[i])^input$a_value)
        }
      }
    }
    
    plot_ly() %>%
      add_surface(
        x = x,
        y = y,
        z = z,
        colorscale = "Viridis"
      ) %>%
      layout(
        scene = list(
          xaxis = list(title = "X", range = c(-10, 10)),
          yaxis = list(title = "Y", range = c(-10, 10)),
          zaxis = list(title = "Z"),
          camera = list(
            eye = list(x = 1.5, y = 1.5, z = 1.5)
          )
        ),
        title = paste("반구 형태 (a =", input$a_value, ", Z =", input$z_value, ")")
      )
  })
  
  # 단면 플롯 (X^a + Z^a = 100)
  output$crossSectionDisplay <- renderPlotly({
    x <- seq(-10, 10, length.out = 500)  # 더 부드러운 곡선을 위해 포인트 수 증가
    
    # Z = (10^a - X^a)^(1/a) 계산
    z <- sapply(x, function(x_val) {
      if (abs(x_val)^input$a_value <= 10000) {
        return((10^input$a_value - abs(x_val)^input$a_value)^(1/input$a_value))
      } else {
        return(NA)
      }
    })
    # 데이터프레임 생성
    df <- data.frame(x = x, z = z)
    
    plot_ly(df, x = ~x, y = ~z, type = 'scatter', mode = 'lines',
            line = list(color = 'blue', width = 2)) %>%
      layout(
        title = paste("X-Z 평면: X^", input$a_value, " + Z^", input$a_value, " = 10^",input$a_value, "\n" ),
        xaxis = list(title = "X",
                     range = c(-10, 10),  # x축 범위 고정
                     zeroline = TRUE),
        yaxis = list(title = "Z", 
                     range = c(0, 10),    # z축 범위 설정
                     zeroline = TRUE),
        showlegend = FALSE
      ) %>%
      add_trace(x = c(-10, 10), y = c(0, 0), 
                mode = 'lines', line = list(color = 'gray', dash = 'dash'),
                showlegend = FALSE)  # x축 선 추가
  })
}

shinyApp(ui = ui, server = server)
