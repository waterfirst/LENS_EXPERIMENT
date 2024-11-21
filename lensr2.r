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
                  step = 0.1)
    ),
    mainPanel(
      plotlyOutput("crossSectionDisplay", height = "400px")
    )
  )
)

server <- function(input, output) {
  # 단면 플롯 생성
  output$