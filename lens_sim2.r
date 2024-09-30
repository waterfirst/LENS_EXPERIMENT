library(shiny)

# 에어리 디스크 함수 (안전한 버전)
safe_airy_disk <- function(rho) {
  result <- rep(0, length(rho))
  valid <- rho != 0 & is.finite(rho)
  result[valid] <- (2 * besselJ(rho[valid], 1) / rho[valid])^2
  result[rho == 0] <- 1  # rho가 0일 때의 극한값
  result
}

# 렌즈 투과율 함수
lens_transmission <- function(r, radius) {
  transmission <- (1 - (r/radius)^2)^2
  transmission[r > radius] <- 0
  transmission
}

ui <- fluidPage(
  titlePanel("2x2 마이크로렌즈 어레이 회절 패턴 시뮬레이션"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("wavelength", "빛의 파장 (nm):", 
                  min = 400, max = 700, value = 550, step = 10),
      sliderInput("lens_distance", "렌즈-CCD 거리 (μm):", 
                  min = 100, max = 10000, value = 5000, step = 100),
      sliderInput("lens_diameter", "렌즈 지름 (μm):", 
                  min = 10, max = 100, value = 50, step = 5),
      sliderInput("lens_spacing", "렌즈 간격 (μm):", 
                  min = 10, max = 200, value = 60, step = 5),
      sliderInput("lens_height", "렌즈 높이 (μm):", 
                  min = 1, max = 20, value = 10, step = 1),
      sliderInput("lens_refractive_index", "렌즈 굴절률:", 
                  min = 1.4, max = 2.0, value = 1.5, step = 0.1)
    ),
    
    mainPanel(
      plotOutput("diffractionPlot", width = 400, height=400)
    )
  )
)

server <- function(input, output) {
  
  output$diffractionPlot <- renderPlot({
    # 파라미터 설정
    wavelength <- input$wavelength * 1e-9  # nm to m
    k <- 2 * pi / wavelength
    lens_distance <- input$lens_distance * 1e-6  # μm to m
    lens_diameter <- input$lens_diameter * 1e-6  # μm to m
    lens_radius <- lens_diameter / 2
    lens_spacing <- input$lens_spacing * 1e-6  # μm to m
    lens_height <- input$lens_height * 1e-6  # μm to m
    lens_n <- input$lens_refractive_index
    
    # 렌즈 초점 거리 계산
    R <- lens_diameter^2 / (8 * lens_height) + lens_height / 2  # 렌즈의 곡률 반경
    f <- R / (lens_n - 1)  # 렌즈의 초점 거리
    
    # 격자 생성
    x <- y <- seq(-1.5*lens_spacing, 1.5*lens_spacing, length.out = 500)
    grid <- expand.grid(x = x, y = y)
    
    # 2x2 렌즈 어레이 생성
    lens_centers <- expand.grid(x = c(-lens_spacing/2, lens_spacing/2),
                                y = c(-lens_spacing/2, lens_spacing/2))
    
    # 각 렌즈에 대한 회절 패턴 계산 및 간섭 고려
    amplitude <- matrix(0, nrow = 500, ncol = 500)
    for (i in 1:4) {  # 4개의 렌즈 모두 계산
      lens_x <- grid$x - lens_centers$x[i]
      lens_y <- grid$y - lens_centers$y[i]
      r <- sqrt(lens_x^2 + lens_y^2)
      
      # 렌즈 투과율 계산
      transmission <- lens_transmission(r, lens_radius)
      
      # 위상 계산
      phase <- -k * (lens_x^2 + lens_y^2) / (2 * f)
      
      # 진폭 계산 (투과율과 위상 고려)
      amplitude <- amplitude + sqrt(transmission) * exp(1i * phase)
    }
    
    # 강도 계산
    intensity <- abs(amplitude)^2
    
    # 결과 플로팅
    image(x*1e6, y*1e6, intensity, 
          col = colorRampPalette(c("black", "blue", "yellow", "red"))(100),
          xlab = "X (μm)", ylab = "Y (μm)", 
          main = "2x2 마이크로렌즈 어레이 회절 패턴")
    
    # 렌즈 위치 표시
    for (i in 1:nrow(lens_centers)) {
      symbols(lens_centers$x[i]*1e6, lens_centers$y[i]*1e6, 
              circles = lens_radius*1e6, inches = FALSE, add = TRUE)
    }
  })
}

shinyApp(ui = ui, server = server)
