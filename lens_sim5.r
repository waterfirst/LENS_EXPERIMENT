library(shiny)

# 에어리 디스크 함수 (안전한 버전)
safe_airy_disk <- function(rho) {
  result <- rep(0, length(rho))
  valid <- rho != 0 & is.finite(rho)
  result[valid] <- (2 * besselJ(rho[valid], 1) / rho[valid])^2
  result[rho == 0] <- 1  # rho가 0일 때의 극한값
  result
}

# 파장을 RGB로 변환하는 함수
wavelength_to_rgb <- function(wavelength) {
  gamma <- 0.8
  intensity_max <- 255
  factor <- 0
  rgb <- rep(0, 3)
  
  if (wavelength >= 380 && wavelength < 440) {
    rgb[1] <- -(wavelength - 440) / (440 - 380)
    rgb[3] <- 1
  } else if (wavelength >= 440 && wavelength < 490) {
    rgb[2] <- (wavelength - 440) / (490 - 440)
    rgb[3] <- 1
  } else if (wavelength >= 490 && wavelength < 510) {
    rgb[2] <- 1
    rgb[3] <- -(wavelength - 510) / (510 - 490)
  } else if (wavelength >= 510 && wavelength < 580) {
    rgb[1] <- (wavelength - 510) / (580 - 510)
    rgb[2] <- 1
  } else if (wavelength >= 580 && wavelength < 645) {
    rgb[1] <- 1
    rgb[2] <- -(wavelength - 645) / (645 - 580)
  } else if (wavelength >= 645 && wavelength <= 780) {
    rgb[1] <- 1
  }
  
  if (wavelength >= 380 && wavelength < 420) {
    factor <- 0.3 + 0.7 * (wavelength - 380) / (420 - 380)
  } else if (wavelength >= 420 && wavelength <= 700) {
    factor <- 1
  } else if (wavelength > 700 && wavelength <= 780) {
    factor <- 0.3 + 0.7 * (780 - wavelength) / (780 - 700)
  }
  
  rgb <- (rgb * factor)^gamma * intensity_max
  rgb[rgb > 255] <- 255
  rgb[rgb < 0] <- 0
  
  return(rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255))
}

# 노출량에 따른 이미지 조정 함수
adjust_exposure <- function(intensity, exposure) {
  adjusted <- intensity * exposure
  adjusted[adjusted > 1] <- 1  # 1을 초과하는 값은 1로 제한
  adjusted
}

ui <- fluidPage(
  titlePanel("에어리 원반(Airy Disk) 시뮬레이션"),
  
  fluidRow(
    column(4,
           wellPanel(
             sliderInput("wavelength", "빛의 파장 (nm):", 
                         min = 380, max = 780, value = 550, step = 10),
             sliderInput("aperture_diameter", "조리개 직경 (mm):", 
                         min = 1, max = 100, value = 10, step = 1),
             sliderInput("detector_distance", "조리개-검출기 거리 (mm):", 
                         min = 10, max = 1000, value = 500, step = 10),
             sliderInput("exposure", "노출량:", 
                         min = 0.1, max = 10, value = 10, step = 0.1),
             sliderInput("x_scale", "X축 범위 (μm):", 
                         min = 1, max = 100, value = 50, step = 1),
             sliderInput("refractive_index", "매질의 굴절률:", 
                         min = 1, max = 2, value = 1.6, step = 0.1),
             sliderInput("transmittance", "매질의 투과율 (%):", 
                         min = 0, max = 100, value = 80, step = 1),
             downloadButton("downloadPNG", "PNG 다운로드"),
             downloadButton("downloadCSV", "CSV 다운로드")
           )
    ),
    column(8,
           # SVG 이미지 삽입
           HTML('
        <svg xmlns="http://www.w3.org/2000/svg" width="100%" height="200" viewBox="0 0 800 400">
          <rect width="100%" height="100%" fill="#f0f0f0"/>
          <rect x="50" y="180" width="100" height="40" fill="#ff6666" />
          <text x="100" y="170" text-anchor="middle" font-size="14">레이저 광원</text>
          <line x1="150" y1="200" x2="750" y2="200" stroke="#ff0000" stroke-width="2" stroke-dasharray="5,5" />
          <rect x="350" y="150" width="10" height="100" fill="#666666" />
          <circle cx="355" cy="200" r="20" fill="#ffffff" stroke="#000000" stroke-width="2" />
          <text x="355" y="250" text-anchor="middle" font-size="14">조리개</text>
          <rect x="650" y="150" width="50" height="100" fill="#9999ff" />
          <text x="675" y="270" text-anchor="middle" font-size="14">검출기</text>
          <text x="250" y="150" text-anchor="middle" font-size="16">λ (파장)</text>
          <text x="355" y="130" text-anchor="middle" font-size="16">D (조리개 직경)</text>
          <text x="500" y="150" text-anchor="middle" font-size="16">z (조리개-검출기 거리)</text>
          <line x1="355" y1="135" x2="355" y2="185" stroke="#000000" stroke-width="1" marker-end="url(#arrowhead)" />
          <line x1="360" y1="220" x2="650" y2="220" stroke="#000000" stroke-width="1" marker-end="url(#arrowhead)" />
          <line x1="360" y1="225" x2="360" y2="215" stroke="#000000" stroke-width="1" />
          <line x1="650" y1="225" x2="650" y2="215" stroke="#000000" stroke-width="1" />
          <defs>
            <marker id="arrowhead" markerWidth="10" markerHeight="7" refX="0" refY="3.5" orient="auto">
              <polygon points="0 0, 10 3.5, 0 7" />
            </marker>
          </defs>
        </svg>
      '),
           plotOutput("airyPlot", width = 400, height=400),
           plotOutput("intensityPlot", width = 400, height=400)
    )
  )
)

server <- function(input, output) {
  
  generatePlots <- reactive({
    wavelength <- input$wavelength * 1e-9 / input$refractive_index  # 매질 내 파장
    aperture_diameter <- input$aperture_diameter * 1e-3  # mm to m
    detector_distance <- input$detector_distance * 1e-3  # mm to m
    k <- 2 * pi / wavelength
    
    # 회절 패턴 생성
    size <- 400  # 해상도 증가
    x <- y <- seq(-input$x_scale * 1e-6, input$x_scale*1e-6, length.out = size)  # 범위 확장
    grid <- expand.grid(x = x, y = y)
    r <- with(grid, sqrt(x^2 + y^2))
    rho <- k * aperture_diameter * r / (2 * detector_distance)
    intensity <- safe_airy_disk(rho) * (input$transmittance / 100)  # 투과율 적용
    intensity_matrix <- matrix(intensity, nrow = size, ncol = size)
    
    # 노출량 적용
    adjusted_intensity <- adjust_exposure(intensity_matrix, input$exposure)
    
    # 파장에 따른 색상 설정
    color <- wavelength_to_rgb(input$wavelength)
    
    # 강도 프로필 계산
    r_1d <- seq(-input$x_scale * 1e-6, input$x_scale*1e-6, length.out = 1000)
    rho_1d <- k * aperture_diameter * abs(r_1d) / (2 * detector_distance)
    intensity_1d <- safe_airy_disk(rho_1d) * (input$transmittance / 100)  # 투과율 적용
    
    # 0th peak 반지름 계산 (굴절률 고려)
    zero_peak_radius <- 1.22 * wavelength * detector_distance / aperture_diameter
    
    list(x = x, y = y, intensity_matrix = adjusted_intensity, color = color,
         r_1d = r_1d, intensity_1d = intensity_1d, zero_peak_radius = zero_peak_radius)
  })
  
  output$airyPlot <- renderPlot({
    data <- generatePlots()
    image(data$x*1e6, data$y*1e6, data$intensity_matrix, 
          col = colorRampPalette(c("black", data$color))(100),
          xlab = "X (μm)", ylab = "Y (μm)", main = "에어리 원반 회절 패턴")
  })
  
  output$intensityPlot <- renderPlot({
    data <- generatePlots()
    plot(data$r_1d*1e6, data$intensity_1d, type = "l", col = data$color,
         xlab = "반경 (μm)", ylab = "상대 강도", main = "에어리 원반 강도 프로필",
         xlim = c(-input$x_scale, input$x_scale))
    abline(v = 0, lty = 2, col = "gray")
    abline(v = c(-data$zero_peak_radius, data$zero_peak_radius)*1e6, lty = 2, col = "red")
    text(data$zero_peak_radius*1e6, max(data$intensity_1d)*0.1, 
         paste("0th peak 반지름:", round(data$zero_peak_radius*1e6, 2), "μm"), 
         pos = 4, col = "red")
  })
  
  output$downloadPNG <- downloadHandler(
    filename = function() {
      paste("airy_disk_plot_", input$wavelength, "nm_n", input$refractive_index, "_t", input$transmittance, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 1200)  # 높이를 증가시켜 모든 그래프를 포함
      par(mfrow = c(3, 1))  # 3개의 그래프를 세로로 배치
      
      # SVG 이미지는 PNG에 포함할 수 없으므로 생략
      
      # 회절 패턴 플롯
      data <- generatePlots()
      image(data$x*1e6, data$y*1e6, data$intensity_matrix, 
            col = colorRampPalette(c("black", data$color))(100),
            xlab = "X (μm)", ylab = "Y (μm)", main = "에어리 원반 회절 패턴")
      
      # 강도 프로필 플롯
      plot(data$r_1d*1e6, data$intensity_1d, type = "l", col = data$color,
           xlab = "반경 (μm)", ylab = "상대 강도", main = "에어리 원반 강도 프로필",
           xlim = c(-input$x_scale, input$x_scale))
      abline(v = 0, lty = 2, col = "gray")
      abline(v = c(-data$zero_peak_radius, data$zero_peak_radius)*1e6, lty = 2, col = "red")
      text(data$zero_peak_radius*1e6, max(data$intensity_1d)*0.1, 
           paste("0th peak 반지름:", round(data$zero_peak_radius*1e6, 2), "μm"), 
           pos = 4, col = "red")
      
      dev.off()
    }
  )
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("airy_disk_data_", input$wavelength, "nm_n", input$refractive_index, "_t", input$transmittance, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- generatePlots()
      df <- data.frame(
        Radius_um = data$r_1d*1e6,
        Intensity = data$intensity_1d
      )
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
