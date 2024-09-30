## 2 by 2 lens

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

# 초점 거리 계산 함수
calculate_focal_length <- function(diameter, height, n) {
  R <- diameter^2 / (8 * height) + height / 2  # 렌즈의 곡률 반경
  f <- R / (n - 1)  # 렌즈의 초점 거리
  return(f)
}

ui <- fluidPage(
  titlePanel("2x2 마이크로렌즈 어레이 회절 패턴 시뮬레이션"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("wavelength", "빛의 파장 (nm):", 
                  min = 380, max = 780, value = 550, step = 10),
      sliderInput("lens_distance", "렌즈-CCD 거리 (μm):", 
                  min = 50, max = 10000, value = 500, step = 50),
      sliderInput("lens_diameter", "렌즈 지름 (μm):", 
                  min = 10, max = 100, value = 50, step = 5),
      sliderInput("lens_spacing", "렌즈 간격 (μm):", 
                  min = 10, max = 200, value = 60, step = 5),
      sliderInput("lens_height", "렌즈 높이 (μm):", 
                  min = 1, max = 20, value = 10, step = 1),
      sliderInput("lens_refractive_index", "렌즈 굴절률:", 
                  min = 1.4, max = 2.0, value = 1.5, step = 0.05),
      sliderInput("exposure", "노출량:", 
                  min = 0.1, max = 10, value = 1, step = 0.1),
      downloadButton("downloadPNG", "PNG 다운로드"),
      downloadButton("downloadCSV", "CSV 다운로드")
    ),
    
    mainPanel(
      plotOutput("diffractionPlot",  width = 400, height=400),
      plotOutput("transmissionProfile", width = 500, height=400)
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
    
    # 초점 거리 계산
    f <- calculate_focal_length(lens_diameter, lens_height, lens_n)
    
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
      
      # 위상 계산 (초점 거리와 CCD 거리 고려)
      phase <- -k * (lens_x^2 + lens_y^2) * (1/f - 1/lens_distance) / 2
      
      # 진폭 계산 (투과율과 위상 고려)
      amplitude <- amplitude + sqrt(transmission) * exp(1i * phase)
    }
    
    # 강도 계산 및 노출량 조정
    intensity <- abs(amplitude)^2
    intensity <- adjust_exposure(intensity, input$exposure)
    
    # 결과 플로팅
    image(x*1e6, y*1e6, intensity, 
          col = colorRampPalette(c("black", wavelength_to_rgb(input$wavelength)))(100),
          xlab = "X (μm)", ylab = "Y (μm)", 
          main = "2x2 마이크로렌즈 어레이 회절 패턴")
    
    # 렌즈 위치 표시
    for (i in 1:nrow(lens_centers)) {
      symbols(lens_centers$x[i]*1e6, lens_centers$y[i]*1e6, 
              circles = lens_radius*1e6, inches = FALSE, add = TRUE)
    }
  })
  
  output$transmissionProfile <- renderPlot({
    lens_diameter <- input$lens_diameter
    lens_spacing <- input$lens_spacing
    lens_height <- input$lens_height
    lens_n <- input$lens_refractive_index
    wavelength <- input$wavelength * 1e-9  # nm to m
    k <- 2 * pi / wavelength
    lens_distance <- input$lens_distance * 1e-6  # μm to m
    
    f <- calculate_focal_length(lens_diameter*1e-6, lens_height*1e-6, lens_n)
    
    x <- seq(-60, 60, length.out = 1000)
    
    transmission1 <- lens_transmission(abs(x - lens_spacing/2), lens_diameter/2)
    transmission2 <- lens_transmission(abs(x + lens_spacing/2), lens_diameter/2)
    
    phase1 <- -k * ((x - lens_spacing/2)^2) * (1/f - 1/lens_distance) / 2
    phase2 <- -k * ((x + lens_spacing/2)^2) * (1/f - 1/lens_distance) / 2
    
    amplitude1 <- sqrt(transmission1) * exp(1i * phase1)
    amplitude2 <- sqrt(transmission2) * exp(1i * phase2)
    
    combined_amplitude <- amplitude1 + amplitude2
    combined_intensity <- abs(combined_amplitude)^2
    
    plot(x, combined_intensity, type = "l", col = "blue",
         xlab = "X (μm)", ylab = "상대 강도",
         main = "렌즈 투과 강도 프로필 (y=20μm)",
         ylim = c(0, max(combined_intensity)*1.1))
    lines(x, abs(amplitude1)^2, col = "red", lty = 2)
    lines(x, abs(amplitude2)^2, col = "green", lty = 2)
    legend("topright", legend = c("합성", "렌즈 1", "렌즈 2"),
           col = c("blue", "red", "green"), lty = c(1, 2, 2))
  })
  
  # PNG 다운로드 핸들러
  output$downloadPNG <- downloadHandler(
    filename = function() {
      paste("microlens_array_", input$wavelength, "nm_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 1200)
      par(mfrow = c(2, 1))
      
      # 회절 패턴 플롯
      wavelength <- input$wavelength * 1e-9  # nm to m
      k <- 2 * pi / wavelength
      lens_distance <- input$lens_distance * 1e-6  # μm to m
      lens_diameter <- input$lens_diameter * 1e-6  # μm to m
      lens_radius <- lens_diameter / 2
      lens_spacing <- input$lens_spacing * 1e-6  # μm to m
      lens_height <- input$lens_height * 1e-6  # μm to m
      lens_n <- input$lens_refractive_index
      
      f <- calculate_focal_length(lens_diameter, lens_height, lens_n)
      
      x <- y <- seq(-1.5*lens_spacing, 1.5*lens_spacing, length.out = 500)
      grid <- expand.grid(x = x, y = y)
      
      lens_centers <- expand.grid(x = c(-lens_spacing/2, lens_spacing/2),
                                  y = c(-lens_spacing/2, lens_spacing/2))
      
      amplitude <- matrix(0, nrow = 500, ncol = 500)
      for (i in 1:4) {
        lens_x <- grid$x - lens_centers$x[i]
        lens_y <- grid$y - lens_centers$y[i]
        r <- sqrt(lens_x^2 + lens_y^2)
        
        transmission <- lens_transmission(r, lens_radius)
        phase <- -k * (lens_x^2 + lens_y^2) * (1/f - 1/lens_distance) / 2
        amplitude <- amplitude + sqrt(transmission) * exp(1i * phase)
      }
      
      intensity <- abs(amplitude)^2
      intensity <- adjust_exposure(intensity, input$exposure)
      
      image(x*1e6, y*1e6, intensity, 
            col = colorRampPalette(c("black", wavelength_to_rgb(input$wavelength)))(100),
            xlab = "X (μm)", ylab = "Y (μm)", 
            main = "2x2 마이크로렌즈 어레이 회절 패턴")
      
      for (i in 1:nrow(lens_centers)) {
        symbols(lens_centers$x[i]*1e6, lens_centers$y[i]*1e6, 
                circles = lens_radius*1e6, inches = FALSE, add = TRUE)
      }
      
      # 투과율 프로필 플롯
      x <- seq(-60, 60, length.out = 1000)
      
      transmission1 <- lens_transmission(abs(x - lens_spacing*1e6/2), lens_diameter*1e6/2)
      transmission2 <- lens_transmission(abs(x + lens_spacing*1e6/2), lens_diameter*1e6/2)
      
      phase1 <- -k * ((x*1e-6 - lens_spacing/2)^2) * (1/f - 1/lens_distance) / 2
      phase2 <- -k * ((x*1e-6 + lens_spacing/2)^2) * (1/f - 1/lens_distance) / 2
      
      amplitude1 <- sqrt(transmission1) * exp(1i * phase1)
      amplitude2 <- sqrt(transmission2) * exp(1i * phase2)
      
      combined_amplitude <- amplitude1 + amplitude2
      combined_intensity <- abs(combined_amplitude)^2
      
      plot(x, combined_intensity, type = "l", col = "blue",
           xlab = "X (μm)", ylab = "상대 강도",
           main = "렌즈 투과 강도 프로필 (y=20μm)",
           ylim = c(0, max(combined_intensity)*1.1))
      lines(x, abs(amplitude1)^2, col = "red", lty = 2)
      lines(x, abs(amplitude2)^2, col = "green", lty = 2)
      legend("topright", legend = c("합성", "렌즈 1", "렌즈 2"),
             col = c("blue", "red", "green"), lty = c(1, 2, 2))
      
      dev.off()
    }
  )
  
  # CSV 다운로드 핸들러
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("microlens_array_data_", input$wavelength, "nm_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # 렌즈 매개변수 계산
      lens_diameter <- input$lens_diameter
      lens_spacing <- input$lens_spacing
      lens_height <- input$lens_height
      lens_n <- input$lens_refractive_index
      wavelength <- input$wavelength * 1e-9  # nm to m
      k <- 2 * pi / wavelength
      lens_distance <- input$lens_distance * 1e-6  # μm to m
      
      f <- calculate_focal_length(lens_diameter*1e-6, lens_height*1e-6, lens_n)
      
      x <- seq(-60, 60, length.out = 1000)
      
      transmission1 <- lens_transmission(abs(x - lens_spacing/2), lens_diameter/2)
      transmission2 <- lens_transmission(abs(x + lens_spacing/2), lens_diameter/2)
      
      phase1 <- -k * ((x*1e-6 - lens_spacing*1e-6/2)^2) * (1/f - 1/lens_distance) / 2
      phase2 <- -k * ((x*1e-6 + lens_spacing*1e-6/2)^2) * (1/f - 1/lens_distance) / 2
      
      amplitude1 <- sqrt(transmission1) * exp(1i * phase1)
      amplitude2 <- sqrt(transmission2) * exp(1i * phase2)
      
      combined_amplitude <- amplitude1 + amplitude2
      combined_intensity <- abs(combined_amplitude)^2
      
      df <- data.frame(
        X_um = x,
        Combined_Intensity = combined_intensity,
        Lens1_Intensity = abs(amplitude1)^2,
        Lens2_Intensity = abs(amplitude2)^2
      )
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
