# Load necessary libraries
library(fields)
library(ggplot2)
library(reshape2)

# Define simulation parameters
wavelength <- 650e-9  # wavelength in meters
n_lens <- 1.5         # refractive index of the lens
n_air <- 1.0          # refractive index of air

# Grid parameters
grid_size <- 80
x <- seq(-0.05, 0.05, length.out = grid_size)
y <- seq(-0.05, 0.05, length.out = grid_size)

# Create a meshgrid
mesh <- expand.grid(x = x, y = y)

# Define lens properties
lens_radius <- 0.02
lens_centers <- list(c(-0.025, -0.025), c(0.025, -0.025), c(-0.025, 0.025), c(0.025, 0.025))
focal_distances <- c(0.05, 0.053, 0.056, 0.059)  # Different focal distances for the lenses

# Function to calculate the phase shift through a lens
phase_shift <- function(x, y, center, focal_distance, wavelength) {
  r <- sqrt((x - center[1])^2 + (y - center[2])^2)
  phase <- (2 * pi / wavelength) * (sqrt(r^2 + focal_distance^2) - focal_distance)
  return(phase)
}

# Function to calculate the light intensity based on radial distance
radial_intensity <- function(r, radius) {
  intensity <- exp(-r^2 / (2 * (radius / 2)^2))  # Gaussian fall-off from center
  return(intensity)
}

# Function to calculate the intensity based on phase shifts and interference
intensity_function <- function(mesh, centers, radius, focal_distances, wavelength) {
  intensity <- matrix(0, nrow = grid_size, ncol = grid_size)
  for (i in 1:length(centers)) {
    center <- centers[[i]]
    focal_distance <- focal_distances[i]
    phase <- phase_shift(mesh$x, mesh$y, center, focal_distance, wavelength)
    amplitude <- exp(1i * phase)
    lens_intensity <- Re(amplitude * Conj(amplitude))

    # Calculate radial distance from the lens center
    distance <- sqrt((mesh$x - center[1])^2 + (mesh$y - center[2])^2)
    radial_intensity_value <- radial_intensity(distance, radius)

    # Apply radial intensity and mask-0.01
    mask <- distance <= radius+0.02
    lens_intensity <- lens_intensity * radial_intensity_value * mask

    intensity <- intensity + lens_intensity
  }
  return(intensity)
}

# Calculate intensity
intensity <- intensity_function(mesh, lens_centers, lens_radius, focal_distances, wavelength)

# Normalize and scale the intensity to match the given range (0.1 to 1.8)
intensity <- 0.1 + 1.7 * (intensity - min(intensity)) / (max(intensity) - min(intensity))

# Convert to data frame for ggplot
intensity_df <- data.frame(x = mesh$x, y = mesh$y, intensity = as.vector(intensity))

# Plot the result using ggplot2
ggplot(intensity_df, aes(x = x, y = y, fill = intensity)) +
  geom_tile() +
  scale_fill_gradientn(colours = rev(terrain.colors(10)), limits = c(0.1, 1.8)) +
  theme_minimal() +
  labs(title = "Transmittance as wavelength, λ = 650nm",
       fill = "T(%)",
       x = NULL,
       y = NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
+
  geom_hline(yintercept = 0.025)



intensity_df %>%
  filter(round(x, 3) == 0.025)   %>%
  ggplot( aes(x = y, y = intensity)) +
  geom_line()+
  #scale_fill_gradientn(colours = rev(terrain.colors(10)), limits = c(0.1, 1.8)) +
  theme_minimal() +
  labs(title = "Transmittance as wavelength, λ = 650nm",
       fill = "T(%)",
       x = NULL,
       y = NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

