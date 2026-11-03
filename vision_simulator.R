rm(list=ls(all=TRUE)) # clean work space, common practice
#Install packages
install.packages(c("pavo", "jpeg", "png", "imager"))

library(pavo)
library(jpeg)
library(png)
library(imager)

#Define the lambda max values for visual cone opsins
#retruns a normalised matrix of sensitivities
wl <- 300:700 #visible light spectrum


deer_SW <- sensmodel(443)
deer_LW <- sensmodel(538)
class(deer_SW)

deer_cones <- sensmodel(c(443, 538), beta = FALSE, integrate = FALSE) 
plot(deer_cones)


#CIE space:
vis.deer <- vismodel(deer_cones,
                        visual = 'cie10',
                        illum = 'D65',
                        vonkries = TRUE,
                        relative = FALSE,
                        achromatic = 'none')

ciexyz.deer <- colspace(vis.deer, space = 'ciexyz')
head(ciexyz.deer)

plot(ciexyz.deer, pch = 21, bg = spec2rgb(deer_cones))

#standard dichromat simulator: ####
#1 load and linearise the chosen image (needs to be jpeg format)
img <- load.image("IMG_6382.jpeg")

# Convert to linear RGB (important)
img_lin <- img ^ 2.2
dim(img_lin) #check dimensions, width x length x colours x frames
#colours should be 3, if 1 then the image is being misread as grey scale (can happen for ohone pics)

#to switch from grey scale to colour
img_tmp <- imappend(
  list(
    as.cimg(img_lin[,,1,1]),
    as.cimg(img_lin[,,1,2]),
    as.cimg(img_lin[,,1,3])
  ),
  axis = "t"
)

dim(img_tmp)

img_rgb <- as.cimg(aperm(img_tmp, c(1, 2, 4, 3)))
dim(img_rgb)

#extract RGB channels from image to estimate cone catch 
# Extract RGB channels
R <- as.numeric(img_rgb[,,1,1])
G <- as.numeric(img_rgb[,,2,1])
B <- as.numeric(img_rgb[,,3,1])

rgb <- cbind(R, G, B)

# Approximate mapping matrix
M <- matrix(c(
  0.1, 0.3, 0.6,   # S cone #430-450 ish (chatgpt)
  0.6, 0.35, 0.05  # M/L cone #530-540 ish (chatgpt)
), nrow = 2, byrow = TRUE)

cones <- rgb %*% t(M)
cones <- cones / max(cones)  # normalize

#re-map dichromatic space (from our matrix) to RGB (SW -> blue; MW -> green)
out_R <- cones[,2]
out_G <- cones[,2]
out_B <- cones[,1]

out <- cbind(out_R, out_G, out_B)
out <- out / max(out)

#reshape back into image (witchcraft?!!)
dim(out) <- dim(img_lin)
out_img <- as.cimg(out)

out_img <- out_img^(1/2.2)
plot(out_img)

#reindeer specific vision simulator ####
#1 load and linearise the chosen image (needs to be jpeg format)
img <- load.image("IMG_6382.jpeg")

# Convert to linear RGB (important)
img_lin <- img ^ 2.2
dim(img_lin) #check dimensions, width x length x colours x frames
#colours should be 3, if 1 then the image is being misread as grey scale (can happen for ohone pics)

#to switch from grey scale to colour
img_tmp <- imappend(
  list(
    as.cimg(img_lin[,,1,1]),
    as.cimg(img_lin[,,1,2]),
    as.cimg(img_lin[,,1,3])
  ),
  axis = "t"
)

dim(img_tmp)

img_rgb <- as.cimg(aperm(img_tmp, c(1, 2, 4, 3)))
dim(img_rgb)

#extract RGB channels from image to estimate cone catch 
# Extract RGB channels
R <- as.numeric(img_rgb[,,1,1])
G <- as.numeric(img_rgb[,,2,1])
B <- as.numeric(img_rgb[,,3,1])

rgb <- cbind(R, G, B)

# Approximate mapping matrix (M)
deer_s <- deer_cones[,2]  # S cone #reindeer specific
deer_m <- deer_cones[,3]  # M/L cone #reindeer specific

r_band <- which(wl >= 600 & wl <= 700)
g_band <- which(wl >= 500 & wl <= 600)
b_band <- which(wl >= 400 & wl <= 500)

S_weights <- c(
  mean(deer_s[b_band]),
  mean(deer_s[g_band]),
  mean(deer_s[r_band])
)

M_weights <- c(
  mean(deer_m[b_band]),
  mean(deer_m[g_band]),
  mean(deer_m[r_band])
)

#normalise
M <- rbind(S_weights, M_weights)
M <- M / rowSums(M)
View(M)
M <- M[, c(3,2,1)]  # swap columns 1 and 3 #because it 

cones <- rgb %*% t(M)
cones <- cones / max(cones)  # normalize

#re-map dichromatic space (from our matrix) to RGB (SW -> blue; MW -> green)
out_R <- cones[,2]
out_G <- cones[,2]
out_B <- cones[,1]

out <- cbind(out_R, out_G, out_B)
out <- out / max(out)

#reshape back into image (witchcraft?!!)
dim(out) <- dim(img_lin)
out_img <- as.cimg(out)

out_img <- out_img^(1/2.2)
plot(out_img)



####
#don't know if following code is still useful and scared to delete it
deer_s <- deer_cones[,2] / max(deer_cones[,2])   # S cone
deer_m <- deer_cones[,3] / max(deer_cones[,3])   # M/L cone

# define wavelength ranges for RGB channels
r_band <- 600:700
g_band <- 500:600
b_band <- 400:500

S_weights <- c(
  mean(deer_s[b_band]),
  mean(deer_s[g_band]),
  mean(deer_s[r_band])
)

M_weights <- c(
  mean(deer_m[b_band]),
  mean(deer_m[g_band]),
  mean(deer_m[r_band])
)

# assemble matrix
M <- rbind(S_weights, M_weights)

#normalise rows
M <- M / rowSums(M)

# assuming R, G, B are numeric vectors of the image
rgb_matrix <- cbind(R, G, B)

# compute cone catches
cones <- rgb_matrix %*% t(M)


#####
#mapping correctly
wl <- 300:700   # wavelengths used in sensmodel
deer_s <- deer_cones[,2]  # S cone
deer_m <- deer_cones[,3]  # M/L cone

r_band <- which(wl >= 600 & wl <= 700)
g_band <- which(wl >= 500 & wl <= 600)
b_band <- which(wl >= 400 & wl <= 500)

S_weights <- c(
  mean(deer_s[b_band]),
  mean(deer_s[g_band]),
  mean(deer_s[r_band])
)

M_weights <- c(
  mean(deer_m[b_band]),
  mean(deer_m[g_band]),
  mean(deer_m[r_band])
)

#normalise
M <- rbind(S_weights, M_weights)
M <- M / rowSums(M)


