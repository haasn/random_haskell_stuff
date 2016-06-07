-- Constants as per specification

rL = 0.299
bL = 0.114
gL = 1 - rL - bL

uMax = 1
vMax = 1
wMax = 1

y' (r,g,b) = rL*r + gL*g + bL*b

v c@(r,_,_) = vMax * (r - y' c) / (1 - rL)
w c@(_,g,_) = wMax * (g - y' c) / (1 - gL)
u c@(_,_,b) = uMax * (b - y' c) / (1 - bL)

-- Inversion

yuvRgb (y',u,v) = (r,g,b) where
  r = y' + v * (1 - rL) / vMax
  b = y' + u * (1 - bL) / uMax
  g = (y' - rL * r - bL * b) / gL

yuwRgb (y',u,w) = (r,g,b) where
  g = y' + w * (1 - gL) / wMax
  b = y' + u * (1 - bL) / uMax
  r = (y' - gL * g - bL * b) / rL
