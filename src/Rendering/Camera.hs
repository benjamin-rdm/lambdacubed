module Rendering.Camera
  ( Camera (..),
    defaultCam,
    setPosAndAngles,
    applyMousePos,
    Direction (..),
  )
where

import Data.Maybe (fromMaybe)
import Linear

defaultCameraPosition :: V3 Float
defaultCameraPosition = V3 1.2 1.2 1.2

defaultCameraFront :: V3 Float
defaultCameraFront = normalize (V3 (-1) (-1) (-1))

defaultMouseSensitivity :: Double
defaultMouseSensitivity = 0.1

calculateFrontVector :: Float -> Float -> V3 Float
calculateFrontVector pitch yaw =
  normalize $
    V3
      (sin yaw * cos pitch)
      (cos yaw * cos pitch)
      (sin pitch)

clampPitch :: Float -> Float
clampPitch pitch = max (-89) (min 89 pitch)

wrapYaw :: Float -> Float
wrapYaw yaw =
  let yaw' = realToFrac yaw :: Double
   in realToFrac (((yaw' + 180) - fromIntegral (floor ((yaw' + 180) / 360) :: Int) * 360) - 180)

calculateMouseDelta :: (Double, Double) -> Maybe (Double, Double) -> (Double, Double)
calculateMouseDelta (xpos, ypos) prev =
  let (prevX, prevY) = fromMaybe (xpos, ypos) prev
   in (xpos - prevX, prevY - ypos) -- Note: Y is inverted

degreesToRadians :: Float -> Float
degreesToRadians = (* (pi / 180))

data Camera = Camera
  { cameraPos :: V3 Float,
    cameraFront :: V3 Float,
    prevXY :: Maybe (Double, Double)
  }

data Direction = Forward | Backwards | SidewaysL | SidewaysR | Up | Down

defaultCam :: Camera
defaultCam = Camera defaultCameraPosition defaultCameraFront Nothing

frontToAnglesDeg :: V3 Float -> (Float, Float)
frontToAnglesDeg (V3 fx fy fz) =
  let pitch = asin (max (-1) (min 1 fz))
      yaw = atan2 fx fy
   in (pitch * 180 / pi, yaw * 180 / pi)

applyMousePos :: (Double, Double) -> Camera -> Camera
applyMousePos (xpos, ypos) (Camera pos front prev) =
  let (deltaX, deltaY) = calculateMouseDelta (xpos, ypos) prev
      (prevPitch, prevYaw) = frontToAnglesDeg front
      deltaYawDeg = deltaX * defaultMouseSensitivity
      deltaPitchDeg = deltaY * defaultMouseSensitivity
      pitchDeg' = clampPitch (prevPitch + realToFrac deltaPitchDeg)
      yawDeg' = wrapYaw (prevYaw + realToFrac deltaYawDeg)
      pitch = degreesToRadians pitchDeg'
      yaw = degreesToRadians yawDeg'
      newFront = calculateFrontVector pitch yaw
   in Camera pos newFront (Just (xpos, ypos))

setPosAndAngles :: V3 Float -> (Float, Float) -> Camera -> Camera
setPosAndAngles p (pitchDeg, yawDeg) (Camera _ _ oldPrev) =
  let pitch = degreesToRadians pitchDeg
      yaw = degreesToRadians yawDeg
      newFront = calculateFrontVector pitch yaw
   in Camera p newFront oldPrev
