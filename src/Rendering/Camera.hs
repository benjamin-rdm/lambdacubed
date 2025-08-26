module Rendering.Camera
  ( Camera (..),
    defaultCam,
    setPosAndAngles,
    applyMousePos,
    moveCamera,
    camDirectionVec,
    Direction (..),
  )
where

import Data.Maybe (fromMaybe)
import Linear

defaultCameraPosition :: V3 Float
defaultCameraPosition = V3 1.2 1.2 1.2

defaultCameraFront :: V3 Float
defaultCameraFront = normalize (V3 (-1) (-1) (-1))

defaultCameraUp :: V3 Float
defaultCameraUp = V3 0 0 1

defaultMouseSensitivity :: Double
defaultMouseSensitivity = 0.1

calculateDirectionVector :: Direction -> Camera -> V3 Float
calculateDirectionVector Forward (Camera _ front _ _ _) = front
calculateDirectionVector Backwards (Camera _ front _ _ _) = -front
calculateDirectionVector SidewaysL (Camera _ front up _ _) = cross up front
calculateDirectionVector SidewaysR (Camera _ front up _ _) = cross front up
calculateDirectionVector Up (Camera _ _ up _ _) = up
calculateDirectionVector Down (Camera _ _ up _ _) = -up

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

moveCameraByDirection :: Direction -> Float -> Camera -> Camera
moveCameraByDirection dir dist cam@(Camera pos _ _ pxy ppy) =
  Camera (pos + calculateDirectionVector dir cam ^* dist) front up pxy ppy
  where
    (Camera _ front up _ _) = cam

data Camera = Camera
  { cameraPos :: V3 Float,
    cameraFront :: V3 Float,
    cameraUp :: V3 Float,
    prevXY :: Maybe (Double, Double),
    prevPitchYaw :: (Float, Float)
  }

data Direction = Forward | Backwards | SidewaysL | SidewaysR | Up | Down

defaultCam :: Camera
defaultCam = Camera defaultCameraPosition defaultCameraFront defaultCameraUp Nothing (0, -90)

camDirectionVec :: Direction -> Camera -> V3 Float
camDirectionVec = calculateDirectionVector

moveCamera :: Direction -> Float -> Camera -> Camera
moveCamera = moveCameraByDirection

applyMousePos :: (Double, Double) -> Camera -> Camera
applyMousePos (xpos, ypos) (Camera pos _ up prev (prevPitch, prevYaw)) =
  let (deltaX, deltaY) = calculateMouseDelta (xpos, ypos) prev
      deltaYawDeg = deltaX * defaultMouseSensitivity
      deltaPitchDeg = deltaY * defaultMouseSensitivity
      pitchDeg' = clampPitch (prevPitch + realToFrac deltaPitchDeg)
      yawDeg' = wrapYaw (prevYaw + realToFrac deltaYawDeg)
      pitch = degreesToRadians pitchDeg'
      yaw = degreesToRadians yawDeg'
      newFront = calculateFrontVector pitch yaw
   in Camera pos newFront up (Just (xpos, ypos)) (pitchDeg', yawDeg')

setPosAndAngles :: V3 Float -> (Float, Float) -> Camera -> Camera
setPosAndAngles p (pitchDeg, yawDeg) (Camera _ _ up oldPrev _) =
  let pitch = degreesToRadians pitchDeg
      yaw = degreesToRadians yawDeg
      newFront = calculateFrontVector pitch yaw
   in Camera p newFront up oldPrev (pitchDeg, yawDeg)
