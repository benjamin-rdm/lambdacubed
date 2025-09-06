module Game.Physics
  ( Player (..),
    InputState (..),
    playerEyeHeight,
    stepPlayer,
    calculateWishDirection,
    calculateHorizontalVelocity,
    calculateVerticalVelocity,
    playerHalfWidth,
    playerHeight,
    gravityAccel,
  )
where

import Game.World (blockOpaque)
import Game.WorldManager (MonadWorld (..))
import Linear hiding (nearZero)
import Utils.Monad (anyM)

normalizeVelocity :: V3 Float -> V3 Float
normalizeVelocity wish = if nearZero wish then V3 0 0 0 else normalize wish

data InputState = InputState
  { inForward :: !Bool,
    inBackward :: !Bool,
    inLeft :: !Bool,
    inRight :: !Bool,
    inJump :: !Bool
  }
  deriving (Show, Eq)

data Player = Player
  { plPos :: !(V3 Float),
    plVel :: !(V3 Float)
  }
  deriving (Show, Eq)

playerHalfWidth :: Float
playerHalfWidth = 0.3

playerHeight :: Float
playerHeight = 1.8

playerEyeHeight :: Float
playerEyeHeight = 1.6

gravityAccel :: Float
gravityAccel = 25.0

walkSpeed :: Float
walkSpeed = 6.0

jumpSpeed :: Float
jumpSpeed = 8.0

playerBoundingBox :: V3 Float -> (V3 Float, V3 Float)
playerBoundingBox (V3 px py pz) =
  ( V3 (px - playerHalfWidth) (py - playerHalfWidth) pz,
    V3 (px + playerHalfWidth) (py + playerHalfWidth) (pz + playerHeight)
  )

collides :: (MonadWorld m) => (V3 Float, V3 Float) -> m Bool
collides (minP, maxP) = do
  let (V3 ix0 iy0 iz0) = floor <$> minP
      (V3 ix1 iy1 iz1) = floor <$> maxP
      cells = [V3 ix iy iz | ix <- [ix0 .. ix1], iy <- [iy0 .. iy1], iz <- [iz0 .. iz1]]
  anyM isSolidM cells

isSolidM :: (MonadWorld m) => V3 Int -> m Bool
isSolidM v = do
  mb <- blockAtMaybe v
  pure (maybe True blockOpaque mb)

slideAxis :: (MonadWorld m) => Int -> Float -> V3 Float -> m (V3 Float)
slideAxis axis delta pos0
  | abs delta < 1e-8 = pure pos0
  | otherwise = do
      let maxStep = 0.1 :: Float
          stepsN = max 1 (ceiling (abs delta / maxStep) :: Int)
          stepSz = delta / fromIntegral stepsN
          moveOnce (V3 x y z) = case axis of
            0 -> V3 (x + stepSz) y z
            1 -> V3 x (y + stepSz) z
            _ -> V3 x y (z + stepSz)
          go 0 p = pure p
          go n p = do
            let p' = moveOnce p
            coll <- collides (playerBoundingBox p')
            if coll then pure p else go (n - 1) p'
      go stepsN pos0

calculateWishDirection :: V3 Float -> InputState -> V3 Float
calculateWishDirection camForward (InputState goF goB goL goR _) =
  let fwd = normalizeVelocity (camForward * V3 1 1 0)
      right = normalizeVelocity (cross fwd (V3 0 0 1))
      ax = (if goF then 1 else 0) - (if goB then 1 else 0) :: Float
      ay = (if goR then 1 else 0) - (if goL then 1 else 0) :: Float
   in ax *^ fwd + ay *^ right

calculateHorizontalVelocity :: V3 Float -> V3 Float
calculateHorizontalVelocity wish = walkSpeed *^ normalizeVelocity wish

calculateVerticalVelocity :: Bool -> Bool -> V3 Float -> Float -> Float
calculateVerticalVelocity onGround wantJump velocity dt =
  let vz0 = velocity `dot` V3 0 0 1
      vz1 = if onGround then vz0 else vz0 - gravityAccel * dt
   in if onGround && wantJump then jumpSpeed else vz1

applyMovement :: (MonadWorld m) => V3 Float -> V3 Float -> Float -> m (V3 Float)
applyMovement pos0 velocity dt = do
  let (V3 vx vy vz) = velocity
  pos1 <- slideAxis 0 (vx * dt) pos0
  pos2 <- slideAxis 1 (vy * dt) pos1
  slideAxis 2 (vz * dt) pos2

stepPlayer :: (MonadWorld m) => V3 Float -> InputState -> Float -> Player -> m Player
stepPlayer camForward inputState dt player0 = do
  let pos0 = plPos player0
  onGroundBefore <- grounded pos0

  let wish = calculateWishDirection camForward inputState
      horiz = calculateHorizontalVelocity wish

      vx = horiz `dot` V3 1 0 0
      vy = horiz `dot` V3 0 1 0
      vz = calculateVerticalVelocity onGroundBefore (inJump inputState) (plVel player0) dt

  pos1 <- applyMovement pos0 (V3 vx vy vz) dt

  onGroundAfter <- grounded pos1
  let vz' = if onGroundAfter && vz < 0 then 0 else vz
  pure (Player pos1 (V3 vx vy vz'))

nearZero :: V3 Float -> Bool
nearZero v = quadrance v < 1e-12

grounded :: (MonadWorld m) => V3 Float -> m Bool
grounded (V3 px py pz) =
  let probe = 0.2 :: Float
   in collides (playerBoundingBox (V3 px py (pz - probe)))
