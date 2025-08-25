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
    walkSpeed,
    jumpSpeed,
  )
where

import Game.World (Block (..), blockOpaque)
import Linear hiding (nearZero)

calculateBoundingBox :: V3 Float -> (V3 Float, V3 Float)
calculateBoundingBox (V3 px py pz) =
  ( V3 (px - playerHalfWidth) (py - playerHalfWidth) pz,
    V3 (px + playerHalfWidth) (py + playerHalfWidth) (pz + playerHeight)
  )

checkCollision :: (V3 Int -> Block) -> (V3 Float, V3 Float) -> Bool
checkCollision blockAtW (minP, maxP) =
  let (V3 ix0 iy0 iz0) = floor <$> minP
      (V3 ix1 iy1 iz1) = floor <$> maxP
      isSolid ix iy iz = blockOpaque (blockAtW (V3 ix iy iz))
   in or
        [ isSolid ix iy iz
          | ix <- [ix0 .. ix1],
            iy <- [iy0 .. iy1],
            iz <- [iz0 .. iz1]
        ]

normalizeVelocity :: V3 Float -> V3 Float
normalizeVelocity wish = if nearZero wish then V3 0 0 0 else normalize wish

calculateAxisMovement :: (V3 Int -> Block) -> Int -> Float -> V3 Float -> V3 Float
calculateAxisMovement blockAtW axis delta pos0
  | abs delta < 1e-8 = pos0
  | otherwise =
      let maxStep = 0.1 :: Float
          stepsN = max 1 (ceiling (abs delta / maxStep) :: Int)
          stepSz = delta / fromIntegral stepsN
          moveOnce (V3 x y z) = case axis of
            0 -> V3 (x + stepSz) y z
            1 -> V3 x (y + stepSz) z
            _ -> V3 x y (z + stepSz)
          go 0 p = p
          go n p =
            let p' = moveOnce p
             in if checkCollision blockAtW (calculateBoundingBox p') then p else go (n - 1) p'
       in go stepsN pos0

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
playerBoundingBox = calculateBoundingBox

epsilon :: Float
epsilon = 1e-4

type BlockQuery = V3 Int -> Block

collides :: BlockQuery -> (V3 Float, V3 Float) -> Bool
collides = checkCollision

slideAxis :: BlockQuery -> Int -> Float -> V3 Float -> V3 Float
slideAxis = calculateAxisMovement

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

applyMovement :: BlockQuery -> V3 Float -> V3 Float -> Float -> V3 Float
applyMovement blockAtW pos0 velocity dt =
  let (V3 vx vy vz) = velocity
      pos1 = slideAxis blockAtW 0 (vx * dt) pos0
      pos2 = slideAxis blockAtW 1 (vy * dt) pos1
   in slideAxis blockAtW 2 (vz * dt) pos2

checkGroundCollision :: BlockQuery -> V3 Float -> Bool
checkGroundCollision blockAtW pos3 =
  let (V3 minX minY minZ, V3 maxX maxY _) = playerBoundingBox pos3
      ix0 = floor minX
      ix1 = floor (maxX - epsilon)
      iy0 = floor minY
      iy1 = floor (maxY - epsilon)
      bz = floor (minZ - epsilon)
   in any
        (\(ix, iy) -> blockOpaque (blockAtW (V3 ix iy bz)))
        [(ix, iy) | ix <- [ix0 .. ix1], iy <- [iy0 .. iy1]]

stepPlayer :: BlockQuery -> V3 Float -> InputState -> Float -> Player -> Player
stepPlayer blockAtW camForward inputState dt player0 =
  let pos0 = plPos player0
      onGroundBefore = grounded blockAtW pos0

      wish = calculateWishDirection camForward inputState
      horiz = calculateHorizontalVelocity wish

      vx = horiz `dot` V3 1 0 0
      vy = horiz `dot` V3 0 1 0
      vz = calculateVerticalVelocity onGroundBefore (inJump inputState) (plVel player0) dt

      pos1 = applyMovement blockAtW pos0 (V3 vx vy vz) dt

      onGroundAfter = checkGroundCollision blockAtW pos1
      vz' = if onGroundAfter && vz < 0 then 0 else vz
   in Player pos1 (V3 vx vy vz')

nearZero :: V3 Float -> Bool
nearZero v = quadrance v < 1e-12

grounded :: BlockQuery -> V3 Float -> Bool
grounded blockAtW (V3 px py pz) =
  let probe = 0.2 :: Float
   in collides blockAtW (playerBoundingBox (V3 px py (pz - probe)))
