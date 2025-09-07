{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Rendering.Shader.Typed where

import Control.Monad.Writer.Strict
  ( MonadWriter (tell),
    Writer,
    WriterT (WriterT),
    execWriter,
    runWriter,
  )
import Data.Functor (($>))
import GHC.TypeNats (Nat, type (+), type (<=))
import Rendering.Shader.AST
  ( BinOp (Add, Div, Eq, Le, Lt, Mul, Sub),
    Datatype
      ( BoolT,
        FloatT,
        IntT,
        Mat4,
        Sampler2D,
        Sampler2DArray,
        V2,
        V3,
        V4
      ),
    Expr (BinOp, Call, Constructor, Field, LitFloat, LitInt, Var),
    Function (Function, sfArgs, sfBody, sfName, sfType),
    ShaderSource (..),
    ShaderType (..),
    Statement (Assign, Decl, Discard, If),
  )

newtype Name (t :: Datatype) = Name {nStr :: String}

{-
TODO: Investigate if there is actual value in having this typed AST. 
Alternative approach: Wrap values of untyped AST in a newtype with a type-level (OpenGL-)type parameter
This would heavily reduce the code duplication and since we can already enforce some invariants
only via the "smart" constructors and not the GADT itself (see below), this might provide the 
same effective type safety in the OpenGL EDSL.
-}
data TypedExpr (t :: Datatype) where
  TypedVar :: Name t -> TypedExpr t
  TypedLitF :: Double -> TypedExpr 'FloatT
  TypedLitI :: Int -> TypedExpr 'IntT
  TypedVec2 :: [Expr] -> TypedExpr 'V2
  TypedVec3 :: [Expr] -> TypedExpr 'V3
  TypedVec4 :: [Expr] -> TypedExpr 'V4
  -- We have no nice way of enforcing the type safetly of Vector constructors in the Typed AST
  TypedSwizzleX :: (1 <= Len t) => TypedExpr t -> TypedExpr 'FloatT
  TypedSwizzleY :: (2 <= Len t) => TypedExpr t -> TypedExpr 'FloatT
  TypedSwizzleZ :: (3 <= Len t) => TypedExpr t -> TypedExpr 'FloatT
  TypedSwizzleA :: (4 <= Len t) => TypedExpr t -> TypedExpr 'FloatT
  TypedSwizzleXY :: (2 <= Len t) => TypedExpr t -> TypedExpr 'V2
  TypedSwizzleXYZ :: (3 <= Len t) => TypedExpr t -> TypedExpr 'V3
  TypedSwizzleRGB :: (3 <= Len t) => TypedExpr t -> TypedExpr 'V3
  TypedAdd :: TypedExpr 'FloatT -> TypedExpr 'FloatT -> TypedExpr 'FloatT
  TypedSub :: TypedExpr 'FloatT -> TypedExpr 'FloatT -> TypedExpr 'FloatT
  TypedDiv :: TypedExpr 'FloatT -> TypedExpr 'FloatT -> TypedExpr 'FloatT
  TypedMult :: (TypedMultiply a b r) => TypedExpr a -> TypedExpr b -> TypedExpr r
  TypedClamp :: TypedExpr 'FloatT -> TypedExpr 'FloatT -> TypedExpr 'FloatT -> TypedExpr 'FloatT
  TypedMixV3 :: TypedExpr 'V3 -> TypedExpr 'V3 -> TypedExpr 'FloatT -> TypedExpr 'V3
  TypedLength :: TypedExpr 'V3 -> TypedExpr 'FloatT
  TypedFloor :: TypedExpr 'FloatT -> TypedExpr 'FloatT
  TypedMod :: TypedExpr 'FloatT -> TypedExpr 'FloatT -> TypedExpr 'FloatT
  TypedEq :: TypedExpr 'FloatT -> TypedExpr 'FloatT -> TypedExpr 'BoolT
  TypedLt :: TypedExpr 'FloatT -> TypedExpr 'FloatT -> TypedExpr 'BoolT
  TypedLe :: TypedExpr 'FloatT -> TypedExpr 'FloatT -> TypedExpr 'BoolT
  TypedTexture2d :: Name 'Sampler2D -> TypedExpr 'V2 -> TypedExpr 'V4
  TypedTexture2dArray :: Name 'Sampler2DArray -> TypedExpr 'V3 -> TypedExpr 'V4

data TStmt where
  TDeclF :: String -> Maybe (TypedExpr 'FloatT) -> TStmt
  TDeclV2 :: String -> Maybe (TypedExpr 'V2) -> TStmt
  TDeclV3 :: String -> Maybe (TypedExpr 'V3) -> TStmt
  TDeclV4 :: String -> Maybe (TypedExpr 'V4) -> TStmt
  TAssign :: Name t -> TypedExpr t -> TStmt
  TAssignGL :: TypedExpr 'V4 -> TStmt
  TIf :: TypedExpr 'BoolT -> [TStmt] -> TStmt
  TDiscard :: TStmt

data Acc = Acc
  { aIns :: [(Datatype, String)],
    aOuts :: [(Datatype, String)],
    aUnis :: [(Datatype, String)],
    aBodyT :: [TStmt]
  }

instance Semigroup Acc where
  (<>) :: Acc -> Acc -> Acc
  Acc i1 o1 u1 b1 <> Acc i2 o2 u2 b2 = Acc (i1 <> i2) (o1 <> o2) (u1 <> u2) (b1 <> b2)

instance Monoid Acc where
  mempty :: Acc
  mempty = Acc [] [] [] []

newtype ShaderT a = ShaderT {unT :: Writer Acc a}
  deriving (Functor, Applicative, Monad)

runVertexT :: ShaderT a -> ShaderSource
runVertexT = runWith VertexShader

runFragmentT :: ShaderT a -> ShaderSource
runFragmentT = runWith FragmentShader

runWith :: ShaderType -> ShaderT a -> ShaderSource
runWith ty (ShaderT m) =
  let acc = execWriter m
      fun = Function {sfType = ty, sfName = "main", sfArgs = [], sfBody = map untypeS (aBodyT acc)}
   in ShaderSource
        { ssType = ty,
          ssIns = dedupeDecls (aIns acc),
          ssOuts = dedupeDecls (aOuts acc),
          ssUniforms = dedupeDecls (aUnis acc),
          ssFunctions = pure fun
        }

appendBody :: TStmt -> ShaderT ()
appendBody s = ShaderT $ tell mempty {aBodyT = [s]}

inF :: String -> ShaderT (Name 'FloatT)
inF s = ShaderT $ tell mempty {aIns = [(FloatT, s)]} $> Name s

inV2 :: String -> ShaderT (Name 'V2)
inV2 s = ShaderT $ tell mempty {aIns = [(V2, s)]} $> Name s

inV3 :: String -> ShaderT (Name 'V3)
inV3 s = ShaderT $ tell mempty {aIns = [(V3, s)]} $> Name s

inV4 :: String -> ShaderT (Name 'V4)
inV4 s = ShaderT $ tell mempty {aIns = [(V4, s)]} $> Name s

outF :: String -> ShaderT (Name 'FloatT)
outF s = ShaderT $ tell mempty {aOuts = [(FloatT, s)]} $> Name s

outV2 :: String -> ShaderT (Name 'V2)
outV2 s = ShaderT $ tell mempty {aOuts = [(V2, s)]} $> Name s

outV3 :: String -> ShaderT (Name 'V3)
outV3 s = ShaderT $ tell mempty {aOuts = [(V3, s)]} $> Name s

outV4 :: String -> ShaderT (Name 'V4)
outV4 s = ShaderT $ tell mempty {aOuts = [(V4, s)]} $> Name s

uniformFloat :: String -> ShaderT (Name 'FloatT)
uniformFloat s = ShaderT $ tell mempty {aUnis = [(FloatT, s)]} $> Name s

uniformV3 :: String -> ShaderT (Name 'V3)
uniformV3 s = ShaderT $ tell mempty {aUnis = [(V3, s)]} $> Name s

uniformMat4 :: String -> ShaderT (Name 'Mat4)
uniformMat4 s = ShaderT $ tell mempty {aUnis = [(Mat4, s)]} $> Name s

uniformSampler2D :: String -> ShaderT (Name 'Sampler2D)
uniformSampler2D s = ShaderT $ tell mempty {aUnis = [(Sampler2D, s)]} $> Name s

uniformSampler2DArray :: String -> ShaderT (Name 'Sampler2DArray)
uniformSampler2DArray s = ShaderT $ tell mempty {aUnis = [(Sampler2DArray, s)]} $> Name s

localF :: String -> Maybe (TypedExpr 'FloatT) -> ShaderT (Name 'FloatT)
localF s mi = appendBody (TDeclF s mi) $> Name s

localV2 :: String -> Maybe (TypedExpr 'V2) -> ShaderT (Name 'V2)
localV2 s mi = appendBody (TDeclV2 s mi) $> Name s

localV3 :: String -> Maybe (TypedExpr 'V3) -> ShaderT (Name 'V3)
localV3 s mi = appendBody (TDeclV3 s mi) $> Name s

localV4 :: String -> Maybe (TypedExpr 'V4) -> ShaderT (Name 'V4)
localV4 s mi = appendBody (TDeclV4 s mi) $> Name s

use :: Name t -> TypedExpr t
use = TypedVar

litF :: Double -> TypedExpr 'FloatT
litF = TypedLitF

litI :: Int -> TypedExpr 'IntT
litI = TypedLitI

class ToArgs a where
  type TotalLen a :: Nat
  toArgs :: a -> [Expr]

instance ToArgs (TypedExpr t) where
  type TotalLen (TypedExpr t) = Len t
  toArgs :: TypedExpr t -> [Expr]
  toArgs v = [untypeE v]

instance ToArgs Double where
  type TotalLen Double = 1
  toArgs :: Double -> [Expr]
  toArgs d = [untypeE (litF d)]

instance (ToArgs a1, ToArgs a2) => ToArgs (a1, a2) where
  type TotalLen (a1, a2) = TotalLen a1 + TotalLen a2
  toArgs :: (ToArgs a1, ToArgs a2) => (a1, a2) -> [Expr]
  toArgs (p1, p2) = toArgs p1 ++ toArgs p2

instance (ToArgs a1, ToArgs a2, ToArgs a3) => ToArgs (a1, a2, a3) where
  type TotalLen (a1, a2, a3) = TotalLen a1 + TotalLen a2 + TotalLen a3
  toArgs :: (ToArgs a1, ToArgs a2, ToArgs a3) => (a1, a2, a3) -> [Expr]
  toArgs (p1, p2, p3) = toArgs p1 ++ toArgs p2 ++ toArgs p3

instance (ToArgs a1, ToArgs a2, ToArgs a3, ToArgs a4) => ToArgs (a1, a2, a3, a4) where
  type TotalLen (a1, a2, a3, a4) = TotalLen a1 + TotalLen a2 + TotalLen a3 + TotalLen a4
  toArgs :: (ToArgs a1, ToArgs a2, ToArgs a3, ToArgs a4) => (a1, a2, a3, a4) -> [Expr]
  toArgs (p1, p2, p3, p4) = toArgs p1 ++ toArgs p2 ++ toArgs p3 ++ toArgs p4

vec2 :: (ToArgs a, TotalLen a ~ 2) => a -> TypedExpr 'V2
vec2 = TypedVec2 . toArgs

vec3 :: (ToArgs a, TotalLen a ~ 3) => a -> TypedExpr 'V3
vec3 = TypedVec3 . toArgs

vec4 :: (ToArgs a, TotalLen a ~ 4) => a -> TypedExpr 'V4
vec4 = TypedVec4 . toArgs

type family Len (t :: Datatype) :: Nat where
  Len 'FloatT = 1
  Len 'V2 = 2
  Len 'V3 = 3
  Len 'V4 = 4

x :: (1 <= Len t) => TypedExpr t -> TypedExpr 'FloatT
x = TypedSwizzleX

y :: (2 <= Len t) => TypedExpr t -> TypedExpr 'FloatT
y = TypedSwizzleY

z :: (3 <= Len t) => TypedExpr t -> TypedExpr 'FloatT
z = TypedSwizzleZ

xy :: (2 <= Len t) => TypedExpr t -> TypedExpr 'V2
xy = TypedSwizzleXY

xyz :: (3 <= Len t) => TypedExpr t -> TypedExpr 'V3
xyz = TypedSwizzleXYZ

rgb :: (3 <= Len t) => TypedExpr t -> TypedExpr 'V3
rgb = TypedSwizzleRGB

a :: (4 <= Len t) => TypedExpr t -> TypedExpr 'FloatT
a = TypedSwizzleA

addF :: (ToScalar a, ToScalar b) => a -> b -> TypedExpr 'FloatT
addF f g = TypedAdd (toFloat f) (toFloat g)

subF :: (ToScalar a, ToScalar b) => a -> b -> TypedExpr 'FloatT
subF f g = TypedSub (toFloat f) (toFloat g)

mulF :: (ToScalar a, ToScalar b) => a -> b -> TypedExpr 'FloatT
mulF f g = TypedMult (toFloat f) (toFloat g)

divF :: (ToScalar a, ToScalar b) => a -> b -> TypedExpr 'FloatT
divF f g = TypedDiv (toFloat f) (toFloat g)

class TypedMultiply a b r | a b -> r where
  (.*.) :: TypedExpr a -> TypedExpr b -> TypedExpr r
  (.*.) = TypedMult

infixl 7 .*.

instance TypedMultiply 'FloatT 'FloatT 'FloatT

instance TypedMultiply 'V2 'V2 'V2

instance TypedMultiply 'V3 'V3 'V3

instance TypedMultiply 'V3 'FloatT 'V3

instance TypedMultiply 'Mat4 'V4 'V4

instance TypedMultiply 'Mat4 'Mat4 'Mat4

class ToScalar a where
  toFloat :: a -> TypedExpr 'FloatT

instance ToScalar (TypedExpr 'FloatT) where
  toFloat :: TypedExpr 'FloatT -> TypedExpr 'FloatT
  toFloat = id

instance ToScalar Double where
  toFloat :: Double -> TypedExpr 'FloatT
  toFloat = litF

clamp :: (ToScalar a, ToScalar b) => TypedExpr 'FloatT -> (a, b) -> TypedExpr 'FloatT
clamp f1 (f2, f3) = TypedClamp f1 (toFloat f2) (toFloat f3)

clamp01 :: TypedExpr 'FloatT -> TypedExpr 'FloatT
clamp01 f1 = clamp f1 (0.0 :: Double, 1.0 :: Double)

(.==.) :: (ToScalar a, ToScalar b) => a -> b -> TypedExpr 'BoolT
(.==.) f1 f2 = TypedEq (toFloat f1) (toFloat f2)

infix 4 .==.

ltF :: (ToScalar a, ToScalar b) => a -> b -> TypedExpr 'BoolT
ltF f1 f2 = TypedLt (toFloat f1) (toFloat f2)

leF :: (ToScalar a, ToScalar b) => a -> b -> TypedExpr 'BoolT
leF f1 f2 = TypedLe (toFloat f1) (toFloat f2)

modF :: (ToScalar a, ToScalar b) => a -> b -> TypedExpr 'FloatT
modF f g = TypedMod (toFloat f) (toFloat g)

mixV3 :: TypedExpr 'V3 -> TypedExpr 'V3 -> TypedExpr 'FloatT -> TypedExpr 'V3
mixV3 = TypedMixV3

length3 :: TypedExpr 'V3 -> TypedExpr 'FloatT
length3 = TypedLength

floorF :: TypedExpr 'FloatT -> TypedExpr 'FloatT
floorF = TypedFloor

texture2D :: Name 'Sampler2D -> TypedExpr 'V2 -> TypedExpr 'V4
texture2D = TypedTexture2d

texture2DArray :: Name 'Sampler2DArray -> TypedExpr 'V3 -> TypedExpr 'V4
texture2DArray = TypedTexture2dArray

assignN :: Name t -> TypedExpr t -> ShaderT ()
assignN nm v = appendBody (TAssign nm v)

assignGLPosition :: TypedExpr 'V4 -> ShaderT ()
assignGLPosition v = appendBody (TAssignGL v)

dedupeDecls :: [(Datatype, String)] -> [(Datatype, String)]
dedupeDecls = foldr go []
  where
    go (dt, n) acc =
      case lookup n (fmap (\(d, name) -> (name, d)) acc) of
        Nothing -> (dt, n) : acc
        Just _ -> acc

withBlock :: ShaderT () -> ShaderT [TStmt]
withBlock (ShaderT m) = ShaderT $ do
  let (_, acc) = runWriter m
  tell mempty {aIns = aIns acc, aOuts = aOuts acc, aUnis = aUnis acc}
  pure (aBodyT acc)

ifT :: TypedExpr 'BoolT -> ShaderT () -> ShaderT ()
ifT c t = do
  tb <- withBlock t
  appendBody (TIf c tb)

discardT :: ShaderT ()
discardT = appendBody TDiscard

untypeS :: TStmt -> Statement
untypeS s = case s of
  TDeclF n mi -> Decl FloatT n (fmap untypeE mi)
  TDeclV2 n mi -> Decl V2 n (fmap untypeE mi)
  TDeclV3 n mi -> Decl V3 n (fmap untypeE mi)
  TDeclV4 n mi -> Decl V4 n (fmap untypeE mi)
  TAssign (Name n) v -> Assign (Var n) (untypeE v)
  TAssignGL v -> Assign (Var "gl_Position") (untypeE v)
  TIf c t -> If (untypeE c) (map untypeS t)
  TDiscard -> Discard

untypeE :: TypedExpr t -> Expr
untypeE e = case e of
  TypedVar (Name s) -> Var s
  TypedLitF x' -> LitFloat x'
  TypedLitI x' -> LitInt x'
  TypedVec2 xs -> Constructor V2 xs
  TypedVec3 xs -> Constructor V3 xs
  TypedVec4 xs -> Constructor V4 xs
  TypedSwizzleX v' -> Field (untypeE v') "x"
  TypedSwizzleY v' -> Field (untypeE v') "y"
  TypedSwizzleZ v' -> Field (untypeE v') "z"
  TypedSwizzleA v' -> Field (untypeE v') "a"
  TypedSwizzleXY v' -> Field (untypeE v') "xy"
  TypedSwizzleXYZ v' -> Field (untypeE v') "xyz"
  TypedSwizzleRGB v' -> Field (untypeE v') "rgb"
  TypedAdd a' b' -> BinOp Add (untypeE a') (untypeE b')
  TypedSub a' b' -> BinOp Sub (untypeE a') (untypeE b')
  TypedMult a' b' -> BinOp Mul (untypeE a') (untypeE b')
  TypedDiv a' b' -> BinOp Div (untypeE a') (untypeE b')
  TypedClamp x' lo hi -> Call "clamp" [untypeE x', untypeE lo, untypeE hi]
  TypedMixV3 a' b' t' -> Call "mix" [untypeE a', untypeE b', untypeE t']
  TypedLength v' -> Call "length" [untypeE v']
  TypedFloor v' -> Call "floor" [untypeE v']
  TypedMod a' b' -> Call "mod" [untypeE a', untypeE b']
  TypedEq a' b' -> BinOp Eq (untypeE a') (untypeE b')
  TypedLt a' b' -> BinOp Lt (untypeE a') (untypeE b')
  TypedLe a' b' -> BinOp Le (untypeE a') (untypeE b')
  TypedTexture2d (Name s) uv -> Call "texture" [Var s, untypeE uv]
  TypedTexture2dArray (Name s) uv -> Call "texture" [Var s, untypeE uv]
