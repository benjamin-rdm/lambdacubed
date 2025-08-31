module Rendering.Shader.AST where

import Data.Foldable
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE

data Datatype
  = V2
  | V3
  | V4
  | Mat4
  | FloatT
  | IntT
  | BoolT
  | Sampler2D
  | Sampler2DArray
  | Void
  deriving (Eq, Ord, Show)

data ShaderType = FragmentShader | VertexShader deriving (Eq, Ord, Show)

version :: String
version = "330 core"

data ShaderSource = ShaderSource
  { ssType :: ShaderType,
    ssIns :: [(Datatype, String)],
    ssOuts :: [(Datatype, String)],
    ssUniforms :: [(Datatype, String)],
    ssFunctions :: NE.NonEmpty Function
    -- making this []able should be fine, use sides do not need head, but
    -- there is probably no sound reason to have this empty
  }
  deriving (Eq, Ord, Show)

data Function = Function
  { sfType :: ShaderType,
    sfName :: String,
    sfArgs :: [(Datatype, String)],
    sfBody :: [Statement]
  }
  deriving (Eq, Ord, Show)

data Statement
  = Decl Datatype String (Maybe Expr)
  | Assign Expr Expr
  | If Expr [Statement]
  | Discard
  | StmtExpr Expr
  deriving (Eq, Ord, Show)

data UnOp = Not deriving (Eq, Ord, Show)

data BinOp = Add | Sub | Mul | Div | Eq | Lt | Le | Gt | Ge deriving (Eq, Ord, Show)

data Expr
  = Var String
  | LitFloat Double
  | LitInt Int
  | Field Expr String
  | Constructor Datatype [Expr]
  | Call String [Expr]
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr
  deriving (Eq, Ord, Show)

class Ppr a where
  ppr :: a -> String

instance Ppr Datatype where
  ppr :: Datatype -> String
  ppr dt = case dt of
    V2 -> "vec2"
    V3 -> "vec3"
    V4 -> "vec4"
    Mat4 -> "mat4"
    FloatT -> "float"
    IntT -> "int"
    BoolT -> "bool"
    Sampler2D -> "sampler2D"
    Sampler2DArray -> "sampler2DArray"
    Void -> "void"

commaSep :: (Foldable t, Functor t, Ppr a) => t a -> String
commaSep xs = intercalate ", " (toList (fmap ppr xs))

-- This is not really robust to operator precedence yet
-- Current workflow:
-- 1: Wait for something to break
-- 2: Realize it is operator precendence
-- 3: Fix special case
-- 4: Repeat
instance Ppr Expr where
  ppr :: Expr -> String
  ppr e = case e of
    Var a -> a
    LitFloat a -> show a
    LitInt a -> show a
    Field ex f -> "(" ++ ppr ex ++ ")." ++ f
    Constructor dt es ->
      ppr dt ++ "(" ++ commaSep es ++ ")"
    Call n es ->
      n ++ "(" ++ commaSep es ++ ")"
    BinOp Add lhs rhs ->
      ppr lhs ++ " + " ++ ppr rhs
    BinOp Sub lhs rhs ->
      ppr lhs ++ " - " ++ ppr rhs
    BinOp Mul lhs rhs ->
      ppr lhs ++ " * " ++ ppr rhs
    BinOp Div lhs rhs ->
      "(" ++ ppr lhs ++ ") / (" ++ ppr rhs ++ ")"
    BinOp Eq lhs rhs ->
      ppr lhs ++ " == " ++ ppr rhs
    BinOp Lt lhs rhs ->
      ppr lhs ++ " < " ++ ppr rhs
    BinOp Le lhs rhs ->
      ppr lhs ++ " <= " ++ ppr rhs
    BinOp Gt lhs rhs ->
      ppr lhs ++ " > " ++ ppr rhs
    BinOp Ge lhs rhs ->
      ppr lhs ++ " >= " ++ ppr rhs
    UnOp Not op -> "!(" ++ ppr op ++ ")"

instance Ppr Statement where
  ppr :: Statement -> String
  ppr s = case s of
    Decl dt name mInit ->
      ppr dt ++ " " ++ name ++ maybe ";" (\e -> " = " ++ ppr e ++ ";") mInit
    Assign lhs rhs -> ppr lhs ++ " = " ++ ppr rhs ++ ";"
    If cond th ->
      "if (" ++ ppr cond ++ ") {\n" ++ indent (block th) ++ "\n}"
    Discard -> "discard;"
    StmtExpr e -> ppr e ++ ";"
    where
      block :: [Statement] -> String
      block = intercalate "\n" . fmap ppr
      indent :: String -> String
      indent = unlines . fmap ("  " ++) . lines

instance Ppr Function where
  ppr :: Function -> String
  ppr (Function _ name args body) =
    "void "
      ++ name
      ++ "("
      ++ intercalate ", " (fmap (\(dt, n) -> ppr dt ++ " " ++ n) args)
      ++ ") {\n"
      ++ indent (intercalate "\n" (fmap ppr body))
      ++ "\n}"
    where
      indent :: String -> String
      indent = unlines . fmap ("  " ++) . lines

instance Ppr ShaderSource where
  ppr :: ShaderSource -> String
  ppr (ShaderSource _ ins outs uniforms funs) =
    unlines $
      ["#version " ++ version]
        ++ fmap (\(dt, n) -> "in  " ++ ppr dt ++ " " ++ n ++ ";") ins
        ++ fmap (\(dt, n) -> "out " ++ ppr dt ++ " " ++ n ++ ";") outs
        ++ fmap (\(dt, n) -> "uniform " ++ ppr dt ++ " " ++ n ++ ";") uniforms
        ++ [""]
        ++ intercalate [""] (fmap (lines . ppr) (toList funs))
