--
-- Parser AST.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.ParseAST where

--------------------------------------------------------------------------------

type FnSym     = String
type Var       = String
type RefVar    = String
type IxVar     = String
type TypeVar   = String
type FieldNm   = String

--------------------------------------------------------------------------------

-- Top level symbols.
data GlobalSym = GlobalProc ProcDef
               | GlobalStruct StructDef
  deriving Show

--------------------------------------------------------------------------------
-- Procs

data ProcDef = ProcDef
  { procTy      :: Type         -- ^ Return type
  , procSym     :: FnSym        -- ^ Function name
  , procArgs    :: [(Type,Var)] -- ^ Argument types
  , procStmt    :: [Stmt]       -- ^ Body
  , procPrePost :: [PrePost]
  } deriving (Show, Read, Eq, Ord)

-- Pre and post conditions
data PrePost = PreCond  Exp
             | PostCond Exp
  deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Types

data Type
  = TyVoid                     -- ^ Unit type
  | TyInt IntSize              -- ^ Signed ints
  | TyWord WordSize            -- ^ Unsigned ints
  | TyBool                     -- ^ Booleans
  | TyChar                     -- ^ Characters
  | TyFloat                    -- ^ Floats
  | TyDouble                   -- ^ Doubles
  | TyRef      Scope Area      -- ^ References
  | TyConstRef Scope Area      -- ^ Constant References
  -- XXX
  -- | TyPtr Type              -- ^ Pointers
  | TyIx Integer               -- ^ Index type
  | TyArea Area                -- ^ Area types
  | TySynonym String           -- ^ Type synonym
  deriving (Show, Read, Eq, Ord)

data Area =
    TyStruct String            -- ^ Structures
  | TyArray Area Integer       -- ^ Arrays of fixed length
  -- XXX
  --  | TyCArray Area          -- ^ C Arrays
  | TyStored Type
  | AreaSynonym String         -- ^ Synonym
  deriving (Show, Read, Eq, Ord)

data Scope =
    Stack (Maybe TypeVar)
    -- ^ Stack allocated.  If no type variable is provided, a fresh one is
    -- constructed.
  | Global
  -- ^ Globally allocated
  | PolyMem (Maybe TypeVar)
  -- ^ Either allocation.  If no type variable is provided, a fresh one is
  -- constructed.
  deriving (Show, Read, Eq, Ord)

data IntSize
  = Int8
  | Int16
  | Int32
  | Int64
  deriving (Show, Read, Eq, Ord)

data WordSize
  = Word8
  | Word16
  | Word32
  | Word64
  deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Expressions

data Literal
  = LitInteger Integer
  deriving (Show, Read, Eq, Ord)

data Exp
  = ExpLit Literal
  | ExpVar Var
  | ExpRet -- Used only in post-conditions
  | ExpDeref Exp -- Note: these are statements in Ivory.
  | ExpOp ExpOp [Exp]
  | ExpArrIxRef RefVar Exp
  | ExpFieldRef RefVar FieldNm
  | ExpAnti String
    -- ^ Ivory antiquotation
  deriving (Show, Read, Eq, Ord)

data ExpOp
  = EqOp
  | NeqOp
  | CondOp

  | GtOp Bool
  -- ^ True is >=, False is >
  | LtOp Bool
  -- ^ True is <=, False is <

  | NotOp
  | AndOp
  | OrOp

  | MulOp
  | AddOp
  | SubOp
  | NegateOp
  | AbsOp
  | SignumOp

  | DivOp
  | ModOp

  | FExpOp
  | FSqrtOp
  | FLogOp
  | FPowOp
  | FSinOp
  | FTanOp
  | FCosOp
  | FAsinOp
  | FAtanOp
  | FAcosOp
  | FSinhOp
  | FTanhOp
  | FCoshOp
  | FAsinhOp
  | FAtanhOp
  | FAcoshOp

  | IsNanOp
  | IsInfOp
  | RoundFOp
  | CeilFOp
  | FloorFOp

  | BitAndOp
  | BitOrOp
  | BitXorOp
  | BitComplementOp
  | BitShiftLOp
  | BitShiftROp

  | ConstRefOp

  deriving (Show, Read, Eq, Ord)

data AllocRef
  = AllocBase RefVar Exp
  | AllocArr  RefVar [Exp]
  deriving (Show, Read, Eq, Ord)

-- | AST for parsing C-like statements.
data Stmt
  = IfTE Exp [Stmt] [Stmt]
  | Assert Exp
  | Assume Exp
  | Return Exp
  | ReturnVoid
  -- Deref dereferencing is an expression in our language here.
  | Store RefLVal Exp
  | Assign Var Exp
  | Call (Maybe Var) FnSym [Exp]
  | RefCopy Exp Exp
-- Local is AllocRef
  | AllocRef AllocRef
  | Loop IxVar [Stmt]
  | Forever [Stmt]
-- Break XXX Too dangerous (and difficult) for non-macro use?
  deriving (Show, Read, Eq, Ord)

data RefLVal
  = RefVar RefVar
  | ArrIx RefVar Exp
  deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Structs

data StructDef
  = StructDef String [Field]
  | AbstractDef String FilePath
  | StringDef String Integer
    deriving (Show)

structSym :: StructDef -> String
structSym s = case s of
  StructDef   sym _ -> sym
  AbstractDef sym _ -> sym
  StringDef   sym _ -> ivoryStringStructName sym

ivoryStringStructName :: String -> String
ivoryStringStructName = ("ivory_string_" ++)

data Field = Field
  { fieldName :: FieldNm
  , fieldType :: Area
  } deriving (Show)