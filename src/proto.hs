{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Proto where

import Clash.Annotations.BitRepresentation
import Clash.Annotations.Primitive (HDL (..), Primitive (..))
import Clash.Backend (Backend)
import Clash.Explicit.Prelude as C
-- \| floating point addition, assume pipeline depth of 2

-- import Clash.Driver.Bool (OverridingBool(Always))

import Clash.Explicit.Testbench
import Clash.Netlist.BlackBox.Types
  ( BlackBoxFunction,
    BlackBoxMeta (..),
    TemplateKind (..),
    emptyBlackBoxMeta,
  )
-- import qualified Clash.Netlist.Types as DSL

import Clash.Netlist.BlackBox.Util (bbResult)
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
  ( BlackBox (..),
    BlackBoxContext,
    EntityOrComponent (..),
    HWType,
    TemplateFunction (..),
    bbResults,
  )
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL
import Clash.Primitives.Types (Primitive (BlackBoxHaskell, workInfo))
import Control.Monad.State (State, liftIO)
import qualified Data.List as L
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Number.MPFR as M
import Data.Proxy
import Data.String.Interpolate (__i)
import Data.Text
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc.Extra (Doc)
import Debug.Trace (trace, traceShow)
import FPFloat
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax (Name)
import System.IO.Unsafe
import Text.Show.Pretty (ppShow)

type N = 1

xp = SNat :: SNat N

plusFloatBV ::
  forall exponentBits mantissaBits rndMode n.
  (KnownNat exponentBits, KnownNat mantissaBits) =>
  Proxy (FoFloat exponentBits mantissaBits rndMode) ->
  Clock System ->
  DSignal System n (BitVector (BitSize (FoFloat exponentBits mantissaBits rndMode))) ->
  DSignal System n (BitVector (BitSize (FoFloat exponentBits mantissaBits rndMode))) ->
  DSignal System (n + N) (BitVector (BitSize (FoFloat exponentBits mantissaBits rndMode)))
plusFloatBV _ clk a b = delayN xp undefined enableGen clk (a + b)

plusFloat ::
  forall n.
  Clock System ->
  DSignal System n (FoFloat 4 11 M.Near) ->
  DSignal System n (FoFloat 4 11 M.Near) ->
  DSignal System (n + N) (FoFloat 4 11 M.Near)
plusFloat clk a b = C.unpack <$> plusFloatBV (Proxy @(FoFloat 4 11 M.Near)) clk (C.pack <$> a) (C.pack <$> b)

{-# OPAQUE plusFloatBV #-}
{-# ANN
  plusFloatBV
  ( let primName = show 'plusFloatBV
        tfName = show 'plusFloatBBF
     in InlineYamlPrimitive
          [minBound ..]
          [__i|
        BlackBoxHaskell:
          name: #{primName}
          templateFunction: #{tfName}
          workInfo: Always
      |]
  )
  #-}

plusFloatBBF :: BlackBoxFunction
plusFloatBBF _isD _primName _args _resTys = do
  -- Call Flopoco
  -- do something here to call flopo and get
  --

  -- let entityName = parseEntityName output
  let entityName = "plusFloat"

  let meta = emptyBlackBoxMeta {bbKind = TDecl}
      bb = BBFunction (show 'plusFloatTF) 0 (plusFloatTF entityName)
  -- Debugging output
  -- trace (show meta) $ return ()
  -- trace (show bb) $ return ()
  pure (Right (meta, bb))

plusFloatTF ::
  (HasCallStack) =>
  Text ->
  TemplateFunction
plusFloatTF entityName =
  TemplateFunction
    [0, 2, 3]
    (const True)
    (plusFloatBBTF entityName)

plusFloatBBTF ::
  forall s.
  (Backend s) =>
  Text ->
  BlackBoxContext ->
  State s Doc
plusFloatBBTF entityName bbCtx
  | [ clk,
      a,
      b
      ] <-
      L.map fst (DSL.tInputs bbCtx),
    [result] <- DSL.tResults bbCtx =
      do
        plusFloatInstName <- Id.makeBasic "plusFloat_inst"

        let compInps =
              [ ("clk", N.Bit),
                ("X", DSL.ety a),
                ("Y", DSL.ety b)
              ]
            compOuts =
              [("R", DSL.ety result)]

        DSL.declaration "plusFloat_inst_block" $ do
          DSL.compInBlock entityName compInps compOuts

          let inps =
                [ ("clk", clk),
                  ("X", a),
                  ("Y", b)
                ]

              outs =
                [ ("R", result)
                ]

          DSL.instDecl
            Empty
            (Id.unsafeMake entityName)
            plusFloatInstName
            []
            inps
            outs
  | otherwise = error $ ppShow bbCtx

topEntity ::
  Clock System ->
  DSignal System 0 (FoFloat 4 11 M.Near) ->
  DSignal System 0 (FoFloat 4 11 M.Near) ->
  DSignal System (0 + N) (FoFloat 4 11 M.Near)
topEntity clk x y =
  plusFloat clk x y
{-# OPAQUE topEntity #-}
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "topEntity",
        t_inputs =
          [ PortName "clk",
            PortName "x",
            PortName "y"
          ],
        t_output = PortName "result"
      }
  )
  #-}

-- Lift instance causess error unknown symbol "mpfr_add" during cabal build but not cabal repl
ta = $(lift (1.2 :: FoFloat 4 11 M.Near))

{-
testBench = fmap C.pack $ toSignal $ topEntity clk  (fromSignal a) (fromSignal b)
     where
         a = stimuliGenerator clk rst $(lift (1.2 :> 14 :> 23 :> (-4.3) :> (-5.6) :> Nil :: Vec 5 (FoFloat 4 11 M.Near)))
         b = stimuliGenerator clk rst $(lift (1.2 :> 1.4 :> 2.3 :> (-4.3) :> (-5.6) :> Nil :: Vec 5 (FoFloat 4 11 M.Near)))
         clk = systemClockGen
         rst = systemResetGen
-}
