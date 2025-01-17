{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Example() where

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

import Clash.Netlist.Types
  (BlackBox (..), BlackBoxContext, EntityOrComponent(..), TemplateFunction(..))
import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL
import Control.Monad.State (State)
import qualified Data.List as L
import qualified Data.Number.MPFR as M
import Data.Proxy
import Data.String.Interpolate (__i)
import Data.Text
import Data.Text.Prettyprint.Doc.Extra (Doc)
import FPFloat
import GHC.Stack (HasCallStack)
import Text.Show.Pretty (ppShow)
import qualified Prelude as P
type N = 1

xp = SNat :: SNat N

plusFloat ::
  forall n.
  Clock System ->
  DSignal System n (FoFloat 4 11 M.Near) ->
  DSignal System n (FoFloat 4 11 M.Near) ->
  DSignal System (n + N) (FoFloat 4 11 M.Near)
plusFloat clk a b = delayN xp undefined enableGen clk (a + b)

{-# OPAQUE plusFloat #-}
-- This YamlPrimitive is similar to BlackBoxFuncion, TemplateFunction and BlackBoxTemplateFunction 
--
{-
{-# ANN plusFloat (InlineYamlPrimitive [VHDL][__i|
BlackBoxHaskell:
  name: Example.plusFloat
  kind: Declaration
  type: |-
    plusFloat ::
      forall n.
      Clock System ->
      DSignal System n (FoFloat 4 11 M.Near) ->
      DSignal System n (FoFloat 4 11 M.Near) ->
      DSignal System (n + N) (FoFloat 4 11 M.Near)
  template: |-
    ~GENSYM[plusFloat_inst_block][0] : block
      component ~GENSYM[plusFloat][1] port
        ( clk : in std_logic;
          X   : in std_logic_vector(~SIZE[~TYP[1]] - 1 downto 0);
          Y   : in std_logic_vector(~SIZE[~TYP[2]] - 1 downto 0);
          R   : out std_logic_vector(~SIZE[~TYP[3]] - 1 downto 0) );
      end component;
    begin
      ~GENSYM[plusFloat_inst][2] : ~SYM[1]
        port map
          ( clk => ~ARG[0],
            X   => ~TOBV[~ARG[1]][~TYP[1]],
            Y   => ~TOBV[~ARG[2]][~TYP[2]],
            R   => ~FROMBV[~RESULT][~TYP[3]] );
    end block;
|])#-}

-}
{-# ANN
  plusFloat
  ( let primName = show 'plusFloat
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
  let meta = emptyBlackBoxMeta {bbKind = TDecl}
      bb = BBFunction (show 'plusFloatTF) 0 (plusFloatTF "plusFloat")
  pure (Right (meta, bb))
plusFloatTF :: HasCallStack => Text -> TemplateFunction
plusFloatTF entityName
  = TemplateFunction [0, 1, 2, 3] (const True) (plusFloatBBTF entityName)
plusFloatBBTF ::
      forall s. Backend s => Text -> BlackBoxContext -> State s Doc
plusFloatBBTF entityName bbCtx
      | [clk, x, y] <- P.map fst (DSL.tInputs bbCtx),
        [r] <- DSL.tResults bbCtx
      = do plusFloatInstName <- Id.makeBasic "plusFloat_inst"
           let compInps
                 = [("clk", DSL.ety clk), ("X", DSL.ety x), ("Y", DSL.ety y)]
               compOuts = [("R", DSL.ety r)]
           (DSL.declaration "plusFloat_inst_block"
              $ (do DSL.compInBlock entityName compInps compOuts
                    let inps = [("clk", clk), ("X", x), ("Y", y)]
                        outs = [("R", r)]
                    DSL.instDecl
                      Empty (Id.unsafeMake entityName) plusFloatInstName [] inps outs))
      | otherwise = error (ppShow bbCtx)
    
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

-- This causes error when building
-- ta = $(lift (1.2 :: FoFloat 4 11 M.Near))
-- This testbench will cause build error. The temporary solution is to 
-- run clashi, load the Haskell file, and then run it. 
{-
testBench = toSignal $ topEntity clk (fromSignal a) (fromSignal b)
  where
    a = stimuliGenerator clk rst $(lift (1.2 :> 14 :> 23 :> (-4.3) :> (-5.6) :> Nil :: Vec 5 (FoFloat 4 11 M.Near)))
    b = stimuliGenerator clk rst $(lift (1.2 :> 1.4 :> 2.3 :> (-4.3) :> (-5.6) :> Nil :: Vec 5 (FoFloat 4 11 M.Near)))
    clk = systemClockGen
    rst = systemResetGen
-}

