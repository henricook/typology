module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)

import Data.Char as CH
import Data.Const (Const(..))
import Data.Functor.Coproduct (Coproduct, right)
import Data.Maybe (Maybe(..))
import Data.String as ST

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.Query.EventSource as ES
import Halogen.Util (runHalogenAff, awaitBody)

import Keyboard as K

type State = {
  text   :: String,
  typed  :: String,
  errors :: Int
}

initialState :: String -> State
initialState s = { text : s, typed : "", errors : 0 }

data Query a = Init a | Input Char a

-- | Effects embedding the Ace editor requires.
type E eff = (dom :: DOM, avar :: AVAR, keyboard :: K.KEYBOARD | eff)

ui :: forall eff. H.Component State Query (Aff (E eff))
ui = H.lifecycleComponent { render, eval, initializer, finalizer: Nothing }
  where

  initializer :: Maybe (Query Unit)
  initializer = Just (H.action Init)

  render :: State -> H.ComponentHTML Query
  render s =
    HH.div_
      [ HH.p_ [ HH.text s.text ]
      , HH.p_ [ HH.text s.typed ]
      ]

  eval :: Query ~> H.ComponentDSL State Query (Aff (E eff))
  eval (Init next) = do
    document <- H.fromEff $ DOM.window >>= DOM.document <#> DOM.htmlDocumentToDocument
    let
      querySource :: H.EventSource (Coproduct (Const Unit) Query) (Aff (E eff))
      querySource =
        H.eventSource (K.onKeyUp document) \e -> do
          K.preventDefault e
          let info = K.readKeyboardEvent e
          let c = CH.fromCharCode info.keyCode
          pure $ H.action (right <<< Input c)
              
    H.subscribe $ ES.catEventSource querySource
    pure next
  eval (Input c next) = do
    H.modify (\st -> st {typed = st.typed <> ST.singleton c})
    pure next

-- | Run the app
main :: Eff (H.HalogenEffects (keyboard :: K.KEYBOARD)) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui (initialState "Hello World!") body
