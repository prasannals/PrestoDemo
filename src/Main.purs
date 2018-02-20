module Main where

import Data.String
import Halogen.VDom.Types
import Prelude
import Types
import UI.Elements
import UI.Events
import UI.Properties

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (stack)
import Control.Plus ((<|>))
import DOM.HTML.Event.ErrorEvent (lineNo)
import DOM.HTML.History (back)
import Data.Array (group)
import FRP as F
import FRP.Behavior (behavior)
import FRP.Event as E
import Halogen.VDom.Types (graft)
import UI.Core (MEvent, AttrValue(..), Attr(..), Prop)
import UI.Util as U

foreign import click :: MEvent
foreign import change :: MEvent


widget :: forall a b. {background :: String |b} -> VDom Attr a
widget state = scrollView
                  [ id_ "100"
                  , height "match_parent"
                  , width "match_parent"
                  ]
                  [

              linearLayout
              [ id_ "1"
              , height "match_parent"
              , width "match_parent"
              , background "#76b852"
              , gravity "center"
              , orientation "vertical"
              ]
              [
                linearLayout
                (  [id_ "8"
                  , height "300"
                  , width "400"
                  , background "#ffffff"
                  , gravity "center"
                  , orientation "vertical"
                  ])

                  [
                    editText
                      [
                        id_ "userEdit"
                      , height "50"
                      , width "250"
                      , text ""
                      , padding "10,10,10,10"
                      , onChange (Some change)
                      , hint "username"
                      ],
                    editText
                      [
                        id_ "passEdit"
                      , height "50"
                      , width "250"
                      , margin "0,20,0,0"
                      , padding "40,0,0,0"
                      , text ""
                      , stroke "#fff,0"
                      , onChange (Some change)
                      , hint "password"
                      ],
                    linearLayout
                      [
                        id_ "button"
                      , height "40"
                      , width "250"
                      , background (state.background)
                      , margin "0,20,0,0"
                      , gravity "center"
                      , color "#ffffff"
                      , onClick (Some click)
                      ]
                      [
                       textView
                       [
                          id_ "7"
                        , height "20"
                        , width "match_parent"
                        , text "LOGIN"
                        , fontStyle "Source Sans Pro-Regular"
                        , gravity "center"
                       ]

                      ]
                   ]
                   ]
                  ]





main = do
  --- Init State {} empty record--
  U.initializeState

  --- Update State ----
  state <- U.updateState "background" "#C0C0C0"
  _ <- U.updateState "x" ""
  _ <- U.updateState "y" ""    ---  global  key value pair in your "state" (which is also global)

  ---- Render Widget ---
  U.render (widget state) listen   --

  pure unit   -- syntactical shi... stuff


-- CONTINUE  EVAL : x is the String in sig3, y is the String in sig5
eval x y = do
      let xBol = length x /= 0
      let yBol = length y /= 0   --- "/=" is not "!="
      let s = xBol && yBol

      _ <- U.updateState "x" x
      _ <- U.updateState "y" y

      if s
          then
           U.updateState "background" "#43A047"
        else
           U.updateState "background" "#C0C0C0" -- updateState returns your state and returns the new state
-- eval function should return a state. the state that you've used

-- updateState returns "Eff Rec" and "<-" takes out "Rec" from Eff.
-- type Rec t = {| t }
-- Rec is basically any record


listen = do
  sig3 <- U.signal "userEdit" ""    -- "3" is the ID. "" is the initial state of your behavior (Behavior String)
  sig5 <- U.signal "passEdit" ""
  sig6 <- U.signal "button" false   -- Behavior Boolean

  -- when we do U.signal, we are listening for an event on the button/edittext/whatever

  -- sig6, 3,5 contains .event and .behavior

  _ <- sig6.event `E.subscribe` (\_ -> do
                                        s <- U.getState
                                        log s.x
                                        log s.y
                                         )

  -- every time sig6.event occurs, the (\ -> do) thingi is called.


  let behavior = eval <$> sig3.behavior <*> sig5.behavior   -- this specifies WHAT TO DO WHEN EVENTS OCCUR
  -- since we are passing in sig3 and sig5 behavior which has Strings soooooo, GOTO EVAL
  let events = (sig3.event <|> sig5.event)   --- this specifies on WHICH EVENT THE PATCH SHOULD BE TRIGGERED

  U.patch widget behavior events

  -- when these "events" occur, do the following
  -- execute the function specified as "behavior"
  -- update the widget again
