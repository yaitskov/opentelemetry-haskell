module TestEventlogStreaming where

import qualified Data.ByteString.Char8 as BS
import Data.List (foldl', sort)
import qualified Data.Text as T
import Data.Word
import GHC.RTS.Events
import GHC.RTS.Events.Incremental
import OpenTelemetry.Common hiding (Event)
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.SpanContext
import Test.Tasty.HUnit
import Text.Printf

processEvents :: [Event] -> State -> (State, [Span])
processEvents events st0 = foldl' go (st0, []) events
  where
    go (st, sps) e =
      let (st', sps') = processEvent e st
       in (st', sps' <> sps)

prop_spans_are_not_lost :: [(Word64, Int)] -> Bool
prop_spans_are_not_lost spans =
  let input_events = concatMap convert spans
      convert (span_serial_number, thread_id) =
        [ Event 0 (UserMessage {msg = T.pack $ printf "ot2 begin span %d %d" span_serial_number thread_id}) (Just 0),
          Event 42 (UserMessage {msg = T.pack $ printf "ot2 end span %d" span_serial_number}) (Just 0)
        ]
      (end_state, emitted_spans) = processEvents input_events (initialState 0 (error "randomGen seed"))
   in length emitted_spans == length spans
