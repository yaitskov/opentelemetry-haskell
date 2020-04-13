{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.EventlogStreaming_Internal where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Data.Word
import GHC.RTS.Events
import GHC.RTS.Events.Incremental
import GHC.Stack
import OpenTelemetry.Common hiding (Event, Timestamp)
import qualified OpenTelemetry.Common as OTel
import OpenTelemetry.Exporter
import OpenTelemetry.SpanContext
import System.IO
import qualified System.Random.SplitMix as R
import Text.Printf

work :: Timestamp -> Exporter Span -> Handle -> IO ()
work origin_timestamp exporter input = do
  putStrLn "Starting the eventlog reader"
  smgen <- R.initSMGen -- TODO(divanov): seed the random generator with something more random than current time
  go (initialState origin_timestamp smgen) decodeEventLog
  putStrLn "no more work"
  where
    go s (Produce event next) = do
      case evSpec event of
        Shutdown {} -> do
          putStrLn "Shutdown-like event detected"
        CapDelete {} -> do
          putStrLn "Shutdown-like event detected"
        CapsetDelete {} -> do
          putStrLn "Shutdown-like event detected"
        _ -> do
          -- putStrLn "go Produce"
          print (evTime event, evCap event, evSpec event)
          let (s', sps) = processEvent event s
          _ <- export exporter sps
          print s'
          mapM_ (putStrLn . ("emit " <>) . show) sps
          go s' next
    go s d@(Consume consume) = do
      -- putStrLn "go Consume"
      eof <- hIsEOF input
      case eof of
        False -> do
          chunk <- B.hGetSome input 4096
          -- printf "chunk = %d bytes\n" (B.length chunk)
          if B.null chunk
            then do
              -- putStrLn "chunk is null"
              threadDelay 1000 -- TODO(divanov): remove the sleep by replacing the hGetSome with something that blocks until data is available
              go s d
            else do
              -- putStrLn "chunk is not null"
              go s $ consume chunk
        True -> do
          putStrLn "EOF"
    go _ (Done _) = do
      putStrLn "go Done"
      pure ()
    go _ (Error _leftover err) = do
      putStrLn "go Error"
      putStrLn err

data State = S
  { originTimestamp :: Timestamp,
    threadMap :: IM.IntMap ThreadId,
    spanStacks :: HM.HashMap ThreadId (NonEmpty Span),
    traceMap :: HM.HashMap ThreadId TraceId,
    randomGen :: R.SMGen
  }
  deriving (Show)

initialState :: Word64 -> R.SMGen -> State
initialState timestamp = S timestamp mempty mempty mempty

processEvent :: Event -> State -> (State, [Span])
processEvent (Event ts ev m_cap) st@(S {..}) =
  let now = originTimestamp + ts
      m_thread_id = m_cap >>= flip IM.lookup threadMap
      m_trace_id = m_thread_id >>= flip HM.lookup traceMap
   in case (ev, m_cap, m_thread_id) of
        (WallClockTime {sec, nsec}, _, _) -> (st {originTimestamp = sec * 1_000_000_000 + fromIntegral nsec - ts}, [])
        (CreateThread new_tid, _, _) ->
          case m_trace_id of
            Just trace_id -> (st {traceMap = HM.insert new_tid trace_id traceMap}, [])
            _ -> (st, [])
        (RunThread tid, Just cap, _) ->
          (st {threadMap = IM.insert cap tid threadMap}, [])
        (StopThread _ tstatus, Just cap, _)
          | isTerminalThreadStatus tstatus -> (st {threadMap = IM.delete cap threadMap}, [])
        (StartGC, _, _) -> (pushGCSpans st now, [])
        (GCStatsGHC {gen}, _, _) -> (modifyAllSpans (setTag "gen" gen) st, [])
        (EndGC, _, _) -> popSpansAcrossAllThreads now st
        (HeapAllocated {allocBytes}, _, Just tid) ->
          (modifySpan tid (addEvent now "heap_alloc_bytes" (showT allocBytes)) st, [])
        (UserMessage {msg}, _, Just tid) -> case T.words msg of
          ("ot1" : "begin" : "span" : name) ->
            (pushSpan tid (T.intercalate " " name) now st, [])
          ("ot1" : "end" : "span" : _) -> popSpan tid now st
          ["ot1", "set", "tag", k, v] -> (modifySpan tid (setTag k v) st, [])
          ["ot1", "set", "traceid", trace_id] ->
            (modifySpan tid (setTraceId (TId (read ("0x" <> T.unpack trace_id)))) st, [])
          ["ot1", "set", "spanid", span_id] ->
            (modifySpan tid (setSpanId (SId (read ("0x" <> T.unpack span_id)))) st, [])
          ["ot1", "set", "parent", trace_id, sid] ->
            (modifySpan tid (setParent (TId (read ("0x" <> T.unpack trace_id))) (SId $ read ("0x" <> T.unpack sid))) st, [])
          ["ot1", "add", "event", k, v] -> (modifySpan tid (addEvent now k v) st, [])
          ("ot1" : rest) -> error $ printf "Unrecognized %s" (show rest)
          _ -> (st, [])
        _ -> (st, [])

setTag :: ToTagValue v => T.Text -> v -> Span -> Span
setTag k v sp =
  sp
    { spanTags = HM.insert k (toTagValue v) (spanTags sp)
    }

setSpanId :: SpanId -> Span -> Span
setSpanId sid sp =
  sp
    { spanContext = SpanContext sid (spanTraceId sp)
    }

setTraceId :: TraceId -> Span -> Span
setTraceId tid sp =
  sp
    { spanContext = SpanContext (spanId sp) tid
    }

setParent :: TraceId -> SpanId -> Span -> Span
setParent ptid psid sp =
  sp
    { spanParentId = Just psid,
      spanContext = SpanContext (spanId sp) ptid
    }

addEvent :: Timestamp -> T.Text -> T.Text -> Span -> Span
addEvent ts k v sp = sp {spanEvents = new_events}
  where
    new_events = ev : spanEvents sp
    ev = SpanEvent ts k v

modifyAllSpans :: (Span -> Span) -> State -> State
modifyAllSpans f st =
  st
    { spanStacks =
        fmap
          (\(sp :| sps) -> (f sp :| sps))
          (spanStacks st)
    }

modifySpan :: HasCallStack => ThreadId -> (Span -> Span) -> State -> State
modifySpan tid f st =
  st
    { spanStacks =
        HM.update (\(sp :| sps) -> Just (f sp :| sps)) tid (spanStacks st)
    }

pushSpan :: HasCallStack => ThreadId -> T.Text -> OTel.Timestamp -> State -> State
pushSpan tid name timestamp st = st {spanStacks = new_stacks, randomGen = new_randomGen, traceMap = new_traceMap}
  where
    maybe_parent = NE.head <$> HM.lookup tid (spanStacks st)
    new_stacks = HM.alter f tid (spanStacks st)
    f Nothing = Just $ sp :| []
    f (Just sps) = Just $ cons sp sps
    (sid, new_randomGen) = R.nextWord64 (randomGen st)
    (new_traceMap, trace_id) = case (maybe_parent, HM.lookup tid (traceMap st)) of
      (Just parent, _) -> (traceMap st, spanTraceId parent)
      (_, Just trace_id') -> (traceMap st, trace_id')
      _ -> let new_trace_id = TId sid in (HM.insert tid new_trace_id (traceMap st), new_trace_id)
    sp =
      Span
        { spanContext = SpanContext (SId sid) trace_id,
          spanOperation = name,
          spanStartedAt = timestamp,
          spanFinishedAt = 0,
          spanTags = HM.singleton "tid" (IntTagValue $ fromIntegral tid),
          spanEvents = mempty,
          spanStatus = OK,
          spanParentId = spanId <$> maybe_parent
        }

popSpan :: HasCallStack => ThreadId -> OTel.Timestamp -> State -> (State, [Span])
popSpan tid timestamp st = (st {spanStacks = new_stacks, traceMap = new_traceMap}, [sp {spanFinishedAt = timestamp}])
  where
    sp :| new_stack = fromMaybe (error $ printf "popSpan: missing span stack for thread %d" tid) $ HM.lookup tid (spanStacks st)
    (new_traceMap, new_stacks) = case new_stack of
      [] -> (HM.delete tid (traceMap st), HM.delete tid (spanStacks st))
      x : xs -> (traceMap st, HM.insert tid (x :| xs) (spanStacks st))

pushGCSpans :: HasCallStack => State -> OTel.Timestamp -> State
pushGCSpans st timestamp = foldr go st tids
  where
    tids = HM.keys (spanStacks st)
    go tid = pushSpan tid "gc" timestamp

popSpansAcrossAllThreads :: HasCallStack => OTel.Timestamp -> State -> (State, [Span])
popSpansAcrossAllThreads timestamp st = foldr go (st, []) tids
  where
    tids = HM.keys (spanStacks st)
    go tid (st', sps) =
      let (st'', sps') = popSpan tid timestamp st'
       in (st'', sps' <> sps)

isTerminalThreadStatus :: ThreadStopStatus -> Bool
isTerminalThreadStatus HeapOverflow = True
isTerminalThreadStatus StackOverflow = True
isTerminalThreadStatus ThreadFinished = True
isTerminalThreadStatus _ = False

showT :: Show a => a -> T.Text
showT = T.pack . show
