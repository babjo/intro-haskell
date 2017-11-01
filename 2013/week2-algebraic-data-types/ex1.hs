data MessageType = Info
				 | Warning
				 | Error Int
				 deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
				| Unknown String
				deriving (Show, Eq)

parseMessage :: String -> LogMessage
parseMessage s = case words s of
      ("E":sev:t:msg) -> LogMessage (Error (read sev)) (read t) (unwords msg)
      ("W":t:msg) -> LogMessage Warning (read t) (unwords msg)
      ("I":t:msg) -> LogMessage Info (read t) (unwords msg)
      _ -> Unknown s
