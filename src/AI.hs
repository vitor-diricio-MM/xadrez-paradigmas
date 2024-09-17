{-# LANGUAGE ScopedTypeVariables #-}

-- AI.hs-- AI.hs

module AI (getBestMove) where

import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Char (toLower)
import Data.List (intersperse, isInfixOf)
import System.IO
  ( Handle,
    hClose,
    hFlush,
    hGetLine,
    hIsEOF,
    hPutStrLn,
  )
import System.Process
  ( CreateProcess (std_err, std_in, std_out),
    StdStream (CreatePipe),
    createProcess,
    proc,
    terminateProcess,
    waitForProcess,
  )
import Tabuleiro (Tabuleiro)

-- Function to get the best move from StockFish given the current board
getBestMove :: Tabuleiro -> IO (Maybe String)
getBestMove tab = do
  let fen = tabuleiroToFEN tab
  -- Start StockFish process
  (Just hin, Just hout, Just herr, ph) <-
    createProcess
      (proc "/opt/homebrew/bin/stockfish" [])
        { std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe
        }
  -- Make sure to close handles on exception
  let cleanup = do
        hClose hin
        hClose hout
        hClose herr
        terminateProcess ph
        waitForProcess ph
  -- Use catch to handle any exceptions and ensure cleanup
  catch
    ( do
        -- Initialize UCI
        hPutStrLn hin "uci"
        hFlush hin
        waitForResponse hout "uciok"

        -- Tell StockFish we are ready
        hPutStrLn hin "isready"
        hFlush hin
        waitForResponse hout "readyok"

        -- Set the position to the current FEN
        hPutStrLn hin $ "position fen " ++ fen
        hFlush hin

        -- Start the search for best move with a reasonable depth
        hPutStrLn hin "go depth 15"
        hFlush hin

        -- Wait for the best move
        bestMoveLine <- getBestMoveLine hout

        -- Close the input to let StockFish know we are done
        hClose hin

        -- Parse the best move
        let bestMove = parseBestMove bestMoveLine

        -- Cleanup
        cleanup

        return bestMove
    )
    ( \(e :: SomeException) -> do
        -- In case of any exception, perform cleanup and return Nothing
        cleanup
        return Nothing
    )

-- Function to wait for a specific response line (case-insensitive)
waitForResponse :: Handle -> String -> IO ()
waitForResponse hout expected = do
  end <- hIsEOF hout
  if end
    then return ()
    else do
      line <- hGetLine hout
      let lineLower = map toLower line
          expectedLower = map toLower expected
      when (expectedLower `notElem` [take (length expectedLower) lineLower]) $
        waitForResponse hout expected

-- Function to continuously read lines until "bestmove" is found (case-insensitive)
getBestMoveLine :: Handle -> IO String
getBestMoveLine hout = do
  eof <- hIsEOF hout
  if eof
    then return ""
    else do
      line <- hGetLine hout
      if "bestmove" `isInfixOf` (map toLower line)
        then return line
        else getBestMoveLine hout

-- Function to parse the best move from the "bestmove" line
parseBestMove :: String -> Maybe String
parseBestMove line =
  case words line of
    ["bestmove", mv] -> Just mv
    _ -> Nothing

-- Function to convert the current board to FEN notation
tabuleiroToFEN :: Tabuleiro -> String
tabuleiroToFEN tab =
  let rows = map rowToFEN tab
      piecesFEN = concat $ intersperse "/" rows
      -- Assuming it's Black's turn; adjust if necessary
      activeColor = "b" -- AI is always playing Black
      castling = "-" -- Castling not implemented
      enPassant = "-" -- En passant not implemented
      halfMove = "0" -- Not implemented
      fullMove = "1" -- Not implemented
   in unwords [piecesFEN, activeColor, castling, enPassant, halfMove, fullMove]

-- Helper function to convert a single row to FEN
rowToFEN :: String -> String
rowToFEN [] = ""
rowToFEN (c : cs) =
  let (empty, rest) = span (== ' ') (c : cs)
   in if not (null empty)
        then show (length empty) ++ rowToFEN rest
        else c : rowToFEN cs