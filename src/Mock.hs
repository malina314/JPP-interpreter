module Mock where
import System.IO

-- type Program = [String]

run :: FilePath -> IO ()
-- run = readFile >=> putStrLn . unlines . map (++ " OK") . lines
run _ = hPutStrLn stdout "OK"
