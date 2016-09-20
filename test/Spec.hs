import WaterVolumeSolver (volume,merge,solve)
import System.Environment
import Control.DeepSeq

main :: IO ()
main = do
    putStrLn "\nBEGIN TEST"
    runTest "test/testdata/test_16.txt" 8
    runTest "test/testdata/test_32.txt" 16
    runTest "test/testdata/test_256.txt" 128
    runTest "test/testdata/test_512.txt" 256
    runTest "test/testdata/test_1024.txt" 512
    putStrLn "DONE"

runTest :: String -> Int -> IO ()
runTest path n = do
    s <- readFile path
    let list = read ('[' : s ++ "]") :: [Int]
    return $ force list
    let (l,r) = splitAt n list
    putStrLn $ show $ volume $ merge (solve l) (solve r)
    
