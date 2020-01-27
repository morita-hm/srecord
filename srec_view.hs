import System.IO
import Numeric (readDec, readHex)
import Data.Char (isDigit)
import System.Environment (getArgs)
import Data.Text
import Data.Text.IO

data Srec = Srec { addr :: Int, size :: Int, hexval :: String }

-- 16 進数に変換
toHex :: String -> Int
toHex str = fst $ Prelude.head $ readHex str

-- 10 進数に変換
toDec :: String -> Int
toDec str = fst $ Prelude.head $ readDec str

-- 文字列が 10 進数であることの判定
isDigitString :: String -> Bool
isDigitString str = Prelude.foldl (\b c -> b && isDigit c) True str

-- S record からデータ長さを取得
parseLength :: String -> Int
parseLength str = toHex $ Prelude.take 2 $ Prelude.drop 2 str

--
parseAddr :: String -> Int
parseAddr str = toHex $ Prelude.take 8 $ Prelude.drop 4 str

--
trimString :: Int -> Int -> String -> String
trimString n l str = Prelude.take (2*l) (Prelude.drop (2*n) str)

{-
    S record の値を分割
    第一引数 : S record の行
    第二引数 : 先頭アドレス
    第三引数 : データ長(ループカウンタの終了値)
    第四引数 : 前行までの分割結果
    第五引数 : インデックス(ループカウンタ)
    戻り値 : 現在の行までの S record の分割結果
 -}
splitValues :: String -> Int -> Int -> [Srec] -> Int -> [Srec]
splitValues line start len lst idx =
    if idx < len then splitValues line start len (lst ++ [Srec {addr = start+idx, size = 1, hexval = trimString idx 1 line}]) (idx+1)
    else lst

-- S record の行バーサー
parseLine :: String -> [Srec]
parseLine str
    | header == "S3" = splitValues (trimString 6 (l-5) str) m (l-5) [] 0
    | header == "S2" = [] 
    | otherwise = []
    where
        m = parseAddr str
        l = parseLength str
        header = Prelude.take 2 str

-- Srec の値が指定アドレス範囲内であれば出力
putSrec :: Int -> Int -> Srec -> IO ()
putSrec b1 b2 s =
    if (b1 <= n) && (n < b2) then System.IO.putStr $ hexval s
    else return ()
    where n = addr s


-- 指定アドレス範囲内の値を出力
createAndDumpSrecs :: Handle -> Int -> Int -> [Srec] -> IO ()
createAndDumpSrecs fp p_start p_end lsr = do
    eof <- hIsEOF fp
    if eof
        then mapM_ (putSrec p_start p_end) lsr
        else do
            line <- Data.Text.IO.hGetLine fp
            createAndDumpSrecs fp p_start p_end (lsr ++ (parseLine (Data.Text.unpack line))) 

-- main
main :: IO()
main = do
    -- コマンドライン引数のリスト
    args <- getArgs
    if 3 == Prelude.length args
        then do
            fp <- openFile (args !! 0) ReadMode
            let begin_addr = toHex (args !! 1)
            let end_addr = begin_addr + toDec (args !! 2)
            print begin_addr
            print end_addr
            createAndDumpSrecs fp begin_addr end_addr []
            System.IO.putStrLn ""
        else System.IO.putStrLn "Argument is wrong"
 

