import System.IO
import Numeric (readDec, readHex)
import Data.Char (isDigit)
import System.Environment (getArgs)
import Data.Text
import Data.Text.IO


data Ihex = Ihex { addr :: Int, hexval :: String }


--
subString :: Int -> Int -> String -> String
subString n l str = Prelude.take l (Prelude.drop n str)

-- 16 進数に変換
toHex :: String -> Int
toHex str = fst $ Prelude.head $ readHex str

-- 10 進数に変換
toDec :: String -> Int
toDec str = fst $ Prelude.head $ readDec str

-- 文字列が 10 進数であることの判定
isDigitString :: String -> Bool
isDigitString str = Prelude.foldl (\b c -> b && isDigit c) True str


-- Intel Hex からデータ長さを取得
parseLength :: String -> Int
parseLength str = toHex $ subString 1 2 str

{-
 レコードタイプ
 00 データレコード
 01 エンドレコード
 04 拡張リニアアドレスレコード
 -}
parseType :: String -> String
parseType str = subString 7 2 str

-- データレコードのアドレス
parseAddr :: String -> Int
parseAddr str = toHex $ subString 3 4 str

-- ELA のアドレス
parseELAAddr :: String -> Int
parseELAAddr str = toHex $ subString 9 4 str


{-
    データレコードの値を分割
    第一引数 : Intel Hex のデータレコードの値
    第二引数 : 先頭アドレス
    第三引数 : データ長(ループカウンタの終了値)
    第四引数 : 前行までの分割結果
    第五引数 : インデックス(ループカウンタ)
    戻り値 : 現在の行までの S record の分割結果
 -}
splitDataRecord :: String -> Int -> Int -> [Ihex] -> Int -> [Ihex]
splitDataRecord line start len lst idx =
    if idx < len then splitDataRecord line start len (lst ++ [(Ihex (start+idx) (subString (idx*2) 2 line))]) (idx+1)
    else lst

-- record の行バーサー
parseLine :: String -> Int -> [Ihex]
parseLine str offset =
    splitDataRecord (subString 9 (l*2) str) (offset+m) l [] 0
    where
        l = parseLength str
        m = parseAddr str


-- Ihex の値が指定アドレス範囲内であれば出力
putIHex :: Int -> Int -> Ihex -> IO ()
putIHex b1 b2 s = 
    if (b1 <= pos) && (pos < b2)
        then System.IO.putStr $ hexval s
        else return ()
    where
        pos = addr s


-- 指定アドレス範囲内の値を出力
createAndDumpSrecs :: Handle -> Int -> Int -> [Ihex] -> IO ()
createAndDumpSrecs fp p_start p_end lsr = do
    eof <- hIsEOF fp
    if eof
        then mapM_ (putIHex p_start p_end) lsr
        else do
            text_line <- Data.Text.IO.hGetLine fp
            let line = Data.Text.unpack text_line
            let typ = parseType line
            if typ == "00" then createAndDumpSrecs fp p_start p_end (lsr ++ (parseLine line 0))
            else if typ == "04" then do
                text_line2 <- Data.Text.IO.hGetLine fp
                let line2 = Data.Text.unpack text_line2
                let offset = parseELAAddr line
                createAndDumpSrecs fp p_start p_end (lsr ++ (parseLine line2 (offset*65536)))
            else createAndDumpSrecs fp p_start p_end lsr

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


