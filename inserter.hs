-- コマンドのフレーム数を増やしたり減らしたりする
{-# LANGUAGE OverloadedStrings #-} -- 言語拡張: Text型にリテラル値を使用
import System.IO 
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TI

command :: String
command = "200 ENEMY_LEFT 0 20 -50\n230 ENEMY_LEFT 0 60 -50"

unicodeText :: T.Text
unicodeText = "追加のテキスト"

-- テスト関数
hello :: T.Text -> T.Text
hello text = T.unwords [text, unicodeText]

-- ファイル読込
main :: IO ()
main = do
    myFile <- openFile "stage1.txt" ReadMode
    hSetEncoding myFile utf8
    cs <- TI.hGetContents myFile
    let commands = T.lines cs
    let noComments = filter (\x -> (T.head x) /= '#') commands
    TI.putStrLn (head noComments)
    hClose myFile

-- フレームの数値が指定値以上の要素を抜き出す
filterLines :: [String] -> Int -> [String]
filterLines cmd targetPoint = filter (\x -> (read (head (words x))::Int) >= targetPoint) cmd

-- フレームの数値に指定の値を足したコマンド文字列を返す
processLines :: String -> Int -> String
processLines cmd value = unlines (map func cmdLines)
    where func = insert value
          cmdLines = lines cmd

insert :: Int -> String -> String
insert value cmd = unwords newCmdAray
    where cmdArray = words cmd
          frame = head cmdArray
          newCmdAray = (addFrame frame value):(tail cmdArray)

addFrame :: String -> Int -> String
addFrame frame value = show (intFrame + value)
    where intFrame = read frame :: Int
