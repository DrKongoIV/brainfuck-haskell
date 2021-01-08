import Text.ParserCombinators.ReadP
import Data.Char (chr, ord)
import System.Environment (getArgs)
import System.IO (BufferMode(NoBuffering) , hSetBuffering, stdin)

data Instruction = GoLeft | GoRight | Inc | Dec | In | Out | Loop [Instruction]

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  file <- head <$> getArgs
  p <- fst . last . readP_to_S (parser <* eof) . filter (`elem` "<>+-,.[]") <$> readFile file
  exec (repeat 0, 0, repeat 0) p

parser :: ReadP [Instruction]
parser = many1 ((GoLeft <$ char '<')
            +++ (GoRight <$ char '>')
            +++ (Inc <$ char '+')
            +++ (Dec <$ char '-')
            +++ (In <$ char ',')
            +++ (Out <$ char '.')
            +++ (Loop <$> between (char '[') (char ']') parser))

exec :: ([Int], Int, [Int]) -> [Instruction] -> IO ()
exec   _                  []            = pure ()
exec   (l:ls, x, rs  )    (GoLeft :is)  = exec (ls, l, x:rs) is
exec   (ls,   x, r:rs)    (GoRight:is)  = exec (x:ls, r, rs) is
exec   (ls,   x, rs  )    (Inc    :is)  = exec (ls, succ x, rs) is
exec   (ls,   x, rs  )    (Dec    :is)  = exec (ls, pred x, rs) is
exec   (ls,   _, rs  )    (In     :is)  = getChar >>= (\c -> exec (ls, ord c, rs) is)
exec t@(_ ,   x, _   )    (Out    :is)  = putChar (chr x) >> exec t is
exec t@(_ ,   x, _   ) is@(Loop l :is') | x == 0    = exec t is'
                                        | otherwise = exec t (l ++ is)
exec   _                  _             = error "Something went wrong :("
