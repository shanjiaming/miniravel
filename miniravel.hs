import qualified Data.Map as Map
import Data.Char
import Data.Bits

type Reg = String
type Constant = Int
type Mem = (Int, Reg)
type BlockName = String
data Stmt = 
    Addi Reg Reg Constant | 
    Slti Reg Reg Constant | 
    Sgti Reg Reg Constant | 
    Sltiu Reg Reg Constant | 
    Xori Reg Reg Constant | 
    Ori Reg Reg Constant | 
    Andi Reg Reg Constant | 
    Slli Reg Reg Constant | 
    Srli Reg Reg Constant | 
    Srai Reg Reg Constant | 
    Add Reg Reg Reg | 
    Sub Reg Reg Reg | 
    Mul Reg Reg Reg | 
    Div Reg Reg Reg | 
    Sll Reg Reg Reg | 
    Slt Reg Reg Reg | 
    Sgt Reg Reg Reg | 
    Sltu Reg Reg Reg | 
    Xor Reg Reg Reg | 
    Srl Reg Reg Reg | 
    Sra Reg Reg Reg | 
    Or Reg Reg Reg | 
    And Reg Reg Reg | 
    Li Reg Constant |
    Mv Reg Reg | 
    Lw Reg Mem | 
    Sw Reg Mem | 
    J BlockName | 
    Beqz Reg BlockName | 
    Bnez Reg BlockName | 
    Ret 
    deriving (Show)
type Block = [Stmt]

type Program = Map.Map BlockName Block

type State = (Map.Map Reg Int, Map.Map Int Int) 

parseMem :: String -> Mem

parseMem xs = (read $ takeWhile (/='(') xs, init $ tail $ dropWhile (/='(') xs)

str2Op :: [String] -> Stmt
str2Op ["addi", rd, r1, imm] = Addi rd r1 $ read imm 
str2Op ["slti", rd, r1, imm] = Slti rd r1 $ read imm 
str2Op ["sgti", rd, r1, imm] = Sgti rd r1 $ read imm 
str2Op ["xori", rd, r1, imm] = Xori rd r1 $ read imm 
str2Op ["ori" , rd, r1, imm] = Ori  rd r1 $ read imm 
str2Op ["andi", rd, r1, imm] = Andi rd r1 $ read imm 
str2Op ["slli", rd, r1, imm] = Slli rd r1 $ read imm 
str2Op ["srli", rd, r1, imm] = Srli rd r1 $ read imm 
str2Op ["srai", rd, r1, imm] = Srai rd r1 $ read imm 
str2Op ["add", rd, r1, r2] = Add rd r1 r2 
str2Op ["sub", rd, r1, r2] = Sub rd r1 r2 
str2Op ["mul", rd, r1, r2] = Mul rd r1 r2 
str2Op ["div", rd, r1, r2] = Div rd r1 r2 
str2Op ["sll", rd, r1, r2] = Sll rd r1 r2 
str2Op ["slt", rd, r1, r2] = Slt rd r1 r2 
str2Op ["sgt", rd, r1, r2] = Sgt rd r1 r2 
str2Op ["sltu", rd, r1, r2] = Sltu rd r1 r2 
str2Op ["xor", rd, r1, r2] = Xor rd r1 r2 
str2Op ["srl", rd, r1, r2] = Srl rd r1 r2 
str2Op ["sra", rd, r1, r2] = Sra rd r1 r2 
str2Op ["or" , rd, r1, r2] = Or  rd r1 r2 
str2Op ["and", rd, r1, r2] = And rd r1 r2 
str2Op ["li", rd, imm] = Li rd $ read imm 
str2Op ["mv"  , rd , r1] = Mv rd r1 
str2Op ["lw"  , rd , mem] = Lw rd $ parseMem mem
str2Op ["sw"  , rd , mem] = Sw rd $ parseMem mem
str2Op ["j"   , b] = J b 
str2Op ["beqz", rd, b] = Beqz rd b 
str2Op ["bnez", rd, b] = Bnez rd b 
str2Op ["ret", _] = Ret

split :: String -> [String] 
split [] = [""] 
split (c:cs) 
    | c == ',' = "" : rest 
    | otherwise = (c : head rest) : tail rest 
    where 
       rest = split cs 

strictline2Stmt :: String -> Stmt 

strictline2Stmt xs = let (op, para) = (break isSpace xs) 
    in str2Op $ op : (split $ filter (not.isSpace) para)


line2Stmt :: String -> Maybe (Either BlockName Stmt)
line2Stmt xs =
    let strxs = reverse.dropWhile isSpace.reverse.dropWhile isSpace.takeWhile (\c -> c /= '#') $ xs
    in if (strxs == [] || head strxs == '.') then Nothing
        else if (last strxs == ':') then Just $ Left $ dropWhile isSpace (init strxs)
           else Just $ Right $ strictline2Stmt strxs


addline2ProgramList :: [(BlockName, Block)] -> String -> [(BlockName, Block)] 

addline2ProgramList all @ ((bn, b):xs) str = case line2Stmt str of 
    Nothing -> all
    Just (Left blockname) -> (blockname, []):all
    Just (Right realstmt) -> (bn, realstmt:b):xs

addline2ProgramList all str = case line2Stmt str of 
    Nothing -> all
    Just (Left blockname) -> (blockname, []):all



lines2Program :: [String] -> Program

lines2Program = Map.fromList . foldl addline2ProgramList []


setVal :: String -> Int -> State -> State

setVal "zero" _ state = state
setVal s i (re,me) = (Map.insert s i re, me)


getVal :: String -> State -> Int

getVal "zero" _ = 0
getVal k (re,me) = case Map.lookup k re of 
    Just v -> v
    Nothing -> 0


setMemVal :: Mem -> Int -> State -> State

setMemVal (imm, offset) i  state @ (re,me) = (re, Map.insert (imm + (getVal offset state)) i me)


getMemVal :: Mem -> State -> Int

getMemVal (imm, offset)  state @ (re,me) = case Map.lookup (imm + (getVal offset state)) me of
    Just v -> v
    Nothing -> 0


runStmt :: State -> Stmt -> (State, Maybe BlockName)

runStmt state (Addi rd r1 imm) = (setVal rd ((getVal r1 state) + imm) state, Nothing)
runStmt state (Slti rd r1 imm) = (setVal rd (if (getVal r1 state) < imm then 1 else 0) state, Nothing)
runStmt state (Sgti rd r1 imm) = (setVal rd (if (getVal r1 state) > imm then 1 else 0) state, Nothing)
runStmt state (Xori rd r1 imm) = (setVal rd ((getVal r1 state) `xor` imm) state, Nothing)
runStmt state (Ori  rd r1 imm) = (setVal rd ((getVal r1 state) .|. imm) state, Nothing)
runStmt state (Andi rd r1 imm) = (setVal rd ((getVal r1 state) .&. imm) state, Nothing)
runStmt state (Slli rd r1 imm) = (setVal rd ((getVal r1 state) `shiftL` imm) state, Nothing)
--runStmt state (Srli rd r1 imm) = (setVal rd ((getVal r1 state) `shiftR` imm) state, Nothing)
runStmt state (Srai rd r1 imm) = (setVal rd ((getVal r1 state) `shiftR` imm) state, Nothing)
runStmt state (Add rd r1 r2) = (setVal rd ((getVal r1 state) + (getVal r2 state)) state, Nothing)
runStmt state (Sub rd r1 r2) = (setVal rd ((getVal r1 state) - (getVal r2 state)) state, Nothing)
runStmt state (Mul rd r1 r2) = (setVal rd ((getVal r1 state) * (getVal r2 state)) state, Nothing)
runStmt state (Div rd r1 r2) = (setVal rd ((getVal r1 state) `div` (getVal r2 state)) state, Nothing)
runStmt state (Sll rd r1 r2) = (setVal rd ((getVal r1 state) `shiftL` (getVal r2 state)) state, Nothing)
runStmt state (Slt rd r1 r2) = (setVal rd (if (getVal r1 state) < (getVal r2 state) then 1 else 0) state, Nothing)
runStmt state (Sgt rd r1 r2) = (setVal rd (if (getVal r1 state) > (getVal r2 state) then 1 else 0) state, Nothing)
runStmt state (Xor rd r1 r2) = (setVal rd ((getVal r1 state) `xor` (getVal r2 state)) state, Nothing)
--runStmt state (Srl rd r1 r2) = (setVal rd ((getVal r1 state) `shiftR` (getVal r2 state)) state, Nothing)
runStmt state (Sra rd r1 r2) = (setVal rd ((getVal r1 state) `shiftR` (getVal r2 state)) state, Nothing)
runStmt state (Or rd r1 r2) = (setVal rd ((getVal r1 state) .|. (getVal r2 state)) state, Nothing)
runStmt state (And rd r1 r2) = (setVal rd ((getVal r1 state) .&. (getVal r2 state)) state, Nothing)
runStmt state (Li rd imm) = (setVal rd imm state, Nothing)
runStmt state (Mv rd r1) = (setVal rd (getVal r1 state)  state, Nothing) 
runStmt state (Lw rd mem) = (setVal rd (getMemVal mem state) state, Nothing)
runStmt state (Sw rd mem) = (setMemVal mem (getVal rd state) state, Nothing)
runStmt state (J b) = (state, Just b)
runStmt state (Beqz rd b) = (state, if (0== (getVal rd state)) then Just b else Nothing)
runStmt state (Bnez rd b) = (state, if (0/= (getVal rd state)) then Just b else Nothing)
runStmt state (Ret) = (state, Just "return")



--myfoldr f z [] = z
--myfoldr f z (x:xs) = f x (myfoldr f z xs)


runSingleBlock :: Block -> State -> Maybe BlockName -> (State, BlockName)

runSingleBlock _ s (Just b) = (s, b)
runSingleBlock (x:xs) ss Nothing = runSingleBlock xs sss bn where (sss, bn) = runStmt ss x



runBlock :: Program -> BlockName -> State -> (State, BlockName)

runBlock p b s = let (rs,rb) = runSingleBlock (reverse $ case (Map.lookup b p) of Just x -> x) s Nothing in if (rb == "return") then (rs,rb) else runBlock p rb rs



runProgram :: Program -> State

runProgram p = fst $ runBlock p "main" (Map.empty, Map.empty) 




main = do 
    --contents <- return "main:\nli a0,1\nret"
    contents <- getContents
    --putStr(show $ lines2Program $ lines contents)
    putStr(show $ runProgram $ lines2Program $ lines contents)
