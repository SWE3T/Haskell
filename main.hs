-- Criação de Função 
dobro x = x + x

-- Função dentro de Função
quad  x = dobro (dobro x)

--Função com include do sistema
raiz  x = sqrt(x)

areaCirc r = pi * r ^ 2 

-- Guarda, chama a funcão após avaliar a entrada
avaliaSinal x | x  > 0 = "Positivo"
              | x == 0 = "Zero"  
              | x  < 0 = "Negativo"


-- Uso de let e where: 
areaTria a b c = let s = (a+b+c)/2 in sqrt (s*(s-a) * (s-b) * (s-c))

areaTri  a b c = sqrt (s*(s-a) * (s-b) * (s-c)) where s = (a+b+c)/2

-- Duas formas de chamar a mesma função, ' => prime
firstOrEmpty :: [String] -> String
firstOrEmpty list = if not (null list)
                    then head list
                    else "Vazia"

firstOrEmpty' [] = "Vazia"
firstOrEmpty' list = head list


-- Declaração da função main 
-- module Main (main) where
main :: IO ()
main = do putStrLn "Digite um valor para dobrar: "
          n1 <- readLn
          putStrLn (show(dobro(n1)))

-- junta :: [String] -> String
junta a b = a ++ b 

inputString = do
    putStr "Escreva seu nome: "
    s <- getLine
    putStrLn(">>~S2~"++s++"~~>")


-- Datas (Algebraic Data Types (ADTs))
data Cliente = OrgGov String    
             | Empresa String Integer String String 
             | Individuo Pessoa Bool 
             deriving Show 
    
    
data Pessoa = Pessoa String String Genero
             deriving Show 

-- let p = Pessoa "Jose" "Maria" Masculino 
data Genero = Masculino | Feminino | Outro
            deriving Show

nomeCliente :: Cliente -> String
nomeCliente cliente = case cliente of 
                    OrgGov nome -> nome
                    Empresa nome id resp cargo -> nome
                    Individuo pessoa ads -> 
                        case pessoa of 
                            Pessoa pNome sNome g -> pNome ++ " " ++ sNome

nomeCliente' :: Cliente -> String
nomeCliente' cliente = case cliente of 
                    OrgGov nome -> nome
                    Empresa nome _ _ _ -> nome
                    Individuo (Pessoa pNome sNome _) _ -> pNome ++ " " ++ sNome


-- Uso de Maybe
nomeEmpresa :: Cliente -> Maybe String 
nomeEmpresa cliente = case cliente of 
                        Empresa nome _ _ _ -> Just nome     
                        _                  -> Nothing 

ehBeninoBenina :: Pessoa -> Maybe Genero 
ehBeninoBenina ser = case ser of
                    Pessoa _ _ gender -> Just gender     

-- Uso de Maybe Oculto
nomeCliente'' :: Cliente -> String
nomeCliente'' (OrgGov nome) = nome
nomeCliente'' (Empresa nome _ _ _) = nome
nomeCliente'' (Individuo (Pessoa pn sn _)_) = pn ++ " " ++ sn


data Nat = Zero | Suc Nat 
        deriving Show    

um :: Nat
um = Suc Zero 

dois :: Nat
dois = Suc (um)

tres :: Nat
tres = Suc (dois)

quatro :: Nat
quatro = Suc (tres)

cinco :: Nat
cinco = Suc (quatro)

nat2int :: Nat -> Integer
nat2int Zero = 0 
nat2int (Suc n) = 1 + nat2int(n)

int2nat :: Integer -> Nat
int2nat 0 = Zero 
int2nat n = Suc (int2nat(n - 1))


-- Exemplo de Função Recursiva 
fatorial :: Integer -> Integer
fatorial n = if n == 0  
            then 1                      --caso base
            else fatorial (n - 1) * n   --recursividade



-- Mede o tamanho da lista 
comprimento :: [Int] -> Int 
comprimento list = if null list
                  then 0
                  else 1 + comprimento (tail list)

-- Outra forma de escrever uma função recursiva, desta vez com pattern matching 
soma :: [Int] -> Int 
soma [ ] = 0 
soma (x : xs) = x + soma(xs)
            

-- Exemplo de Função Recursiva, com Fibonacci  
fibonacci :: Integer -> Integer
fibonacci 0 = 0  
fibonacci 1 = 1
fibonacci 2 = 1  
fibonacci 20 = 6765
fibonacci 30 = 833040 -- checkpoints, ajuda a aumentar o desempenho!
fibonacci 40 = 102478155 -- checkpoints, ajuda a aumentar o desempenho!
fibonacci 50 = 12603980025 -- checkpoints, ajuda a aumentar o desempenho!
fibonacci n = fibonacci(n - 2) + fibonacci(n - 1)   --recursividade

-- Função de alta ordem (recebe uma função como parametro); 
greeting f []  = []
greeting f lst =  map f lst
-- greeting (\x -> "Bem vindo(a), " ++ x) ["Josias", "Maria"]
--          |     Expressão Lambda      | |     lst      |