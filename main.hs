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