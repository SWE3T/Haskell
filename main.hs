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
