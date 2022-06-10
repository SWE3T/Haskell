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

natAdd :: Nat -> Nat -> Nat
natAdd Zero Zero = Zero
natAdd (Suc n) Zero = Suc n
natAdd Zero (Suc n) = Suc n
natAdd (Suc n) (Suc m) = Suc(Suc(natAdd n m))
 
natSub :: Nat -> Nat -> Nat
natSub Zero Zero = Zero
natSub (Suc n) Zero = Suc n
natSub (Suc n) (Suc m) = natSub n m 
 
natMul :: Nat -> Nat -> Nat
natMul _ Zero = Zero
natMul Zero _ = Zero
natMul (Suc m) (Suc n) = natAdd (Suc m) (natMul (Suc m) n)