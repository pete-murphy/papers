class Comonad d where
  counit :: d a -> a
  cobind :: (d a -> b) -> d a -> d b

cmap :: Comonad d => (a -> b) -> d a -> d b
cmap f = cobind (f . counit)

newtype Id a = Id a

instance Comonad Id where
  counit (Id a) = a
  cobind k d = Id (k d)

data Prod e a = a :& e

instance Comonad (Prod e) where
  counit (a :& _) = a
  cobind k d@(_ :& e) = k d :& e

askP :: Prod e a -> e
askP (_ :& e) = e

localP :: (e -> e) -> Prod e a -> Prod e a
localP g (a :& e) = a :& g e

data Stream a = a :< Stream a -- coinductive (??)
  deriving Show

instance Comonad Stream where
  counit (a :< _) = a
  cobind k d@(_ :< as) = k d :< cobind k as

nextS :: Stream a -> Stream a
nextS (_ :< as) = as

-- Streams are "naturally isomorphic to functions from natural numbers"

str2fun :: Stream a -> (Int -> a)
str2fun (a :< _)  0 = a
str2fun (_ :< as) n = str2fun as (n - 1)

fun2str_ :: (Int -> a) -> Stream a
fun2str_ fun = fun 0 :< fun2str_ (\i -> fun (i + 1))

fun2str :: (Int -> a) -> Stream a
fun2str f = fun2str' f 0

fun2str' f i = f i :< fun2str' f (i + 1)

toList :: Stream a -> [a]
toList (a :< as) = a : toList as

-- $> take 10 $ toList $ fun2str_ id

{-
the object mapping FunArgS is a comonad (in fact, it is the “state-in-context” comonad considered by Kieburtz ...
-}

data FunArg s a = (s -> a) :# s

instance Comonad (FunArg s) where
  counit (f :# s) = f s
  cobind k d@(f :# s) = (\s' ->  k (f :# s')) :# s

runFA :: (FunArg Int a -> b) -> Stream a -> Stream b
runFA k as = runFA' k (str2fun as :# 0)

runFA' k d@(f :# i) = k d :< runFA' k (f :# (i + 1))

{- 
I was wrong about this
runFA_ :: (FunArg Int a -> b) -> Stream a -> Stream b
runFA_ k (a :< as) = k ((\_ -> a) :# 0) :< runFA_ k as
-}

-- $> take 10 $ toList $ runFA (\(f :# x) -> f x + 2) (fun2str (\n -> n * 100))

data List a = Nil | List a :> a

data LV a = List a := a

data LVS a = LV a :| Stream a

instance Comonad LVS where
  counit (az := a :| as) = a
  cobind k d = cobindL d := k d :| cobindS d
    where
      cobindL (Nil       := a :| as) = Nil
      cobindL (az' :> a' := a :| as) = cobindL d' :> k d'
        where d' = az' := a' :| (a :< as)

      cobindS (az := a :| (a' :< as')) = k d' :< cobindS d'
        where d' = az :> a := a' :| as'

-- runLVS :: (LVS a -> b) -> Stream a -> Stream b
-- runLVS k (a' :< as') = 

main = pure ()
