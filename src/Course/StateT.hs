{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Applicative
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Monad
import Course.Optional
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary
-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a = StateT
  { runStateT :: s -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) :: (a -> b) -> StateT s f a -> StateT s f b
  f <$> StateT k = StateT $ (first f <$>) . k

-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
--
-- f s == f (a, s)
instance Monad f => Applicative (StateT s f) where
  pure :: a -> StateT s f a
  pure a = StateT (\s -> return (a, s))
  (<*>) :: StateT s f (a -> b) -> StateT s f a -> StateT s f b
  StateT f <*> StateT g =
    StateT $ \s -> do
      (fun, s2) <- f s
      (a, s3) <- g s2
      return (fun a, s3)
    -- StateT $ \s -> f s >>= \(f, s) -> g s >>= (\(x, s2) -> return (f x, s2))

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) :: (a -> StateT s f b) -> StateT s f a -> StateT s f b
  g =<< (StateT f) =
    StateT $ \s -> do
      (a, nextS) <- f s
      runStateT (g a) nextS

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a = StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne  ((),1)
state' :: (s -> (a, s)) -> State' s a
state' f = StateT $ \s -> ExactlyOne $ f s

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' :: State' s a -> s -> (a, s)
runState' (StateT f) s = runExactlyOne $ f s

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
-- f s :: f (a, s)
execT :: Functor f => StateT s f a -> s -> f s
execT stateT s = snd <$> runStateT stateT s

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' :: State' s a -> s -> s
exec' state = snd . runState' state

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT :: Functor f => StateT s f a -> s -> f a
evalT stateT s = fst <$> runStateT stateT s

-- | Run the `State'` seeded with `s` and retrieve the resulting value.
eval' :: State' s a -> s -> a
eval' state = fst . runState' state

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT :: Applicative f => StateT s f s
getT = StateT $ \s -> pure (s, s)

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT :: Applicative f => s -> StateT s f ()
putT s = StateT $ \_ -> pure ((), s)

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' :: (Ord a, Num a) => List a -> List a
distinct' xs = eval' go S.empty
  where
    go =
      filtering
        (\a -> do
           s <- getT
           if S.member a s
             then return False
             else putT (S.insert a s) >> return True)
        xs

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF :: (Ord a, Num a) => List a -> Optional (List a)
distinctF xs = do
  list <- evalT (go xs) S.empty
  case find (> 100) list of
    Empty -> return list
    _ -> Empty
  where
    go :: (Ord a, Num a) => List a -> StateT (S.Set a) Optional (List a)
    go = filtering condition
    condition :: (Ord a, Num a) => a -> StateT (S.Set a) Optional Bool
    condition value = do
      s <- getT
      if S.member value s
        then return False
        else do
          putT (S.insert value s)
          return True

-- | An `OptionalT` is a functor of an `Optional` value.
newtype OptionalT f a = OptionalT
  { runOptionalT :: f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) :: (a -> b) -> OptionalT f a -> OptionalT f b
  f <$> (OptionalT g) = OptionalT $ (<$>) f <$> g

-- | Implement the `Applicative` instance for `OptionalT f` given a Monad f.
--
-- /Tip:/ Use `onFull` to help implement (<*>).
--
-- >>> runOptionalT $ OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- []
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
-- []
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty,Empty]
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
--
-- onFull :: Applicative f => (t -> f (Optional a)) -> Optional t -> f (Optional a)
instance Monad f => Applicative (OptionalT f) where
  pure :: a -> OptionalT f a
  pure = OptionalT . return . return
  (<*>) :: OptionalT f (a -> b) -> OptionalT f a -> OptionalT f b
  OptionalT (g :: f (Optional (a -> b))) <*> mx =
    OptionalT $ do
      optionalFunction <- g
      case optionalFunction of
        Empty -> pure Empty
        Full someFunction -> runOptionalT $ someFunction <$> mx

-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  (=<<) :: (a -> OptionalT f b) -> OptionalT f a -> OptionalT f b
  g =<< OptionalT o =
    OptionalT $ do
      (o' :: Optional a) <- o
      case o' of
        Full a -> runOptionalT (g a)
        Empty -> return Empty


-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l)
         a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) :: (a -> b) -> Logger l a -> Logger l b
  f <$> Logger l a = Logger l (f a)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure :: a -> Logger l a
  pure = Logger Nil

  (<*>) :: Logger l (a -> b) -> Logger l a -> Logger l b
  Logger l1 f <*> Logger l2 x = Logger (l1 ++ l2) (f x)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) :: (a -> Logger l b) -> Logger l a -> Logger l b
  f =<< Logger l x = Logger l id <*> f x

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 :: l -> a -> Logger l a
log1 l = Logger (l :. Nil)

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG :: (Integral a, Show a) => List a -> Logger Chars (Optional (List a))
distinctG xs = runOptionalT $ evalT (filtering cond' xs) S.empty

-- if I have
--
-- cond' :: a -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
--
-- Then,
--
-- I can get
--
-- StateT (S.Set a) (OptionalT (Logger Chars)) (List a)
--
-- by running filtering cond' xs
--
-- Then I can run runStateT
--
-- to get
--
-- OptionalT (Logger Chars) (List a)
--
-- then I can run runOptionalT to get
--
-- Logger Chars (Optional (List a))
--

cond' ::
     (Show a, Integral a)
  => a
  -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
cond' a = StateT (handler' a) -- (f (a, s)) should return OptionalT (Logger Chars) (Bool, S.Set a)

handler' ::
     (Show a, Integral a)
  => a
  -> S.Set a
  -> OptionalT (Logger Chars) (Bool, S.Set a)
handler' value set = OptionalT (optionHandler value set)

optionHandler ::
     (Show a, Integral a)
  => a
  -> S.Set a
  -> Logger Chars (Optional (Bool, S.Set a))
optionHandler value set
  | value <= 100 && S.member value set =
    if even value
      then log1 (logEvenNumber value) (Full (False, set))
      else log1 "" (Full (False, set))
  | value <= 100 && even value =
    log1 (logEvenNumber value) (Full (True, S.insert value set))
  | value <= 100 = return $ Full (True, S.insert value set)
  | otherwise = log1 (logAbort value) Empty


-- | Example
--
-- >>> warnMessage "aborting > 100: " 5
-- "aborting > 100: 5"
--
-- >>> warnMessage "even number: " 4
-- "even number: 4"
warnMessage :: (Show a, Integral a) => Chars -> a -> Chars
warnMessage prefix value = prefix ++ show' value


logEvenNumber :: (Show a, Integral a) => a -> Chars
logEvenNumber = warnMessage "even number: "

logAbort :: (Show a, Integral a) => a -> Chars
logAbort = warnMessage "aborting > 100: "














-- distinctG xs = runOptionalT (evalT (goFiltering xs) S.empty)

-- goFiltering :: (Show a, Integral a) => List a -> StateT (S.Set a) (OptionalT (Logger Chars)) (List a)
-- goFiltering = filtering cond

-- cond ::
     -- (Show a, Integral a)
  -- => a
  -- -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
-- cond a = StateT (OptionalT . go a)

-- go ::
     -- (Show a, Integral a)
  -- => a
  -- -> S.Set a
  -- -> Logger Chars (Optional (Bool, S.Set a))
-- go a s =
  -- if a > 100
    -- then log1 (fromString ("aborting > 100: " P.++ show a)) Empty
    -- else (if even a
            -- then log1 (fromString ("even number: " P.++ show a))
            -- else pure)
           -- (Full (a `S.notMember` s, a `S.insert` s))


onFull :: Applicative f => (t -> f (Optional a)) -> Optional t -> f (Optional a)
onFull g o =
  case o of
    Empty -> pure Empty
    Full a -> g a
