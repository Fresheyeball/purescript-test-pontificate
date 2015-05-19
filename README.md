# Module Documentation


## Module Test.Pontificate.Applicative

#### `Pure`

``` purescript
type Pure f a = a -> f a
```


#### `Category`

``` purescript
type Category f a b c = f b c -> f a b -> a -> c
```


#### `checkApplicative'`

``` purescript
checkApplicative' :: forall f a b c. (Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), Arbitrary a, CoArbitrary a, Arbitrary b, Arbitrary c, CoArbitrary c, Arbitrary (f c), Show a, Show (f a), Show (f b), Show (f c)) => CustomEq (f a) -> CustomEq (f b) -> CustomEq (f c) -> Fmap f c c -> Fmap f (b -> c) ((a -> b) -> a -> c) -> Ap f a c -> Ap f (a -> b) (a -> c) -> Ap f a b -> Ap f b c -> Ap f a a -> Ap f (b -> c) ((a -> b) -> a -> c) -> Ap f a b -> Ap f (a -> b) b -> Pure f (a -> a) -> Pure f (Category Prim.Function a b c) -> Pure f b -> Pure f a -> Pure f (a -> b) -> Pure f ((a -> b) -> b) -> QC Unit
```


#### `checkApplicative`

``` purescript
checkApplicative :: forall f a b c. (Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), Arbitrary a, CoArbitrary a, Arbitrary b, Arbitrary c, CoArbitrary c, Arbitrary (f c), Show a, Show (f a), Show (f b), Show (f c), Eq (f a), Eq (f b), Eq (f c)) => Fmap f c c -> Fmap f (b -> c) ((a -> b) -> a -> c) -> Ap f a c -> Ap f (a -> b) (a -> c) -> Ap f a b -> Ap f b c -> Ap f a a -> Ap f (b -> c) ((a -> b) -> a -> c) -> Ap f a b -> Ap f (a -> b) b -> Pure f (a -> a) -> Pure f (Category Prim.Function a b c) -> Pure f b -> Pure f a -> Pure f (a -> b) -> Pure f ((a -> b) -> b) -> QC Unit
```


#### `checkApplicativeInstance'`

``` purescript
checkApplicativeInstance' :: forall f a b c fn ap. (Applicative f, Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), Arbitrary a, CoArbitrary a, Arbitrary b, Arbitrary c, CoArbitrary c, Arbitrary (f c), Show a, Show (f a), Show (f b), Show (f c)) => CustomEq (f a) -> CustomEq (f b) -> CustomEq (f c) -> QC Unit
```


#### `checkApplicativeInstance`

``` purescript
checkApplicativeInstance :: forall f a b c. (Applicative f, Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), Arbitrary a, CoArbitrary a, Arbitrary b, Arbitrary c, CoArbitrary c, Arbitrary (f c), Show a, Show (f a), Show (f b), Show (f c), Eq (f a), Eq (f b), Eq (f c)) => f a -> f b -> f c -> QC Unit
```



## Module Test.Pontificate.Apply

#### `Ap`

``` purescript
type Ap f a b = f (a -> b) -> f a -> f b
```


#### `checkApply'`

``` purescript
checkApply' :: forall f a b c. (Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), Arbitrary c, CoArbitrary c, Arbitrary (f c), Show (f a), Show (f b), Show (f c)) => CustomEq (f c) -> Fmap f c c -> Fmap f (b -> c) ((a -> b) -> a -> c) -> Ap f a c -> Ap f (a -> b) (a -> c) -> Ap f a b -> Ap f b c -> QC Unit
```


#### `checkApply`

``` purescript
checkApply :: forall f a b c. (Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), Arbitrary c, CoArbitrary c, Arbitrary (f c), Show (f a), Show (f b), Show (f c), Eq (f c)) => Fmap f c c -> Fmap f (b -> c) ((a -> b) -> a -> c) -> Ap f a c -> Ap f (a -> b) (a -> c) -> Ap f a b -> Ap f b c -> QC Unit
```


#### `checkApplyInstance'`

``` purescript
checkApplyInstance' :: forall f a b c. (Apply f, Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), Arbitrary c, CoArbitrary c, Arbitrary (f c), Show (f a), Show (f b), Show (f c)) => CustomEq (f c) -> f a -> f b -> QC Unit
```


#### `checkApplyInstance`

``` purescript
checkApplyInstance :: forall f a b c. (Apply f, Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), Arbitrary c, CoArbitrary c, Arbitrary (f c), Show (f a), Show (f b), Show (f c), Eq (f c)) => f c -> f a -> f b -> QC Unit
```



## Module Test.Pontificate.Bifunctor

#### `checkBifunctor`

``` purescript
checkBifunctor :: forall f a b. (Bifunctor f, Arbitrary a, CoArbitrary a, Arbitrary b, CoArbitrary b, Arbitrary (f a b), Show a, Eq (f a b)) => f a b -> QC Unit
```



## Module Test.Pontificate.Binary

#### `CustomEq`

``` purescript
type CustomEq a = a -> a -> Boolean
```


#### `Binary`

``` purescript
type Binary a = a -> a -> a
```


#### `commutative'`

``` purescript
commutative' :: forall a. (Show a) => CustomEq a -> Binary a -> a -> a -> Result
```


#### `commutative`

``` purescript
commutative :: forall a. (Eq a, Show a) => Binary a -> a -> a -> Result
```


#### `associative'`

``` purescript
associative' :: forall a. (Show a) => CustomEq a -> Binary a -> a -> a -> a -> Result
```


#### `associative`

``` purescript
associative :: forall a. (Show a, Eq a) => Binary a -> a -> a -> a -> Result
```


#### `distributive'`

``` purescript
distributive' :: forall a. (Show a) => CustomEq a -> Binary a -> Binary a -> a -> a -> a -> Result
```


#### `distributive`

``` purescript
distributive :: forall a. (Show a, Eq a) => Binary a -> Binary a -> a -> a -> a -> Result
```



## Module Test.Pontificate.Bind

#### `Bind`

``` purescript
type Bind m a b = m a -> (a -> m b) -> m b
```


#### `checkBind'`

``` purescript
checkBind' :: forall m a b c. (Arbitrary (m a), Arbitrary (m b), Arbitrary (m c), Arbitrary a, CoArbitrary a, CoArbitrary b, Show (m a), Show (m c)) => CustomEq (m c) -> Bind m a b -> Bind m b c -> Bind m a c -> QC Unit
```


#### `checkBind`

``` purescript
checkBind :: forall m a b c. (Arbitrary (m a), Arbitrary (m b), Arbitrary (m c), Arbitrary a, CoArbitrary a, CoArbitrary b, Eq (m c), Show (m a), Show (m c)) => Bind m a b -> Bind m b c -> Bind m a c -> QC Unit
```


#### `checkBindInstance'`

``` purescript
checkBindInstance' :: forall m a b c. (Bind m, Arbitrary (m a), Arbitrary (m b), Arbitrary (m c), Arbitrary a, CoArbitrary a, CoArbitrary b, Show (m a), Show (m c)) => CustomEq (m c) -> m a -> m b -> QC Unit
```


#### `checkBindInstance`

``` purescript
checkBindInstance :: forall m a b c. (Bind m, Arbitrary (m a), Arbitrary (m b), Arbitrary (m c), Arbitrary a, CoArbitrary a, CoArbitrary b, Eq (m c), Show (m a), Show (m c)) => m a -> m b -> m c -> QC Unit
```



## Module Test.Pontificate.Functor

#### `Fmap`

``` purescript
type Fmap f a b = (a -> b) -> f a -> f b
```


#### `checkFunctor'`

``` purescript
checkFunctor' :: forall f a. (Arbitrary a, CoArbitrary a, Arbitrary (f a), Show (f a)) => CustomEq (f a) -> Fmap f a a -> QC Unit
```


#### `checkFunctor`

``` purescript
checkFunctor :: forall f a. (Arbitrary a, CoArbitrary a, Arbitrary (f a), Eq (f a), Show (f a)) => Fmap f a a -> QC Unit
```


#### `checkFunctorInstance'`

``` purescript
checkFunctorInstance' :: forall f a. (Functor f, Arbitrary a, CoArbitrary a, Arbitrary (f a), Show (f a)) => CustomEq (f a) -> QC Unit
```


#### `checkFunctorInstance`

``` purescript
checkFunctorInstance :: forall f a. (Functor f, Eq (f a), Arbitrary a, CoArbitrary a, Arbitrary (f a), Show (f a)) => f a -> QC Unit
```



## Module Test.Pontificate.Fuzzy

#### `(/=~=)`

``` purescript
(/=~=) :: forall a. (FuzzyEq a) => a -> a -> Boolean
```


#### `FuzzyEq`

``` purescript
class FuzzyEq a where
  (=~=) :: a -> a -> Boolean
```


#### `fuzzyNumbers`

``` purescript
instance fuzzyNumbers :: FuzzyEq Number
```



## Module Test.Pontificate.Monad

#### `checkMonad'`

``` purescript
checkMonad' :: forall m a b c. (Arbitrary (m a), Arbitrary (m b), Arbitrary (m c), Arbitrary a, CoArbitrary a, Arbitrary b, CoArbitrary b, Arbitrary c, CoArbitrary c, Arbitrary (m (a -> b)), Arbitrary (m (b -> c)), Show a, Show c, Show (m a), Show (m b), Show (m c)) => CustomEq (m a) -> CustomEq (m b) -> CustomEq (m c) -> Fmap m c c -> Fmap m (b -> c) ((a -> b) -> a -> c) -> Ap m a c -> Ap m (a -> b) (a -> c) -> Ap m a b -> Ap m b c -> Ap m a a -> Ap m (b -> c) ((a -> b) -> a -> c) -> Ap m a b -> Ap m (a -> b) b -> Pure m (a -> a) -> Pure m (Category Prim.Function a b c) -> Pure m a -> Pure m b -> Pure m c -> Pure m (a -> b) -> Pure m ((a -> b) -> b) -> Bind m a a -> Bind m a b -> Bind m a c -> Bind m b c -> Bind m c c -> QC Unit
```


#### `checkMonad`

``` purescript
checkMonad :: forall m a b c. (Arbitrary (m a), Arbitrary (m b), Arbitrary (m c), Arbitrary a, CoArbitrary a, Arbitrary b, CoArbitrary b, Arbitrary c, CoArbitrary c, Arbitrary (m (a -> b)), Arbitrary (m (b -> c)), Show a, Show c, Show (m a), Show (m b), Show (m c), Eq (m a), Eq (m b), Eq (m c)) => Fmap m c c -> Fmap m (b -> c) ((a -> b) -> a -> c) -> Ap m a c -> Ap m (a -> b) (a -> c) -> Ap m a b -> Ap m b c -> Ap m a a -> Ap m (b -> c) ((a -> b) -> a -> c) -> Ap m a b -> Ap m (a -> b) b -> Pure m (a -> a) -> Pure m (Category Prim.Function a b c) -> Pure m a -> Pure m b -> Pure m c -> Pure m (a -> b) -> Pure m ((a -> b) -> b) -> Bind m a a -> Bind m a b -> Bind m a c -> Bind m b c -> Bind m c c -> QC Unit
```


#### `checkMonadInstance'`

``` purescript
checkMonadInstance' :: forall m a b c. (Monad m, Arbitrary (m a), Arbitrary (m b), Arbitrary (m c), Arbitrary a, CoArbitrary a, Arbitrary b, CoArbitrary b, Arbitrary c, CoArbitrary c, Arbitrary (m (a -> b)), Arbitrary (m (b -> c)), Show a, Show c, Show (m a), Show (m b), Show (m c)) => CustomEq (m a) -> CustomEq (m b) -> CustomEq (m c) -> QC Unit
```


#### `checkMonadInstance`

``` purescript
checkMonadInstance :: forall m a b c. (Monad m, Arbitrary (m a), Arbitrary (m b), Arbitrary (m c), Arbitrary a, CoArbitrary a, Arbitrary b, CoArbitrary b, Arbitrary c, CoArbitrary c, Arbitrary (m (a -> b)), Arbitrary (m (b -> c)), Show a, Show c, Show (m a), Show (m b), Show (m c), Eq (m a), Eq (m b), Eq (m c)) => m a -> m b -> m c -> QC Unit
```



## Module Test.Pontificate.Monoid

#### `Id`

``` purescript
type Id a = a
```


#### `checkMonoid'`

``` purescript
checkMonoid' :: forall a. (Show a, Arbitrary a, CoArbitrary a) => CustomEq a -> Binary a -> Id a -> QC Unit
```


#### `checkMonoid`

``` purescript
checkMonoid :: forall a. (Eq a, Show a, Arbitrary a, CoArbitrary a) => Binary a -> Id a -> QC Unit
```


#### `checkMonoidInstance'`

``` purescript
checkMonoidInstance' :: forall a. (Monoid a, Show a, Arbitrary a, CoArbitrary a) => CustomEq a -> QC Unit
```


#### `checkMonoidInstance`

``` purescript
checkMonoidInstance :: forall a. (Monoid a, Eq a, Show a, Arbitrary a, CoArbitrary a) => a -> QC Unit
```


#### `checkCommutativeMonoid'`

``` purescript
checkCommutativeMonoid' :: forall a. (Show a, Arbitrary a, CoArbitrary a) => CustomEq a -> Binary a -> Id a -> QC Unit
```


#### `checkCommutativeMonoid`

``` purescript
checkCommutativeMonoid :: forall a. (Eq a, Show a, Arbitrary a, CoArbitrary a) => Binary a -> Id a -> QC Unit
```



## Module Test.Pontificate.Semigroup

#### `checkSemigroup'`

``` purescript
checkSemigroup' :: forall a. (Show a, Arbitrary a, CoArbitrary a) => CustomEq a -> Binary a -> QC Unit
```


#### `checkSemigroup`

``` purescript
checkSemigroup :: forall a. (Eq a, Show a, Arbitrary a, CoArbitrary a) => Binary a -> QC Unit
```


#### `checkSemigroupInstance'`

``` purescript
checkSemigroupInstance' :: forall a. (Semigroup a, Show a, Arbitrary a, CoArbitrary a) => CustomEq a -> QC Unit
```


#### `checkSemigroupInstance`

``` purescript
checkSemigroupInstance :: forall a. (Semigroup a, Show a, Arbitrary a, CoArbitrary a, Eq a) => a -> QC Unit
```



## Module Test.Pontificate.Semiring

#### `checkSemiring'`

``` purescript
checkSemiring' :: forall a. (Show a, Arbitrary a, CoArbitrary a) => CustomEq a -> Binary a -> a -> Binary a -> a -> QC Unit
```


#### `checkSemiring`

``` purescript
checkSemiring :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a) => Binary a -> Id a -> Binary a -> Id a -> QC Unit
```


#### `checkSemiringInstance'`

``` purescript
checkSemiringInstance' :: forall a. (Semiring a, Arbitrary a, CoArbitrary a, Show a) => CustomEq a -> QC Unit
```


#### `checkSemiringInstance`

``` purescript
checkSemiringInstance :: forall a. (Semiring a, Arbitrary a, CoArbitrary a, Show a, Eq a) => a -> QC Unit
```
