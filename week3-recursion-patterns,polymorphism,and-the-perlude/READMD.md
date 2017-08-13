# Recursion patterns, polymorphism, and the Prelude

## Recursion patterns
Int 를 값으로 가지는 list 를 다음과 같이 표현할 수 있다.
```haskell
data IntList = Empty | Cons Int IntList
	deriving Show
```

IntList 로 아마 이런 것들을 하고 싶을 것이다.
- list 의 모든 요소에 대해 어떤 오퍼레이션을 수행하고 싶다.
- list 의 몇몇 요소는 유지하고 몇몇 개는 버리고 싶다.
- list 의 요소에 대해 요약(?) 하고 싶다. 예를 들면, sum, product, maximum 등

#### Map
list 읨 모든 요소에 어떤 오퍼레이션을 수행한다고 해보자. 예를 들면 모든 요소의 절대값을 구하고 싶다.

```haskell
absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)
```

모든 요소를 제곱화하기

```haskell
squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)
```

헛. 너의 머리에서 위험감지를 했을 것이다. 이 함수들을 보니 유사한 점이 많다. 반복하면 안되는데... 다른 부분만 함수로 인자로 받아서 좀더 일반적인 함수를 만들어보자.

```haskell
exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x + 1
square x = x * x

mapIntList addOne exampleList
mapIntList abs exampleList
mapIntList square exampleList
```

#### Filter
또 list 에 몇몇 요소는 유지하고 버리는 패턴이 자주 나온다. 예를 들면,

```haskell
keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
	| even x = Cons x (KeepOnlyEven xs)
	| otherwise = KeepOnlyEven xs
```

어떻게 이를 일반화 할 수 있을까? 추상화를 하려면 어떻게 해야할까?

#### Fold
마지막 패턴은 list 의 요소를 "summarize" 하는 것이다. 잘 알려진 방법은 "fold" 또는 "reduce" 이다.

## Polymorphism