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
앞에서 `IntList` 에 대해 아주 제너널한 함수들 (map, filter, fold) 을 만들었지만 아직 끝나지 않았다. 만약 `IntList` 가 아닌 `BoolList` 또는 `StringList` 등 다른 형태의 List 의 경우도 이런 제너널한 함수를 사용하고 싶으면 어떻게 해야할까? 그 때마다 새로운 타입을 만들고 새로운 함수를 추가해야할까? 최약의 경우는 타입만 다르고 함수 코드가 완전히 똑같은 경우도 생길 것이다.

이 문제를 하스켈에서는 `polymorphism`으로 해결한다. 이 단어는 그리스에서 유래했으며 많은 형태를 가진다는 것을 의미한다.

### Polymorphic data types
먼저, 다음과 같이 polymorphic 데이터 타입을 선언하는 방법을 보자.

```haskell
data List t = E | C t (List t)
```

(우리는 Empty, Cons 을 재활용할 수 없다. 이미 IntList 생성자에 써버려서, 그대신 E와 C를 사용했다.) 반면 data IntList = ... 을 하기 전에 data List t = ... 을 선언했다. t 는 type 변수이며 어떤 타입을 대표할 수 있다. (타입 변수는 소문자로, 타입은 대문자로 시작해야한다.) data List t = ... 는 List type이 type으로 파라미터화 되었으며 함수도 인자도 동일한 타입으로 입력받을 수 있다는 것을 의미한다.

type t 에서 `(List t)`는 생성자 `E` 또는 `t` 와 또 다른 `(List t)`를 받는 생성자 `C` 로 만들 수 있다. 다음 예제를 보자.

```haskell
lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)
```

### Polymorphic functions
지금부터 새로운 polymorphic `List`에 맞게 `filterIntList`를 일반화해보자. 그냥 `Empty` 를 `E`로 `Cons`를 `C`로 바꾸면 된다.

```haskell
filterList _ E = E
filterList p (C x xs)
	| p x = C x (filterList p xs)
	| otherwise = filterList p xs
```

자, `filterList`의 타입은 무엇인가? `ghci`가 타입추론을 어떻게하는지 보자.

```haskell
*Main> :t filterList
filterList :: (t -> Bool) -> List t -> List t
```

우리는 이렇게 해석할 수 있다.: "어떤 타입 t에 대해 `filterList`는 (t -> Bool)인 함수를 받으며 t 타입 list 그리고 t 타입 list 를 반환한다."

`mapIntList` 는 어떻게 일반화할까? `mapList` 에 어떤 타입을 줘야할까?

```haskell
mapList :: (t -> t) -> List t -> List t
```

```haskell
mapList :: (a -> b) -> List a -> List b
mapList _ E = E
mapList f (C x xs) = C (f x) (mapList f xs)
```

polymorphic functions 핵심은 `the caller gets to pick the types` 이다.

## The Prelude

`Prelude` 는 모든 하스켈 프로그램에 암시적으로 임포트되는 모듈 중 하나이다. `Prelude` 모듈 안에는 다양한 polymorphic functions 가 정의되어 있다. 예를 들면, 우리가 만든 filterList 와 mapList 가 filter, map 으로 정의되어 있다. 한번쯤 도큐먼트를 훑어보는 것을 추천한다.

그외에 알면 좋은 polymorphic type은 `Maybe` 이다.

```haskell
data Maybe a = Nothing | Just a
```

`Maybe` 타입은 값이 있거나 없거나를 표현한다. 값이 있는 경우는 `Just` 생성자로 값을 만든 경우이며 아닌 경우는 `Nothing` 생성자로 만든 경우이다.

### Total and partial functions
다음과 같은 polymoarphic type 을 보자.

```haskell
[a] -> a
```

어떤 함수가 이런 타입을 가질 수 있는가? 이 타입은 단지 `a` 타입의 리스트가 주어지면 함수가 반드시 `a` 타입의 값을 리턴한다는 의미다. 예를 들면 Prelude function `head` 함수가 같이 말이다.

그러나, head 가 텅빈 리스트를 입력받으면 어떻게 될까? head 소스코드를 봐라..

박살난다! 모든 타입에 대해 동작해야하기 때문에 뭔가 할 수 있는 다른 행동이 없다.

`head` 는 `partial function`으로 알려져있다: 특정 input에 대해서는 head가 박살난다. 특정 input에 대해 재귀적으로 동작하는 함수 또한 `partial` 함수라고 한다. 모든 경우에 대해 잘 정의된 함수는 `total` 함수라고 한다.

가능한한 partial function 을 피하는 것이 하스켈의 좋은 프렉티스이다. 사실 partial function을 피하는 것은 어떤 프로그래밍 언어에서나 좋은 프렉티스이다. 하지만 하스켈에서는 이를 좀 더 쉽게 이를 수 있다.

**head is a mistake!** 이 함수는 `Prelude`에 있어서는 안된다. 다른 partial Prelude 함수는 `tail`, `init`, `last`, `!!` 또한 동일하다.

### Replacing partial functions
`head`, `tail`과 같은 partial functions 는 pattern-matching 으로 대체할 수 있다. 다음 두 가지 정의를 보자.

```haskell
doStuff1 :: [Int] -> Int
doStuff1 [] = 0
doStuff1 [_] = 0
doStuff1 xs = head xs + (head (tail xs))

doStuff2 :: [Int] -> Int
doStuff2 [] = 0
doStuff2 [_] = 0
doStuff2 (x1:x2:_) = x1 + x2
```

이 함사는 정확히 같은 결과를 계산하는 함수며 둘다 `total` 함수다. 그렇지만 두 번째 함수가 좀더 `total` 함수이며 좀 더 읽기가 쉽다.

### Writing partial functions
`partial function`을 작성한다면? 취해야할 2가지 접근 방법이 있다. 첫번째는 함수의 output 타입을 possible failure 를 명시적으로 나타낼 수 있도록 바꾼다. `Maybe`를 다시 생각해보자.

```haskell
data Maybe a = Nothing | Just a
```

지금부터 `head` 함수를 작성한다고 해보자. 이렇게 안전하게 작성할 수 있다.

```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
```

정확히는 safe package에 이와 같은 함수가 정의되어 있긴하다.

이는 왜 좋은 아이디어일까?
1. `safeHead` 는 절대 박살날 일이 없다.
2. `safeHead` 타입은 몇몇 입력에 대해 실패할 수 있다는 것을 명시적으로 표현하다.
3. 타입 시스템은 `safeHead`의 유저에게 반드시 `safeHead` 의 리턴 값이 값이 존재하는지 존재하지 않는지 확인하도록 한다.

다른 관점으로는 `safeHead`가 여전히 `partial`이다. 그렇지만 타입 시스템에서는 `partiality`를 명시적으로 표현했으며 그러므로 안전하다. 목표는 타입들로 최대한 함수에 대한 행동을 명시적으로 표현하는 것이다.

좋다. 그렇지만 만약 빈 배열이 절대로 들어가지 않는 상황을 보장하는 곳에서 `head`를 사용했다면? 이와 같은 상황에서 다시 `Maybe`로 돌아가기가 귀찮을 것이다.

답은 만약 그런 상황이 보장된다면, 보장하는 상황을 타입으로 표현하면 된다. 컴파일러가 너가 보장하는 상황을 강제화할 수 있을 것이다. 예를 들면

```haskell
data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel [] = Nothing
listToNel (x:xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as
```