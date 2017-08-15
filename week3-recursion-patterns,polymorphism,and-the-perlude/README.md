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