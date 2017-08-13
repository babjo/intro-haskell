# Algebraic Data Types
## Enumeration types
많은 프로그래밍 언어와 같이 하스켈에션 자신만의 enumeration 타입을 만들 수 있다.
```haskell
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
   deriving Show
```

`Thing` 이라는 새로운 타입을 선언했다. Thing 타입에는 `Shoe`, `Ship` 등 5가지 데이터 타입을 가진다. (여기 `deriving Show`는 마법주분이다. GHC가 자동으로 `Things`를 Strings로 바꾸는 코드를 자동으로 생성 해준다. ghci가 `Thing type`을 출력할 때 사용한다.)


```haskell
shoe :: Thing
shoe= Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]
```

또 `Thing`를 패턴매칭으로 함수를 쓸 수 있다.

```haskell
isSmall :: Thing -> Bool
isSmall Shoe = True
isSmall Ship = False
isSmall SealingWax = True
isSmall Cabbage = True
isSmall King = False
```

위에서부터 아래로 매칭하기에 `_` 를 활용해서 좀더 짧께 쓰는 것도 가능하다.

```haskell
isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _ = True
```

## Beyond enumerations

`Thing` 은 enumeration type 으로 Java, C++ 와 같은 언어에서 제공해주는 것이 비슷하다. 그러나, enumerations 은 하스켈의 algebraic data types 의 특수한 경우일 뿐이다. 다음 예제를 보자.

```haskell
data FailableDouble = Failuare
					| OK Double
  deriving Show
```

`FailableDouble` 타입은 두 가지 생성자를 가진다. 첫 번째로 Failure 는 어떤 인자도 가지지 않으며 `Failure` 그 자체로 FailableDouble 의 값이다. 그리고 두 번째, `OK` 는 Double 이라는 인자를 가지며 `OK` 그 자체로 FailableDouble 의 값은 아니다. Double 을 넘겨줘야지만 FailableDouble 타입의 값이 될 수 있다.

```haskell
ex01 = Failure
ex02 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d
```

물론 데이터 생성에 있어서 하나 이상의 인자를 가질 수 있다.
```haskell
-- Store a person's name, age, and favourite Thing.

data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a
```

## Algebraic data types in general
일반적으로  algebraic data type 은 하나 이상의 데이터 생성자를 가지며 각 데이터 생성자는 최소 0개 이상의 인자를 받는다.

```haskell
data AlgDataType = Constr1 Type11 Type12
				 | Constr2 Typr21
				 | Constr3 Type31 Type32 Type33
				 | Constr4
```

AlgDataType 을 만드려면 4가지 방법이 생기는 것이다. Constr1, Constr2, Constr3, Constr4 을 이용해서 만들 수 있다. 각 생성자에 따라서 AlgDataType 값은 여러 값으로 표현될 수 있다. 예를 들면, Constr1 을 이용하면 Type11 중 하나, Type12 중 하나의 값으로 만들어질 수 있다.

주의! type과 데이터 생성자 이름의 첫 문자는 반드시 대문자로 쓰고 함수 이름을 포함한 변수는 소문자로 시작하도록 한다. 그렇지 않으면 Haskell 파서가 이게 생성자인지 변수인지 확인하는 복잡한 절차를 거칠거다. 우리 파서 도와주자.

## Pattern-matching
앞에서 Pattern-matching 어떻게 사용하지 보긴 봤는데, 이제 좀더 일반적인 케이스들을 보자. pattern-matching으로 타입의 많은 생성자중 특정 생성자인지 찾아 값을 분해할 수 있다. 어떤 생성자인지 찾는 것은 우리가 어떤 코드를 작성해야할지 결정할 수 있는 중요한 정보다.

```haskell
foo (Constr1 a b) = ...
foo (Constr2 a) = ...
foo (Constr3 a b c) = ...
foo Constr4 = ...
```

여기서 기능이 몇가지 더 있다.

1. An underscore `_` 는 어떤 것이든 매칭하는 "wildcard pattern" 이다.
2. A pattern of the form x@pat can be used to match a value against the pattern pat, but also give the name x to the entire value being matched. For example:
```haskell
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

*Main > baz brent
"The name field of (Person \"Brent\" 31 Sealing Wax) is Brent"
```

3. 패턴은 `nested` 가능하다. 예를 들면,
```
checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _) = n ++ ", your favorite thing is lame."

*Main > checkFav brent
"Brent, you're my kind of person!"
*Main > checkFav stan
"Stan, your favorite thing is lame."
```

## Case expressions
기본적으로 pattern-matching 으로 `case` 로 표현할 수 있다.
```haskell
case exp of
	pat1 -> exp1
	pat2 -> exp2
	...
```
exp 은 pat1, pat2 로 각각 매칭되며, 첫 번째 매칭되는 패턴이 선택된다.

```haskell
ex03 = case "Hello" of
		[] -> 3
		('H':s) -> length s
		_ -> 7
```

여기선 2번째 패턴이 매칭되고 끝는다. case 은 함수를 정의할 때 넘나 유용하다. 다른 예를 보자.

```haskell
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
					Failure -> 0
					OK d -> d
```

## Recursive data types
데이터 타입이 재귀적일 수 있다. 이미 재귀적인 타입을 본적있다. list!
```haskell
data IntList = Empty | Cons Int IntList
```

재귀적인 데이터 타입은 처리하기 위해 재귀적인 함수를 사용할 수 있다.
```haskell
intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l
```

다른 예를 보면, binary trees를 표현할 수 있다. 내부 노드에는 Int를 저장하고 leaf에 Char을 저장한다.

```haskell
data Tree = Leaf Char
		  | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
```