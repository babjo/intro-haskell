# Higher-order programming and type inference

## Anonymous functions

정수 배열에서 100 이상인 인자만 남기는 함수를 만들어보자.
```haskell
greaterThan100 :: [Integer] -> [Integer]
greaterThan100 [1,9,349,6,907,98,105] = [349,907,105]
```

우리가 알고 있는 좋은 방법은1

```haskell
gt100 :: Integer -> Bool
gt100 x = x > 100

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter gt100 xs
```

음... `gt100` 라고 이름을 줬지만, 다시는 사용하지 않을 예정이다. 그 대신 우리는 `lambda abstraction`으로 알려진 익명 함수를 사용할 수 있다.

```haskell
greaterThan100_2 :: [Integer] -> [Integer]
greaterThan100_2 xs = filter (\x -> x > 100) xs
```

`\x -> x > 100` 는 인자 x 하나를 입력 받고 x 가 100 이상인지 아닌지 판별하는 함수다.

물론 여러개의 인자를 입력으로 받을 수 있다.

```haskell
Prelude > (\x y z -> [x, 2*y, 3*z]) 5 6 3
[5, 12, 9]
```

그러나, `greaterThan100` 같이 특수한 경우엔, `lambda abstration` 없이 더 쉽게 쓸 수 있다.

```haskell
greaterThan100_3 :: [Integer] -> [Integer]
greaterThan100_x xs = filter (x>100) xs
```

`(>100)` 는 `operator section` 이다. `?`가 오퍼레이션으면, `?y` 는 `\x -> x ? y` 와 같다. 그리고 `y?` 는 `\x -> y ? x` 과 같다. 그렇기 때문에, 인자가 하나의 경우 이렇게 표현할 수 있다.

```haskell
Prelude> (>100) 102
True
Prelude> (100>) 102
False
Prelude> map (*6) [1..5]
[6, 12, 18, 24, 30]
```

### Function composition
읽기 전에, `(b -> c) -> (a -> b) -> (a -> c)` 타입의 함수는 어떻게 쓸까?

함해봐라

```
foo f g = ...
```

`...` 대신에 우리는 `a -> c` 함수 타입을 쓸 필요가 있다. 그리고 `lambda abstraction` 으로 표현하면

```
foo f g = \x -> ...
```

`x` 는 타입 `a`일거고 `...` 에서 타입 `c`를 표현해야한다. 우리는 `a`에서 `b`로 바꿔주는 `g` 와 `b` 를 `c`로 바꿔주는 `f`가 있다. 다시 정리하면 다음과 같다.

```haskell
foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g = \x -> f (g x)
```

자, `foo` 의 요점은 뭘까? 뭔가 유용한 것을 하는가? 걍 타입이 잘 동작하는지 보는 예제인가? `foo` 는 `function composition` 를 표현한 것이며 `.` 이다. 즉, `f` 와 `g` 가 함수면 `f . g`로 함수를 합성할 수 있다.

함수 합성로 코드를 간략하고 우아하게 짤 수 있다. 함수를 조합하여 데이터를 연속적으로 변형해나가는 것은 `wholemeal` 스타일에 딱 맞는다.

예를 들면,

```haskell
myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))
```

이 예제를 다음과 같이 다시 작성 할 수 있다.

```haskell
myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100
```

이 버전이 좀 더 명확하다. 실제로 `.`를 ghci에서 타입을 확인해보면,

```haskell
Prelude> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
```

### Currying and partial application
인자를 여러개 갖는 함수에서 타입이 좀 이상한 것을 기억하는가?

```haskell
f :: Int -> Int -> Int
f x y = 2*x + y
```

이렇게 만드는데는 깊은 이유가 있는데 드디어 밝일 시간이다. 사실 하스켈의 모든 함수는 단 하나의 인자만 입력으로 받는다. !?!?! 저기 `f`함수는 인자를 2개 받는게 아닌가? 사실은 이 함수는 인자를 하나 받고 함수 (Int -> Int) 를 반환하는 함수다.


```haskell
f' :: Int -> (Int -> Int)
f' x y = 2*x + y
```

function arrow 은 오른쪽 연관이다. `W -> X -> Y -> Z` 는 `W -> (X -> (Y -> Z))` 이다. 여기서 활호를 우리가 추가하거나 삭제할 수도 있다.

다음으로 Function application 는 왼쪽 연관이다. `f 3 2` 는 `(f 3) 2` 와 같다.

인자가 여래개인 lambda abstraction 는

```haskell
\x y z -> ...
```

사실 다음 코드의 문법 설탕이다.

```haskell
\x -> (\y -> (\z -> ...)).
```

이와 같이 함수는

```haskell
f x y z = ...
```

이 코드의 문법 설탕이다.

```haskell
f = \x -> (\y -> (\z -> ...)).
```
