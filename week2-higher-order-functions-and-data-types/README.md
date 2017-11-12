# Higher Order Functions
저번 주 과제의 솔루션이다. row 를 draw 한 뒤 column 를 draw 한다.
```haskell
pictureOfMaze :: Picture
pictureOfMaze = drawRows (-10)

drawRows :: Integer -> Picture
drawRows 11 = blank
drawRows r  = drawCols r (-10) & drawRows (r+1)

drawCols :: Integer -> Integer -> Picture
drawCols _ 11 = blank
drawCols r c = drawTileAt r c & drawCols r (c+1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt = …
```

분명 여기에는 반복되는 것이 있다. `drawRows`라던가... `drawCols`라던가 매우 비슷하다. 코드 보자면, "매번 **어떤 함수** 를 21번 호출되지만 호출 횟수에 따라 바뀐다.". `drawRows` 함수와 `drawCols` 함수는 두 가지 측면에서 다르다.
- 그 **어떤 함수**가 다르다. `drawRows`는 `drawCols`이고 `drawCols`는 `drawTileAt`이다.
- `drawCols`의 `drawTileAt`은 `drawRows`의 `drawCols`과 달리 파라미터가 하나 더 있다.

하스켈은 추상화에 강한 언어며 이 패턴 또한 추상화 가능하다. 한번 해보자.

```haskell
draw21times somthing = helper something (-10)

helper somthing 11 = blank
helper somthing n  = something & helper something (n+1)

pictureOfMaze = draw21times drawRow
drawRow = draw21times drawCol
drawCol = drawTileAt ? ?
```

음... `drawTileAt`에서 row 와 column 숫자가 필요한데 값을 가져올 수 없다. 그래서 `helper`가 `something`으로 `n`을 넘겨주는 것이 필요하다. 간단히 말해서 `drawRow`는 `drawCol`에게 어떤 raw가 그려져야하는지 알려줘야한다는 것이다.

```haskell
draw21times something = helper something (-10)

helper something 11 = blank
helper something n  = something n & helper something (n+1)

pictureOfMaze = draw21times drawRow
drawRow r = draw21times (drawCol r)
drawCol r c = drawTileAt r c
```

좋다. 이제 동작한다. 그렇지만 깔끔히 마무리 짓기 위해 타입을 명시해주자. CodeWorld interface가 제공해주는 타입은 이러하다.

```haskell
draw21times :: forall a. (Eq a, Num a) => (a -> Picture) -> Picture
```

그런데 이는 너무 복잡하니, 좀 더 간단히 작성해보자.
```haskell
draw21times :: (Integer -> Picture) -> Picture
helper :: (Integer -> Picture) -> Integer -> Picture
drawRow :: Integer -> Picture
drawCol :: Integer -> Integer -> Picture
```

`draw21times` 타입은 화살표가 두 개지만 2개의 인자를 갖는다는건 아니다. 첫 화살표는 괄호 안에 있어 함수를 의미하며 하나의 인자다.

그러니 `helper`는 인자가 2개이다. 첫 번째 인자는 함수이며 두 번째 인자는 `Integer`이다.

`drawRow`의 타입은 `draw21times` 함수의 첫 번째 타입과 같다. (그래서 `drawRow` 함수를 인자로 넘길 수 있다.)

이게 바로 `Higher Order Function`이다. 함수가 다른 함수를 인자로 받을 수 있는 것이다. 이건 함수 프로그래밍의 핵심적이 아이디어며 좋은 추상화를 가능케하는 중요한 요소이다.

## Partial application
`drawCol` 사용법에 조금 이상한 점이 있다. `drawCol`이 *두 개* 인자의 함수로 보이지만 `draw21times` 함수에서 `drawCol`를 인자 하나짜리 함수로 남들어 사용했다. 니는 어떻게 동작할까?

이해를 위해, `drawCol` 타입을 다르게 써보았다.
```
drawCol :: Integer -> (Integer -> Picture)
```

위 `drawCol`은 *하나*의 인자를 받아서 하나의 인자를 입력받는 함수를 반환한다. 이 반환된 함수는 `draw21times` 인자의 딱 받는 함수다.

자, 여기서 관점의 변화가 생긴다. 비록 `drawCol` 타입을 다르게 작성한 타입이지만 `drawCol :: Integer -> Integer -> Picture` 와 완전하게 동일한 타입이다. 그렇다. 하스켈에선 모든 함수는 하나의 인자만을 가진다. 그리고 입력 인자가 남았다면 남은 인자를 받을 수 있는 함수를 반환한다.

또한 함수 호출에서도 이점을 볼 수 있다. `f x y`는 `(f x) y`과 동일한 표현이다.

정리하면 함수에서 `type` 화살표는 *right associative(오른쪽 연관)*이며 함수 호출에서는 *left associative(왼쪽 연관)*이다.

## Local definitions
코드를 좀 정리하자. 먼저 `drawCol r c = drawTileAt r c`는 다소 불필요해보니다. 그저 `drawCol`를 사용하는 것은 `drawTileAt`과 동일하다. 그러므로 `drawCol`을 제거하자.

다음 코드에서 나는 악취는 `helper` 함수이다. `draw21times` 안에서만 사용되는 유용한 함수이다. 그렇기에 `draw21times`에 지역 정의하여 사용하자. 2가지 방법이 있다.

1. `let`:
```haskell
draw21times :: (Integer -> Picture) -> Picture
draw21tiems something = 
  let helper :: (Integer -> Picture) -> Integer -> Picture
	helper something 11 = blank
	helper something n  = something n & helper something (n+1)
  in helper something (-10)
```

`let`은 top-level 정의처럼 함수와 변수에 대한 정의를 가진다. 그리고 여러개의 정의를 가질 수 있으며 각 정의가 서로를 사용할 수 있도록 만들 수도 있다. `let`는 자기자신을 포함하는 표현이며 어디에서나 사용할수 있다.

2. `where`:
```haskell
draw21times :: (Integer -> Picture) -> Picture
draw21times something = helper something (-10)
  where
    helper :: (Integer -> Picture) -> Integer -> Picture
    helper something 11 = blank
    helper something n  = something n & helper something (n+1)
```

그저 문법만 바뀌었을 뿐 위 코드와 동일 내용이다. 하나 또는 그 상의 함수 정의를 `where`에서 할 수 있으며 이를 다른 정의에서나 함수의 내에서 사용할 수 있다.

자 다시 돌아와서 중요한 것은 이 둘 중에 어떤 스타일을 사용하냐이다. 일반적으로 `where`가 좀더 깔끔하며 들여쓰기를 덜하며 거의 지역 함수에서 항상 사용된다. 다만 지역 변수에 경우엔, `let`를 잘 사용한다.

보통 지역 정의 이름은 간단하게 `go`로 많이 사용한다. 그리고 타입 명시의 경우 종종 빠뜨리기도 한다. 그냥 설명이 필요없이 함수 라인도 몇줄안되고 작고 명확하기 때문이다.

## Captured variables
## A note on indentation
## Lambda expressions

# Data types
## Booleans
## More data types for Sokoboban

# Pure Interaction
## Interaction on CodeWorld
## Events
## Some terminology