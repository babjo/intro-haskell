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
## Local definitions
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