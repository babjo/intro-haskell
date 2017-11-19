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
`drawCol` 사용법에 조금 이상한 점이 있다. `drawCol`이 *두 개* 인자의 함수로 보이지만 `draw21times` 함수에서 `drawCol`를 인자 하나짜리 함수로 남들어 사용했다. 이는 어떻게 동작할까?

이해를 위해, `drawCol` 타입을 다르게 써보았다.
```
drawCol :: Integer -> (Integer -> Picture)
```

위 `drawCol`은 *하나*의 인자를 받아서 하나의 인자를 입력받는 함수를 반환한다. 이 반환된 함수는 `draw21times` 인자의 딱 받는 함수다.

자, 여기서 관점의 변화가 생긴다. 비록 `drawCol` 타입을 다르게 작성한 타입이지만 `drawCol :: Integer -> Integer -> Picture` 와 완전하게 동일한 타입이다. 그렇다. 하스켈에선 모든 함수는 하나의 인자만을 가진다. 그리고 입력 인자가 남았다면 남은 인자를 받을 수 있는 함수를 반환한다.

또한 함수 호출에서도 이점을 볼 수 있다. `f x y`는 `(f x) y`과 동일한 표현이다.

정리하면 함수에서 `type` 화살표는 *right associative*(오른쪽 연관)이며 함수 호출에서는 *left associative*(왼쪽 연관)이다.

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

이렇게 지역 함수를 만드는 것에는 또 다른 장점이 있다. `draw21times`으로 들어오는 인자를 지역 함수 내에서 참조할 수 있다. 이전 코드를 보면 `something` 인자를 받아서 `helper`로 넘겨주는데, 그럴 필요가 없다.

```haskell
draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go 11 = blank
    go n  = something n & go (n+1)
```

## A note on indentation

하스켈에서 들여쓰기(indentation)는 중요하다. 하스켈 컴파일러는 `where`절에서 `where`절의 끝을 어떻게 구분할까? 들여쓰기에 있다. 몇몇 다른 언어와 달리 하스켈은 들여쓰기(indentation)에 매우 민감하다. 그로 얻는 2가지 장점이 있다.
- 코드를 보다 명확하고 읽기 쉽게 만든다.
- 프로그래머가 적절히 들여쓰기를 하도록 만든다.

들여쓰기 규칙에 대해서는 다소 기술적이어서 이 지면에서 설명하진 않는다. 그 대신에 [여길](https://en.wikibooks.org/wiki/Haskell/Indentation) 참조하길

## Lambda expressions

함수를 정의하는 3가지 방법을 이미 보았다. 전역에서 정의하거나 `let` 구문으로 정의하거나 `where` 절로 정의하거나. 이 3가지 정의하는 방법에는 반드시 함수의 이름을 붙여줘야한다.

그런데 함수에 이름을 붙이는 것이 굳이 필요없는 경우도 있다. 물론 다 붙일 수 있겠지만 이름을 짓는 다는건 어려운 일이다. 그래서 아주 작은 함수 경우엔 이름을 굳이 붙이긴 그렇고 그냥 우리가 사용할 곳에 정의만 해주고 싶을 때, `lambda`를 이용할 수 있다.

`drawRow` 함수를 보자. 이 함수가 하는 것은 그저 `drawTileAt`을 인자로 하여 `draw21times`를 호출하는 것이다. 그러니 `pictureOfMaze`에서 `drawRow` 대신에 바로 `lambda`로 정의해보자.

```haskell
pictureOfMaze = draw21times (\r -> draw21times (drawTileAt r))
```

여기서 `\`(backslash)가 문자 λ(lambda)를 표현하며 익명, 로컬함수를 정의한다는 것을 의미한다. 그리고 이는 `r` 이라는 첫번째 인자를 받으며 `->` 다음 코드를 호출하여 결과를 리턴한다는 의미다.

여기서 `lambda`를 한번 더 사용하여 좀 더 코드를 명확히 할 수 있다.

```haskell
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt r c))
```

고차 함수, 지역함수, 람다 표현은 매우 간결하면서 읽기 좋은 코드를 만든다. 사용해보길...

# Data types

다음 챕터에서 저번주 과제 코드 상에서 몇몇 문제점을 다루면서 동기를 얻고 데이터 타입을 넣어 볼 예정이다.

`drawTile`과 `maze` 함수는 `number`에 따라 타일의 타입을 지정한다. 그런데 이것에 문제가 있다. `maze`에서 받은 x, y 값으로 타입을 결정하여 화면을 만들기는 쉬운데, 이렇게 숫자로 관리하는 것은 숫자가 어떤 타입을 가지는지 잊어버리기가 쉽다.

그래서 `Integers`는 타입을 표현하는데 좋지가 않은 문제점이 있다. 너무 암시적이다.

그래서 명시적으로 타입을 표현하고 싶으며 할 수 있는 방법이 있다.

```hasekll
data Tile = Wall | Ground | Storage | Box | Blank
```

`data`는 새로운 타입의 이름을 정하는 예약어이다. 타입과 생성자 이름은 보통 대문자로 시작하며 각각 그들만의 네임스페이스를 가진다.

새로운 타입 `Tile`에는 5개의 생성자가 있으며 이는 각각 이 타입의 값들을 의미한다. 아까 전보다 훨씬 명확해졌다.

다시 `maze`로 돌아가서 `drawTile`에서 pattern match를 보자.

```haskell
drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

maze :: Integer -> Integer -> Tile
maze x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
```

`drawTile`의 타입과 `maze`의 타입에서 `Integer`가 없어지고 `Tile` 타입이 들어오면서 좀더 명확해졌다.

## Booleans

저번주에 사실 데이터타입을 이용한적이 있다. `Bool` 타입인데, 이는 `True`와 `False`를 값으로 가진다. 이전에서 새로운 타입을 정의한 것처럼 `Bool`도 동일한 매커니즘으로 동작한다.

```haskell
data Bool = False | True
```

## More data types for Sokoboban

이제는 애니메이션과 인터렉션할 수 있도록 만들어보자. 먼저 우리가 할 수 있는 인터렉션은 뷰를 4가지 방향 중 하나로 움직이는 인터렉션으로 이렇게 새로운 타입을 정의 할 수 있다.

```haskell
data Directoin = R | U | L | D
```

그리고 몇번 인터렉션한 후 현재의 위치를 저장할 곳이 필요하다. 이 위치는 좌표로 두개의 정수로 표현할 수 있으며 이렇게 타입으로 정의할 수 있다.

```haskell
data Coord = C Integer Integer
```

`Coord`를 새로 정의했다. `C`라는 생성자 하나만을 가지는데 이는 두 개의 정수를 인자로 받는다. (Integer -> Integer -> Coord)

사용하는 방법이다.

```hasekll
initialCoord :: Coord
initialCoord = C 0 0
```

`Coord`를 어떻게 사용할까? 이를 이렇게 사용할 수 있다.

```haskell
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic
```

이렇게 함수인자로 타입을 넘길 때 괄호로 패턴 매칭할 인자르 명시하여 인자를 넘길 수 있다.

다음 함수는 사용자의 인터렉션 값을 받으면 서로운 좌표를 만드는 함수다. 기존 좌표 값을 받아 방향에 따라 좌표를 새로 만든다.

```haskell
adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)
```

자 지금까지 그림 하나 없이 코딩만 했다. 아래 코드를 작성하고 화면을 확인하자. (CodeWorld)[https://code.world/haskell#PAp1po1qT2i8Dmim1XCGfWw]

```haskell
someCoord :: Coord
someCoord = adjacentCoord U (adjacentCoord U (adjacentCoord L initialCoord))

main = drawingOf (atCoord someCoord pictureOfMaze)
```


# Pure Interaction

자 이제는 화살표 키를 사용해서 미로를 이동해야한다. 이미 거의 모든 기능을 구현했지만 여기서 어떻게  `side-effect` 없이 인터렉션을 구현할 수 있을까? 어떻게 `mutable` 변수 없이 현재 상태를 기억할 수 있을까?

이전에 애니메이션을 구현할 때, 이 수수께끼를 풀었다. 순수 함수로 우리의 생각을 모델링한 다음 이 순수함수를 원하는 효과로 실행하는 `machinery`를 만들었었다.

인터렉티브한 프로그램은 새로운 입력을 받을 때마다 상태를 변경한다. 현재 상태를 기억하는 로직에서 상태를 변화시키는 로직을 분리하는 경우, 상태를 변화시키 로직은 다시 주어진 입력 값과 현재 상태를 넣으면 새로운 상태를 반환하는 순수 함수가 된다. 추가적으로 최초의 상태를 정의해주고 그 상태를 화면에 어떻게 그릴지 지정하는 것도 필요하다. 

## Interaction on CodeWorld

이 기능은 CodeWorld에서 제공한다.

```haskell
interactionOf :: world ->
                (Double -> world -> world) ->
                (Event -> world -> world) ->
                (world -> Picture) ->
                IO ()
```

위의 `world`이라는 타입은 어떤 타입을 특정하는 것이 아니고 그냥 타입 변수이다. 물론 나중에 다루겠지만, 지금 알아야할 것은 이 `world`가 어떤 타입이든 가능하는 점이다. 이 `world`를 통해 프로그램을 상태를 나타내자.

`interactionOf`는 4가지 인자를 받는다.
1. 최초 상태
2. 주어진 시간 값으로 상태를 변화하는 함수
3. 주어진 이벤트 값으로 상태를 변화하는 함수
4. 현재 상태를 그리는 함수

`Coord`로 이 함수를 사용하는 방법은 보면, (CodeWorld)[https://code.world/haskell#PpjfIR2NrgPeBJQKfg_63Kg]

```haskell
main = interactionOf initialCoord handleTime handleEvent drawState

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

handleEvent :: Evnet -> Coord -> Coord
handleEvent e c = adjacentCoord U c

drawState :: Coord -> Picture
drawState c = atCoord c pictureOfMaze
```

마우스를 화면상에서 움직이면 미로가 올라가는걸 볼 수 있다. 마우스 오버도 하나의 이벤트로 받아서 올라가는 것이다.

## Events

그래서 `Event` 타입이 무엇인지 알아볼 필요가 있다. 문서를 보면 이는 데이터 타입이다. 우리가 잘 아는 데이터 타입!

```haskell
data Event = KeyPress Text
           | KeyRelease Text
           | MousePress MouseButton Point
           | MouseRelease MouseButton Point
           | MouseMovement Point
```

5개의 서로 다른 이벤트가 생성자로 있다. `KeyPress` 이벤트는 `Text`을 인자로 받으며 사용해본적은 없지만 대략 감이 온다. (CodeWorld)[https://code.world/haskell#Px4XRGfE1Aw0GCEDKSDVAAw]

```haskell
handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = adjacentCoord R c
    | key == "Up"    = adjacentCoord U c
    | key == "Left"  = adjacentCoord L c
    | key == "Down"  = adjacentCoord D c
handleEvent _ c      = c
```

이제 화샬표 키로 미로를 움직일 수 있다.

## Some terminology

- 데이터 타입에서 인자가 없는 생성자는 *enumeration type*이라 한다.
- 데이터 타입에서 단 하나의 생성자만 가지면 *product type*이라 한다.
- 데이터 타입에서 여러 생성자를 가지면 *sum type*이라 한다.
- 데이터 타입에서 생성자가 하나도 없으면 *empty type*이라 한다.