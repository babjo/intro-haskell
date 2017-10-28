# Haskell Basics: Functions and Pictures
## Prelude
### What is Haskell?
하스켈은 1980년도 후반에 만들어진 **lazy, functional** 프로그래밍 언어이다. 예전에 많은 lazy, functional 언어들이 넘쳐났는데, 사람들마다 각기 좋아하는 언어가 있었고 서로간에 의사소통이 어려웠다. 그래서 사람들이 모여서 기존 언어의 좋은 개념들을 모아 새로운 언어를 만들었고 그것이 바로 하스켈이다.

![Haskell logo](http://www.cis.upenn.edu/~cis194/spring13/images/haskell-logo-small.png)

#### Functional
![Function](http://www.cis.upenn.edu/~cis194/spring13/images/function-machine.png)

`functional` 에 대한 정확한 의미는 없다. 다만 하스켈을 `functional` 언어라고 말할 때 2가지를 떠올린다.
- 함수는 `first-class`이다. 함수는 다른 값들을 사용하는 것처럼 사용할 수 있는 하나의 값이다.
- 하스켈 프로그램은 지시명령어를 수행하기보단 표현을 평가하는 것에 초점이 맞춰져있다.

#### Pure
![Pure](http://www.cis.upenn.edu/~cis194/spring13/images/pure.jpg)

하스켈 expressions 은 항상 *참조 투명*하다. 이 말인 즉슨,
- *mutation* (상태) 이 없다. 모든 것이 *immutable* (불변) 이다.
- expressions 는 `side effects` 가 없다.
    - side effects 는 전역 변수를 업데이트 한다던가, 화면에 변수를 출력한다던가...
- 같은 함수에 같은 인자를 넣으면 항상 결과는 같은 결과가 나온다.

아마... 이게 뭔말일가 싶을 것이다. 어떻게 mutation 이 잆이, side-effect 없이 뭔갈 할 수 있단 말인가... 평소 명령형, 객체지향 페러다임을 가진 사람이라면 생각의 전환이 필요하다. 만약 생각의 전환을 성공한다면, 다음과 같은 이득이 있다.

- Equational reasoning and refactoring: 하스켈에선 `replace equals by equals` 를 할 수 있다.
- Parallelism: 병렬로 실행하더라고 서로가 서로에게 영향을 미치지 않는 것을 보장할 수 있다. 쉽게 병렬로 만들 수 있다.
- Fewer headaches

#### Lazy
![Lazy](http://www.cis.upenn.edu/~cis194/spring13/images/relax.jpg)

하스켈에서 표현은 결과가 필요할 때까지 평가되지 않는다. 딱 필요할 때, 수행한다.

- 함수를 정의하는 것보다 새로운 컨트롤 구조를 정의하기 쉬워진다.
- `infinite` 데이터 구조를 만들 수 있다.
- 좀더 구성적인, 조합적인 프로그래밍 스타일을 구현할 수 있다.
- 그러나, 한 가지 않좋은 점은 시간과 공간 사용을 추론하는 것이 좀 더 복잡해진다.

#### Statically typed
![Statically typed](http://www.cis.upenn.edu/~cis194/spring13/images/static.jpg)

모든 하스켈 표현은 타입을 가지며 모든 타입은 컴파일 타임에 체크된다. 타입 에러가 있는 경우 아예 컴파일 되지도 않는다.

### Themes
이번 코드에서 우리는 3가지 주제에 초점을 맞춘다.

#### Types
- Helps clarify thinking and express program structure
하스켈 프로그램에서 첫 스텝은 보통 필요한 모든 타입을 적어내려가는 것이다. 하스켈의 타입 시스템은 매우 표현력이 좋으며 프로그램에서 누군가의 생각을 명확하게 하는데 있어서 엄청난 도움을 준다.

- Serves as a form of documentation
문서에서 한 글자도 읽기 전에 함수가 어떤 일을 하며 어떻게 사용하는지 함수 타입에서 알 수 있다. 이 또한 타입 시스템에 주는 장점이다.

- Turns run-time errors into compile-time errors
"If it compiles, it must be correct"

#### Abstraction
"Don't Repect Yourself" 는 프로그래밍 세계에서 종종 들리는 격언이다. "Abstraction Principle"로 알려진 이 아이디어는 코드에서 발생할 수 있는 것(알고리즘, 생각, 데이터) 모든 것들이 반복되어서는 안된다는 의미다. 이렇게 반복되는 코드를 줄이는 과정을 `추상화` 하는 과정이다.

하스칼에선 parametic polymorphism, higher-order functions, type class 와 같은 기능들로 추상화를 매우 잘할 수 있다.

#### Wholemeal programming
"함수형 언어는 wholemeal programming을 매우 잘 할 수 있다. Wholemeal programming는 크게 생각하는 것이다. 요소의 순서보다 전체 리스트를 생각하는 것, 개개인 솔루션보다 솔루션 공간을 개발하는 것, 하나의 길보다 그래프를 생각하는 것. 보다 크게 생각하는 것이다. 이런 wholemeal 접근은 주어진 문제에 새로운 관점 또는 통찰을 준다. 일단 좀더 general 문제를 풀며 general 프로그램을 특정 상황의 하나로 변형 시켜 주어진 문제를 해결한다."

예를 들면, C/Java 와 같은 언어에서 다음과 같은 슈도코드를 생각해보자.

```java
int acc = 0;
for (int i=0; i< lst.length; i++){
	acc = acc + 3 * lst[i];
}
```
이 코드는 프로그래머는 "indexitis" 이라는 고통(?)을 겪는다. 배열을 반복하는데 현재 인덱스를 추적하면서 반복하는 것, low-level에 신경을 쓰면서 코드를 작성해야한다. 또한 이 코드에는 두 가지 오퍼레이션이 섞여있다. 배열 내 원소를 3으로 곱하는 오퍼레이션과 그 결과를 모두 합치는 오퍼레이션이다.

하스켈은 다음과 같이 쓸 수 있다.

```haskell
sum (map (3*) lst)
```

### Programming environment

이 수업에서는 `CodeWorld`라는 개발 환경을 사용한다. 이 환경은 http://code.world/haskell 로 접근 가능하다. 이 환경의 장점?

- 설치 없이 웹에서 바로 해볼 수 있다.
- 통합 환경을 제공하여 그래픽 출력이 가능하다.

## Baisc Haskell
일단 간단한 코드로 시작해보자. [open on CodeWorld](https://code.world/haskell#PvoZv8n9Mtp5kOMGDszKEcQ)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

ourPicture :: Picture
ourPicture = blank

main :: IO ()
main = drawingOf ourPicture
```

자세한 설명은 나중에 할테니, 일단 `Run` 버튼을 눌러서 돌려보자. 아무것도 안뜬다...

### Declarations and variables
놀랄것없다. `ourPicture`에 `blank`를 넣어서 그럴뿐... 이를 바꾸기 위해 다음과 같이 해보자. 그럼 반지름이 1인 검은 원이 하나가 출력될 것이다.

```haskell
ourPicture = solidCircle 1
```

뭐가 일어난걸까? 이 코드는 `ourPicture`이라는 변수에 `Picture` 타입("::"는 "has type"을 의미한다.)을 선언하고 이 변수에 `circle 1`이라는 값을 정의한다. 이렇게 하면 `ourPicture` 값은 영원하다. 수정이 불가능하다. 한번 해보자.

```haskell
ourPicture = solidCircle 1
ourPicture = solidCircle 2
```

그럼 `Muliple declarations of 'ourPicture'`이라는 에러메세지가 뜰 것이다.

하스켈이서는 변수는 *not mutable boxes*; 그냥 값의 이름이다.

다른 의미로, `=`는 다른 언어처럼 *assignment*가 아니다. 하스켈에서 `=`는 수학에서 그런 것처럼 `definition`으로 보면 된다. 즉, `ourPicture = solidCircle 1`은 `ourPicture`에 `solidCircle`을 할당한다가 아닌, `ourPicture`에 `solidCircle 1`을 정의한다고 해야한다.

그럼 이 코드는 어떤가?

```haskell
y :: Integer
y = y + 1
```

### Picture Types and Functions
일반적인 언어들과는 다르게, numbers, booleans, tuples, lists, strings 으로 시작하지 않고 Picture로 시작했다. Picture은 언어 자체에 포함된 타입은 아니고 `CodeWorld`에 정의된 라이브러리 내 타입이다. 그렇다고 흥미가 떨어지는 것은 아니다. 사실 기본형 boolean 조차도 라이브러리 정의가 있다. 단지 표준 라이브러리 일뿐이다.

#### Primitive pictures
모든 picture는 `Picture` 타입의 값이다. 우리는 아까 `blank`와 `solidCircle 1` 두 가지 값을 보았다. 그외에 rectangles, polygons, lines, ars, text 등 많다. `Help` 버튼을 놀러 한번 참고바란다.

문서에서 `solidCircle`은 다음과 같이 정의되어있다.

```haskell
solidCircle :: Double -> Picture
```

`->`는 `solidCircle` 자체가 함수라는 것을 의미하며 `Double` 타입의 값을 `Picture` 타입의 값으로 바꾼다는 의미다. 그래서 `solidCircle 1`는 Picture의 값이 될 수 있다.

함수를 호출할 때, 인자 자체가 복잡하면 다음과 같이 괄호를 넣어야한다.

```haskell
ourPicture = solidCircle (1+1)
```

#### Modifying Pictures
이제 방금 그 picture에 색을 줘보자. API 문서를 보면 다음과 같은 함수가 있다.

```haskell
colored :: Color -> Picture -> Picture
```

와우 여긴 또 `->`가 두개 이다. 이건 또 뭔지... `colored`는 `Color`, `Picture`를 인자로 요구하는 함수다. 인자가 2개인 함수이다. 이미 color 타입으로 정의된 값으로 다음과 같이 쓸 수 있다.

```haskell
ourPicture = colored green (solidCircle 1)
```

너가 보다 싶이, 여러개 인자를 역시 함수 뒤에 인자를 명시하면 된다. 스페이스로 하나의 인자씩을 구분하며 역시 복잡한 인자의 경우 괄호가 필요하다.

그렇다면 이 두개의 `->`(화살표)는 뭘 처리하는 것인가? 왜 `Color Picture -> Picture`가 아닐까? 이건 지금은 조금 어색할 수 있지만 사실은 매우 깊고 아름다운 이유가 있다. 일단 몇주만 진도를 빼다보면 알 수 있을 것이다.

#### Composing Pictures
하나의 원만 보는데 질렸다. 좀더 재밌는걸 해보자. 일단 더 많이 그려보고 싶다. 그렇다면 우리가 찾아야할 함수는 두 개의 picture을 하나 조합하는 함수이다. 아마 그 함수의 타입은 `Picture -> Picture -> Picture` 일 것이다.

자, 지금부터 타입 시스템의 좋은 장점을 보게 될 것이다. 문서 상에 이런 함수는 딱 하나 있다.

```haskell
(&) :: Picture -> Picture -> Picture
```

좀 함수 이름이 이상하다. 그렇지만 함수 맞다. `(&)`도 `(&) picture1 picture2` 이렇게 사용할 수 있다. 그렇지만 `&`는 오퍼레이터로 두 picture 사이에 쓸 수 있다. 이렇게...

```haskell
ourPicture = colored green (solidCircle 1) & solidCircle 2
```

한번 출력보고 다음과 같은 결과를 얻을 수 있다.

- `&`는 오퍼레이터로 두 개의 picture를 하나의 picture로 조합할 수 있으며 오른쪽 인자 picture를 아래깔고 왼쪽 인자 picture가 그 위로 올라가 합쳐진다.
- `colored green` 함수는 `(solidCircle 1) & solidCircle 2`가 아닌 `(solidCircle 1)`에만 적용되었다. 즉, 하스켈 문법의 중요한 사실은 *Function application binds tighter than any binary operators* 라고 할 수 있다.

좀더 재밌는 그림을 위해 이제는 중앙에 말고 다른 곳에 그릴 필요가 있다. 이 함수를 이용하자.

```haskell
translated :: Double -> Double -> Picture -> Picture
```

이렇게 사용할 수 있다.

```haskell
ourPicture = colored green (translated 0 (-1.5) (solidCircle 1)) & colored red (translated 0 (1.5) (solidCircle 1))
```


여기서 -1.5 괄호를 한 것은 괄호를 하지 않으면 컴파일러가 - 오퍼레이터로 이해하기 때문이다. 코드가 한 줄에 너무 길어졌다. 적당히 이름을 줘서 여러 라인으로 나누자. [open on CodeWorld](https://code.world/haskell#PghsHgwl_rJUmdcCvOPAR-w)

```haskell
botCircleGreen = colored green (translated 0 (-1.5) (solidCircle 1))
topCircleRed = colored red (translated 0 1.5 (solidCircle 1))
frame = rectangle 2.5 5.5
trafficLight = botCircleGreen & topCircleRed & frame

ourPicture :: Picture
ourPicture = trafficLight
```

### Defining functions

자 이제껏 여러 함수를 보왔다. 이제는 직접 함수를 만들어보자. 여태 함수를 정의할 때 다음과 정의를 해왔다. `something` 함수를 정의할 때, `something = this and that` 식으로 말이다. 이 함수는 인자가 없는 함수이다. 여기서 인자를 넣고 싶으면 함수 이름 왼쪽에 넣어주면 된다.

```haskell
botCircle c = colored c (translated 0 (-1.5) (solidCircle 1))
topCircle c = colored c (translated 0 1.5 (solidCircle 1))
frame = rectangle 2.5 5.5
trafficLight = botCircle green & topCircle red & frame

ourPicture :: Picture
ourPicture = trafficLight
```

일반적인 신호등이라면 이렇게 초록색, 빨간색을 동시에 보여주지 않는다. 그럼 `trafficLight` 을 한번에 초록색 또는 빨간색을 보여주는 함수로 바꿔보자. 그럼 Boolean 타입의 값이 인자가 되어야한다. 이렇게 두 가지 경우로 작성할 수 있다.

```haskell
trafficLight True = botCircle green & topCircle black & frame
trafficLight False = botCircle black & topCircle red & frame
ourPicture = trafficLight True
```


이렇게 하면 컴파일러가 이 함수를 추론하여 타입을 할 수 있겠지만 우리는 좀더 코드를 깔끔하게 표현하기 위해 타입을 직접 명시해주자.

```haskell
trafficLight :: Bool -> Picture
```

우리는 일반적으로 프로그래밍에서 가장 중요하게 여기는 `추상화`를 적용했다. `topCircle` 함수와 `trafficLight` 함수를 추상화했다.

### Animations

지금 신호등은 초록색이지만, 본래 신호등은 초록색, 빨간색 계속 바뀐다. CodeWorld API는 이런 drawing API 뿐만 아니라 애니메이션 API도 제공한다. 애니메이션은 무엇인가? 시간마다 바뀌는 picture이다. 이 때 시간은 애니메이션 시작 이후 초(second)라고 할 수 있다.

명령형 언어에서는 drawing을 하면서 어딘가에서 `getCurrentTime()` 함수를 호출할 것이다. 그렇게하면 순수한 함수형 언어는 *hidden side effect*가 되므로 시간을 인자로 제공해주자.

자, 신호등이 3초마다 변하는 코드이다. [open on CodeWorld](https://code.world/haskell#Ph3vruxsOVmcnYG0D2NGG0Q)

```haskell
trafficController :: Double -> Picture
trafficController t
	| round (t/3) `mod` 2 == 0 = trafficLight True
	| otherwise                = trafficLight False

main :: IO()
main = animationOf trafficController
```

여기서 이런 것들을 알 수 있다.

- `drawingOf` 대신에 `animationOf`을 main 함수의 시작점으로 했다. 이제 인자가 더이상 `Picture` 타입의 값이 아니며 `Double -> Picture` 타입의 함수이다. 이 함수는 매 시간이 주어지며 picture를 반환한다.
- *guard* 를 이용하여 여러 케이스를 하나의 함수로 정의했다. guard의 조건를 순서대로 확인하면서 True 조건을 만나면 그 코드를 수행한다.
- `otherwise`는 그저 `True`로 정의된다. `True`로 명시하는 것보다 그냥 읽기에 좋다.
- 함수에 `round`와 `mod` 함수가 있다. `round`는 일반적인 함수처럼 사용되었지만, `mod`는 `(`mod`)`와 같은 형태로 양쪽에 \`를 넣어서 오퍼레이터로 만들어서 사용했다.

### Numerical Types

이제부터 숫자와 관련된 타입을 알아보자.:`Int`, `Integer`, `Double`

- `Int`는 machine-sized 정수이다.
```haskell
i :: Int
i = -42
```

`Int`는 하스켈에서 최대 -2^29~2^29까지를 보장하지만 수행되는 컴퓨터 아키텍쳐에 따라 조금씩 다르다. 예를 들면 64-bit 머신에서는 -2^63~2^63이다.

- 반면, `Integer` 타입은 머신의 메모리만 가능하다면 제한이 없다.
```haskell
n :: Integer
n = 1234567890987654321987340982334987349872349874534

reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))

numDigits :: Int
numDigits = length (show reallyBig)
```

위 코드를 보면 `numDigits`가 `19729`이다. 천자리의 자릿수의 숫자를 아무 문제 없이 다를 수 있다.

- floating-point numbers는 `Double`이다.
```haskell
d1, d2 :: Double
d1 = 4.5387
d2 = 6.2831e-4
```

single-precision floating point number 타입으로 `Float`도 있다.

당연히 숫자이니 다음과 같은 오퍼레이션이 있다.

- (+), (-), (\*) 는 모든 숫자 타입에서 동작한다.
- (/) 는 `Double`에서만 동작한다.
- 정수를 나눌 때는 `div`와 `mod`가 있다.
- `sin`, `cos`, `log`, `sqrt` 모두 있다.

같은 오퍼레이션에 다른 타입을 섞을 수는 없다. 만약 하고 싶다면 명시적으로 타입을 변환해야한다.

- `fromIntegral`: 어떤 정수 타입을 다른 타입을 으로 변환하는 함수
- `round`, `floor`, `ceiling`: floating-point numbers를 `Int` 또는 `Integer`로 변환하는 함수

타입 변환을 암묵적으로 해주는 언어를 사용했다면, 이런 타입 변환에 빡이 칠 수 있겠지만 하다보면 이렇게 타입 변환 명시적으로 하는 것에 대해 감사함을 느낄 것이다. 암묵적인 변환을 하는 곳은 숫자를 다루는 코드를 너무 어설프게 짜게 만든다.

위 코드에서 `(==)`를 사용했는데, 이는 동일한 타입의 값을 비교하는 함수이다. 대부분의 타입에서 동작하며 심지어 floating point numbers에서도 동작하지만 사용하진 말아야한다. ([왜?](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)) 서로 다른 값인지 확인은 `(/=)`이다.

그 뿐만 아니라 비교를 위한 `(<), (>), (<=), (>=)` 것들이 있고 `min`, `max`도 있다.

## Recursion
오늘 마지막 주제는 재귀이다. 재귀는 어떤 함수가 자기 자신을 다시 호출하는 것이다.

재귀는 추상화와 같이 함수형 프로그래밍에서 매우 강력한 도구이다. 이 코스만 지나면, 이 세계에서 재귀를 생각하는 것이 자연스러워질거다.

자, 그럼 여러개의 신호등을 만들어보자. 이런 방식으로 할 수 있다. [open on CodeWorld](https://code.world/haskell#PNApyEJW7C_zshSxdde3eIw)

```haskell
lights :: Integer -> Picture
lights 0 = blank
lights n = trafficLight True & translated 3 0 (lights (n - 1))

ourPicture = lights 3

main = drawingOf ourPicture
```

`lights` 함수는 n(그릴 신호등 개수)에 따라 동작한다. 만일 0이면 아무것도 그리지 않는다. 0이 아니면 하나의 light를 그리고 오른쪽에 나머지 신호등을 그린다.

아무것도 하지 않는 base case와 recursive cases, 이런 형식은 전형적인 재귀 형식이다.

그런데 만약에 `lights (-1)`이 일어나면 어떻게 될까? (직접 넣어보니 컴파일 중 에러가 발생한다.)

여기서 `lights` 함수에 추상화가 가능하다. 그릴 picture과 이 picture을 여러 개를 그릴 때 간격을 어느정도로 할지 인자로 받도록 할 수 있다.

```haskell
spread :: Picture -> Double -> Integer -> Picture
spread pic dx 0 = blank
spread pic dx n = pic & translated dx 0 (spread pic dx (n-1))

ourPicture = spread (trafficLight True) 3 4
```

잠깐 참고: 이 코드에서 컴파일러는 `pic`이랑 `dx`가 사용되지 않는 변수라고 경고를 날릴 것이다. 그러니 이 처럼 사용하지 않는 변수에 대해선 `_`를 사용하도록 하자. 이는 파라미터에 이름을 붙이지 않는다는 의미다.

재귀 함수는 자기 자신을 한번 이상 반복적으로 호출한다. 그래서 이런 나이스한 그림도 그릴 수 있다. [open on CodeWorld](https://code.world/haskell#Pt35LixP8QB5edtSH06YjAA)

```haskell
tree :: Integer -> Picture
tree 0 = blank
tree n = path [(0,0),(0,1)] & translated 0 1 (
    rotated (pi/10) (tree (n-1)) & rotated (- pi/10) (tree (n-1)))

main = drawingOf (tree 8)
```

다음 시간에 저 `[x, y]`, `(x, y)` 형식이 무잇인지 배운다. 지금은 이렇게 한 점에서 또 다른 한점으로 라인을 그린다는 것을 참고해라.

(여기서 나무가 접혔다 펴졌다하는 것을 보고 싶으면 좀더 코드를 추가하면 된다. [open on CodeWorld](https://code.world/haskell#PrrDoZkX8T-a3E9x0rXVydQ)) 

## Code Comments

뭔가 재밌는건 아니지만, 알아두면 좋은 것이다. 하스켈에선 이렇게 두 가지 방법으로 주석을 남길 수 있다.

```haskell
-- Two hyphens, until the end of the line
{- or enclosed
   in curly brace/hyphen pairs, which may span multiple lines
 -}
```

## A word about error messages

**에러 메세지를 무서워하지 말라!**

GHC의 에러메세지는 다소 길고 무서울 수 있다. 많은 유용한 정보를 담고 있어서 그런 것이니 쫄 필요 없다. 예를 들면 다음과 같다.

```
Prelude> 'x' ++ "foo"

<interactive>:1:1:
    Couldn't match expected type `[a0]' with actual type `Char'
    In the first argument of `(++)', namely 'x'
    In the expression: 'x' ++ "foo"
    In an equation for `it': it = 'x' ++ "foo"
```

일단 첫번째 메세지 `Couldn't match expected type '[a0]' with actual type 'Char'` 는 *뭔가*가 list 타입으로 기대했는데, Char 타입을 가졌다는 것이다. 그리고 다음 라인, `(++)`의 첫 번째 인자에 문제가 있다는 의미이다. 그리고 다음 라인을 보면, 좀더 문맥이 드러나 무엇이 문제 였는지 알 수 있다. 첫 번째 라인 분명히 'x'는 Char 타입이라고 했다. 그럼 왜 이것이 list 타입이어야할까? 사실 `(++)`의 첫 번째 인자는 list 타입이어야하기 때문이다. 이런 식으로 문제를 찾을 수 있다.

거대한 에러메세지를 받았을 때, 도망치고 싶다는 감정을 이겨내야한다. 깊게 숨을 쉬고 천천히 읽어라. 그리고 전체를 이해할 필요는 없다. 단지 문제가 무엇인지 이해할 정도의 정보만을 읽어라.