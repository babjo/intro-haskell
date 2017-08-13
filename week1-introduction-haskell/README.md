# Introduction to Haskell
## What is Haskell?
하스켈은 1980년도 후반에 만들어진 **lazy, functional** 프로그래밍 언어이다. 예전에 많은 lazy, functional 언어들이 넘쳐났는데, 사람들마다 각기 좋아하는 언어가 있었고 서로간에 의사소통이 어려웠다. 그래서 사람들이 모여서 기존 언어의 좋은 개념들을 모아 새로운 언어를 만들었고 그것이 바로 하스켈이다.

![Haskell logo](https://)

### Functional
![Function](https://)

`functional` 에 대한 정확한 의미는 없다. 다만 하스켈을 `functional` 언어라고 말할 때 2가지를 떠올린다.
- 함수는 `first-class`이다. 함수는 다른 값들을 사용하는 것처럼 사용할 수 있는 하나의 값이다.
- 하스켈 프로그램은 지시명령어를 수행하기보단 표현을 평가하는 것에 초점이 맞춰져있다.

### Pure
![Pure](http://)

하스켈 expressions 은 항상 *참조 투명*하다. 이 말인 즉슨,
- *mutation* (상태) 이 없다. 모든 것이 *immutable* (불변) 이다.
- expressions 는 `side effects` 가 없다.
    - side effects 는 전역 변수를 업데이트 한다던가, 화면에 변수를 출력한다던가...
- 같은 함수에 같은 인자를 넣으면 항상 결과는 같은 결과가 나온다.

아마... 이게 뭔말일가 싶을 것이다. 어떻게 mutation 이 잆이, side-effect 없이 뭔갈 할 수 있단 말인가... 평소 명령형, 객체지향 페러다임을 가진 사람이라면 생각의 전환이 필요하다. 만약 생각의 전환을 성공한다면, 다음과 같은 이득이 있다.

- Equational reasoning and refactoring: 하스켈에선 `replace equals by equals` 를 할 수 있다.
- Parallelism: 병렬로 실행하더라고 서로가 서로에게 영향을 미치지 않는 것을 보장할 수 있다. 쉽게 병렬로 만들 수 있다.
- Fewer headaches

### Lazy
![Lazy](http://)

하스켈에서 표현은 결과가 필요할 때까지 평가되지 않는다. 딱 필요할 때, 수행한다.

- 함수를 정의하는 것보다 새로운 컨트롤 구조를 정의하기 쉬워진다.
- `infinite` 데이터 구조를 만들 수 있다.
- 좀더 구성적인, 조합적인 프로그래밍 스타일을 구현할 수 있다.
- 그러나, 한 가지 않좋은 점은 시간과 공간 사용을 추론하는 것이 좀 더 복잡해진다.

### Statically typed
![Statically typed](http://)

모든 하스켈 표현은 타입을 가지며 모든 타입은 컴파일 타임에 체크된다. 타입 에러가 있는 경우 아예 컴파일 되지도 않는다.

## Themes
이번 코드에서 우리는 3가지 주제에 초점을 맞춘다.

### Types
- Helps clarify thinking and express program structure
하스켈 프로그램에서 첫 스텝은 보통 필요한 모든 타입을 적어내려가는 것이다. 하스켈의 타입 시스템은 매우 표현력이 좋으며 프로그램에서 누군가의 생각을 명확하게 하는데 있어서 엄청난 도움을 준다.

- Serves as a form of documentation
문서에서 한 글자도 읽기 전에 함수가 어떤 일을 하며 어떻게 사용하는지 함수 타입에서 알 수 있다. 이 또한 타입 시스템에 주는 장점이다.

- Turns run-time errors into compile-time errors
"If it compiles, it must be correct"

### Abstraction
"Don't Repect Yourself" 는 프로그래밍 세계에서 종종 들리는 격언이다. "Abstraction Principle"로 알려진 이 아이디어는 코드에서 발생할 수 있는 것(알고리즘, 생각, 데이터) 모든 것들이 반복되어서는 안된다는 의미다. 이렇게 반복되는 코드를 줄이는 과정을 `추상화` 하는 과정이다.

하스칼에선 parametic polymorphism, higher-order functions, type class 와 같은 기능들로 추상화를 매우 잘할 수 있다.

### Wholemeal programming
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

## Literate Haskell
