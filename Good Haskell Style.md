프로그래밍은 매우 소셜 아트이다. 모든 예술가가 그렇듯, 이런 제약이 더 나은 결과를 만든다.

- 함수와 변수 이름은 `camelCase`를 사용하라.
	- **Variant**: 지역 변수만 `ids_with_underscores`을 사용하라.

- 함수 이름은 적당한 길이로 함수를 설명 할 수 있어야한다.
	- Good: solveRemaining.
	- Bad: slv.
	- Ugly: solveAllTheCasesWitchWeHaven'tYetProcessed.

- 탭을 사용하지 마라.
하스켈은 레이아웃에 민감하고 탭은 모든 것을 망친다. 그렇다고 스페이스를 계속 치고 있으란 것은 아니다. 아마 사용하는 에디터에 탭을 스페이스로 자동으로 바꿔주는 기능이 있을 것이니 그것을 사용하라.

- 적당한 길이의 라인을 유지하라.
- 모든 top-level 함수엔 타입 시그니쳐를 명시하라. 타입 시그니쳐를 명시하여 의도를 정확히 전할 수 있다. 그리고 또 top-level의 타입 시그니쳐는 좋은 에러 메세지를 만든다. 만일 타입 시그니쳐가 없고 타입 에러 발생시 진짜 문제가 발생한 곳부터 멀리 떨어진 곳에서 나타날 것이다. 그러니 명확한 타입 명시는 타입 에러에 도움이 된다.

	- 지엽적으로 정의된 함수와 상수는 타입 시그니쳐가 필요하지 않지만, 명시한다고 해서 나쁠 것 없다.

- 모든 top-level 함수 위에 이 함수가 어떤 일을 하는지 설명하는 주석을 작성하라.

- `-Wall` 을 사용하라. 사용하려면 `-Wall`을 커맨드라인으로 `ghc`넣는 방법 또는 `{-# OPTIONS_GHC -Wall #-}`를 `.hs`파일의 첫줄에 명시하는 방법이 있다.

- 최대한 프로그램을 한가지 일을 하는 작은 함수들로 쪼개라. 그리고 이 함수들을 조합하여 더 복합한 함수를 만들어라.

- 모든 함수가 *total*이 되도록 만들어라. 그러면 그 함수들은 모든 입력에 대해 합당한 결과를 줘야한다.