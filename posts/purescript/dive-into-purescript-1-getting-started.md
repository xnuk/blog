---
title: Dive into PureScript - Getting Started
lang: ko
---

뭔가 만들긴 해야겠습니다. 제가 만들어야 할 목록 중에 테트리스 시뮬레이터라는 것이 있는데, 간단히 말하자면 그냥 테트리미노가 떨어지는 속도가 0이라서 블록 위치를 확정짓기 전까지 마음대로 돌려보고 벽차기도 할 수 있는 걸 만들려 했습니다. 다 만들지는 모르겠지만 무튼 그런 걸 만들려고 합니다. [Elm](http://elm-lang.org/)은 써봤고 테트리스 짜기에는 적합합니다만 언어 디자인이 약간씩 나사가 빠져 있는 그런 언어라 (아 물론 생 자바스크립트보다야 Elm이 낫습니다.)  좀 더 Haskell에 가깝다는 [PureScript](http://www.purescript.org/)를 써보기로 했습니다. 이하는 그 삽질기입니다.



## PureScript?

[PureScript 홈페이지](http://www.purescript.org/)를 들어가면 그 진상을 알 수 있습니다. PureScript는:

- 손쉽게 기존 자바스크립트 코드를 끌어다 쓸 수 있고
- Type class가 존재하기 때문에 전역 함수의 이름 충돌을 줄여 더욱 깔끔하게 쓸 수 있으며
- do notation이 존재하여 필요할 땐 코드를 완전히 절차형처럼 쓸 수 있고
- 패키지 시스템을 Bower에 기대어 사용하기 때문에 손쉽게 패키지를 추가할 수 있습니다.

이 넷은 Elm엔 없는 기능입니다. 부차적인 것을 보면, PureScript의 Hello, world! 코드는 다음과 같습니다:

```haskell
import Prelude
import Control.Monad.Eff.Console (log)

greet :: String -> String
greet name = "Hello, " <> name <> "!"

main = log (greet "World")
```

네, 완전히 그냥 Haskell이네요. [Haskell하고 다른 점을 정리해 둔 문서](https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md)도 있습니다.



## 개발 환경 설정

[Getting Started](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md)를 보시면 npm으로 설치하는 방법이 있고 그냥 바이너리를 받는 방법도 있는데, 저는 npm으로 받는 방법을 선택했습니다.

```
$ npm install -g purescript pulp purescript-psa bower
```

이러면 PureScript의 컴파일러인 `purs`와, 빌드 매니저인 `pulp`와, 예쁜 경고를 보여주는 `psa`와, 제발 자신을 설치하지 말고 [Yarn](https://yarnpkg.com)이나 [npm](https://www.npmjs.com/)을 써달라고 간곡히 부탁하는 `bower`가 설치됩니다. PureScript는 [npm이 의존 패키지 충돌을 패키지 안에 패키지를 설치하는 방식으로 해결하는 문제](http://harry.garrood.me/blog/purescript-why-bower/) 때문에 그렇지 않는 Bower에 기대고 있는데, [Bower와 Node를 제거하려는 시도](https://github.com/purescript/psc-package) 역시 볼 수 있습니다.

[에디터는 현재까지는 Atom, Emacs, Sublime Text 2, Vim, VS Code를 지원](https://github.com/purescript/documentation/blob/master/ecosystem/Editor-and-tool-support.md)합니다. 저는 이 참에 VS Code를 받아서 설치를 해봤습니다.

다 설치했으면, 이제 빈 폴더에

```
$ pulp init
```

라고 치면 뭔가 마법같이 많이 생깁니다. 패키지 인스톨은

```
$ bower install purescript-lists --save
```

와 같이 bower로 할 수 있고, REPL은 `$ pulp repl`로, 브라우저용 빌드는

```
$ pulp browserify -O --to path/to/output.js
```

로 해보실 수 있습니다. CommonJS용 빌드는 `browserify` 대신에 `build`를 써주면 됩니다. 이제 Hello, world!를 써봅시다. 생성된 `src/Main.purs` 코드를 열어 다음과 같이 써줍니다:

```haskell
module Main where

import Prelude
import Control.Monad.Eff.Console (log)

greet :: String -> String
greet name = "Hello, " <> name <> "!"

main = log (greet "World")
```

그리고 빌드합니다.

```
$ pulp browserify -O --to ./output.js
Compiling Main
[1/1 MissingTypeDeclaration] src/Main.purs:9:1

  9  main = log (greet "World")
     ^^^^^^^^^^^^^^^^^^^^^^^^^

  No type declaration was provided for the top-level declaration of main.
  It is good practice to provide type declarations as a form of documentation.
  The inferred type of main was:

    forall t3.
      Eff
        ( console :: CONSOLE
        | t3
        )
        Unit

  in value declaration main
```

음?



## Eff

저 뜨는 것에 굳이 신경 쓸 필요까진 없습니다. 단순히 `main`에 타입을 안 썼다는 *경고*일 뿐, 컴파일에는 지장이 없습니다. 저 코드에서 생성된 `./output.js`를 갖다가 브라우저 콘솔에 실행시키면 문제 없이 `Hello, World!`라고 뜨시는 걸 볼 수 있습니다. 그럼 저

```
The inferred type of main was:
  forall t3. Eff (console :: CONSOLE | t3) Unit
```

은 무엇일까요? 바로 side effect를 다루기 위한 PureScript의 방법입니다. Haskell은 `IO ()`라고 타입을 설정해서 쉽고 간단한 타입을 보여주지만, PureScript는 이것이 너무 포괄적이고 상세하지 않다면서 쓰는 side effect 들을 모두 타입에 기재해버리는 행위를 저질러 어렵고 긴 타입을 만들었습니다.

[Eff에 대해서 따로 기술해둔 문서](https://github.com/purescript/documentation/blob/master/guides/Eff.md)가 존재하니 이를 참고하는 것도 좋습니다. 일단 `Eff`라는 것은 소스 코드를 보면 다음과 같이 정의되어 있습니다:

```haskell
foreign import kind Effect
foreign import data Eff :: # Effect -> Type -> Type
```

`kind`라는 것은 **타입의 타입**이라고 이해하시면 좋습니다. `Effect`라는 속성을 만족하는 타입은 대충 저 `Effect` 자리에 들어갈 수 있는 것입니다. `Type`도 역시 `kind`입니다만 이건 그냥 모든 타입을 말하죠. 여기서 `data Eff :: # Effect -> Type -> Type`가 있는데, 이 `#`이라는 것은 *unordered collection*이랩니다. 즉,  저 `Eff`라는 것은

1. `# Effect`: `Effect`를 만족하는 타입들의 순서 없는 나열과
2. `Type` : 그냥 타입 하나를 받아
3. `Type`: 타입을 반환하는 함수입니다.

`foreign import`라 그 함수에 대한 구현은 바깥 어딘가에 있고 아직 별로 궁금하지 않습니다. 그래서

```haskell
forall t3. Eff (console :: CONSOLE | t3) Unit
```

라는 것은 그냥 `Type`이 되겠지요. 앞에 붙은 `forall t3.`라는 것은 "모든 `t3`에 대해,"라는 뜻입니다. 저 `|`라는 것은 `t3`라는 타입이 `console :: CONSOLE`이라는 사이드 이펙트 속성을 가지고 있는 타입이란 뜻입니다. `data CONSOLE :: Effect`처럼 정의되었기 때문에 `CONSOLE`은 `Effect`를 만족하는 타입이라는 것을 알 수 있습니다. 다른 예를 들어:

```haskell
forall t. Eff (console :: CONSOLE, random :: RANDOM | t) Unit
```

라면 `t`는 `console :: CONSOLE`이라는 속성과 `random :: RANDOM`이라는 속성을 가지고 있는 타입이 됩니다.

자바스크립트의 object literal에 해당하는 record syntax도 비슷합니다. 예로 다음과 같은 함수가 있다면:

```haskell
fullName :: forall t. { firstName :: String, lastName :: String | t } -> String
fullName x = x.firstName <> " " <> x.lastName
```

`t`는 `firstName`과 `lastName`을 키로 가지면 되는 record 타입이 됩니다. 다른 키가 있는지 없는지는 상관이 없기 때문에 다음과 같은 결과가 반환됩니다:

```haskell
> fullName { firstName: "Phil", lastName: "Freeman", location: "Los Angeles" }
Phil Freeman
```

---

무튼 정리하자면 이렇게 됩니다.

```haskell
forall t3. Eff (console :: CONSOLE | t3) Unit
```

라는 것은,

1. `forall t3.` : 모든 `t3`에 대해.
2. `Eff` : 이것은 사이드 이펙트를 사용하는 타입이고
3. `(console :: CONSOLE | t3)` : 사이드 이펙트로 콘솔을 사용하고,
4. `Unit` : 결과로 `Unit`을 반환한다.

가 됩니다. `Unit`은 빈 값만 있는 타입이기 때문에 결국 반환값이 빈 값이라고 볼 수 있습니다.

```haskell
module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

greet :: String -> String
greet name = "Hello, " <> name <> "!"

main :: forall t. Eff (console :: CONSOLE | t) Unit
main = log (greet "World")
```

코드를 이렇게 고치면 이제 경고가 안 뜨게 됩니다.



심란한 Hell-o-world가 끝났습니다. 