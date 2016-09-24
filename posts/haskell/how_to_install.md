---
title: Haskell을 설치하는 법
lang: ko
---

Haskell을 설치하려고 [Haskell 공식 홈페이지의 다운로드 안내 사이트](https://www.haskell.org/downloads)에 들어가 보면 Haskell 설치 방법을 세 가지로 나누어 설명하고 있습니다:

1. GHC와 Cabal을 설치하는 방법과
2. Stack을 설치하는 방법과
3. GHC와 Cabal과 Stack을 설치하는 방법.

대체 GHC와 Cabal은 무엇이며, Stack은 또 뭘까요? 간단히 알려드려 보겠습니다.

## 용어
GHC(Glasgow Haskell Compiler)는 Haskell 컴파일러입니다. 2016년 5월 21일 기준 최신 버전은 GHC 8.0.1^[[GHC 다운로드 페이지](https://www.haskell.org/ghc/)에서 확인할 수 있습니다.] 입니다. 물론 Haskell 컴파일러가 이것만 있는 것은 아니고 다른 컴파일러도 몇 있습니다만 GHC가 가장 많이 쓰이는 편입니다. GHC는 컴파일 뿐만 아니라 대화형 인터프리터 역시 지원하는데, 이를 GHCi라고 부릅니다.

Cabal은 패키지 매니저입니다. 물론 다른 언어의 패키지 매니저처럼 패키지 빌드 역시 Cabal로 가능합니다. 그러나 [Butterfly effect](https://cdsmith.wordpress.com/2011/01/17/the-butterfly-effect-in-cabal/)라고 불리는 의존성 문제가 있어서, 의존성 문제가 없는 패키지 리스트만을 뽑아서 배포하자고 만든 게 바로 Stack입니다. (자료 구조 할 때의 그 스택은 아닙니다. 사실 이게 널리 쓰이는 용어라 이름 가지고 얘기가 좀 있긴 했습니다.^[[Please rename 'stack' to 'haystack' - /r/haskell](https://www.reddit.com/r/haskell/comments/3d3e95/please_rename_stack_to_haystack/)]) Haskell의 중앙 패키지 저장소는 Hackage라고 불리고, Stack에서 쓰이는 패키지 리스트 저장소는 Stackage라고 부릅니다. 또한 리스트를 정기 버전인 [LTS Haskell](https://www.stackage.org/lts)과 하루마다 나오는 [Stackage Nightly](https://www.stackage.org/nightly)로 나누어 배포하고 있습니다.

### 그럼 Cabal은 쓸모가 없나요?
네, 굳이 다운 받으실 필요까진 없습니다. Hackage의 많은 패키지들이 Stackage에도 등록되어 있어서, 대부분의 경우에는 Stack만으로도 패키지 설치에는 지장이 없을 정도입니다. 다만 Stack이 2015년 중순부터 나오기 시작한 패키지 매니저^[[ANNOUNCING: first public beta of stack - FP complete, 2015년 6월 9일 작성](https://www.fpcomplete.com/blog/2015/06/announcing-first-public-beta-stack)]라서, 간혹 Hackage에 있지만 Stackage에는 없는 패키지이거나 소스에서 `stack.yaml`을 지원하지 않는 패키지의 경우 Stack을 쓰지 못하고 Cabal을 써야 하게 됩니다.

## 설치 방법
제 설득을 잘 들으셨다면, Cabal은 굳이 깔지 않아도 된다는 걸 아셨을 겁니다. 그럼 설치 방법은 이렇게 세 가지로 바뀌게 됩니다:

1. GHC만 설치하는 방법과
2. Stack을 설치하는 방법과
3. GHC와 Stack을 설치하는 방법.

그런데 Stack은 설치할 때 ghc 역시 같이 깔아줍니다. 그래서 3번 역시 필요가 없답니다. `:tada:`

### GHC만 설치하는 방법
패키지 매니저 없이 GHC만 써보는 방법입니다. 그닥 권장하는 방법은 아닙니다만, 그냥 다운로드 받고 압축 풀면 끝이라서 Haskell 기초를 배울 때 잠깐 까는 것도 나쁘지 않습니다.

1. [GHC 다운로드 페이지](https://www.haskell.org/ghc/)에서 최신 버전의 GHC를 다운받습니다.
2. 압축을 풀고, `bin` 폴더를 PATH에 추가합니다.
    - Unix 계열 운영체제는 `$PATH` 변수를 조정하세요.
    - Windows 계열 운영체제는 `시스템 속성` → `고급` → `환경 변수`에서 PATH를 조정하세요.
3. 설치가 끝났습니다. 커맨드 라인에서 `ghc --version`을 쳐보세요.

설치가 끝나면 `ghc`, `ghci`, `runhaskell` 등이 같이 깔려 있게 됩니다. `ghc`는 컴파일러고, `runhaskell`은 Haskell 소스를 바로 실행해주는 프로그램, `ghci`는 대화형 인터프리터입니다.

### Stack을 설치하는 방법
1. [이곳의 지시사항에 따라 Stack을 다운받고 설치합니다](https://docs.haskellstack.org/en/stable/README/).
2. 설치가 끝나면, 커맨드 라인에서 `stack setup`을 실행합니다. 인터넷이 연결되어 있어야 하고, 좀 오래 걸릴 겁니다.
3. 끝나면 모든 설치가 끝나게 됩니다. `stack --help`를 쳐보세요.

주의할 점은 `ghc`, `ghci`, `runhaskell` 등은 `stack` 안의 커맨드로 실행시킬 수 있다는 점입니다. `ghci`가 아닌 `stack ghci`로 GHCi를 실행시켜야 하고, `ghc -O2 Main.hs`가 아닌 `stack ghc -- -O2 Main.hs`로 커맨드를 실행해야 합니다. 이 점을 주의하시길 바랍니다.

`stack install`로 패키지를 받을 수 있고, `stack new`로 새 패키지를 만들고, `stack build`로 패키지를 빌드할 수 있습니다.
