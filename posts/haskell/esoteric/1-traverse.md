---
title: Esoteric Haskell - 1
lang: ko
---

```haskell
traverse (\x -> [0..x]) [0..2]
```

난해한 하스켈 코드 중에서는 애교 수준이지만, 충분히 난해한 편이긴 하다. 충분히 쉬워보인다는 분들을 위해, 이걸 실행하지 말고 무슨 결과가 나올지 예측해보자.

## Traversable

[`traverse`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Traversable.html#v:traverse)는 [`Traversable`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Traversable.html#t:Traversable) 타입 클래스의 필수 구현 함수이다. 문서 위쪽에 적힌 설명문은 구현할 때 조심해야 할 사항들이고, 일단 타입 먼저 보자.

```haskell
traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
```

`Traversable`은 배열이나 리스트 생각하시면 좋다. 뭔가 원소를 담을 수 있는 것인데 동시에 왼쪽에서 오른쪽으로 탐방도 가능한 그런 것들을 `Traversable`이라고 한다.

`traverse`는 첫 번째 인자로 함수 `a -> f b`를 받는다. 원소 `a`를 받아 결과값 `b`를 얻을 수 있는 동작 `f b`를 반환한다. 두 번째 인자로 `t a`라는 배열 비슷한 걸 받고, 이걸 가지고

1. 각각 원소마다 함수 `a -> f b`를 왼쪽에서 오른쪽으로 실행하고
2. 나온 결과값들 `t b`를 `f`에 감싸서 반환하는, 그런 함수다.

간단히 예제를 들자면,

```haskell
traverse print [1, 2, 3]
```

이 있겠다. `print`는 `a -> IO ()`로서 `a`를 출력하는 동작을 뱉는 함수다. 당연히 출력값은

```
1
2
3
```

이 된다. 왼쪽부터 오른쪽으로니까.

자, 이제 `traverse (\x -> [0..x]) [0..2]`는 뭐가 나올 거 같은가?

## 신비하고 난해한 리스트의 세계

```haskell
> traverse (\x -> [0..x]) [0..2]
[[0,0,0],[0,0,1],[0,0,2],[0,1,0],[0,1,1],[0,1,2]]
```

환영한다. 일단 타입을 다시 보자.

```haskell
traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
```

여기서 우리는 `t a`에 `[0..2]`를 넣었고 이건 헷갈릴 여지가 없이 `Num a => [a]`다. 당연히 리스트는 예측 가능하게 `Traversable`일 테니까. 대충 숫자를 `Int`로 잡아 `[Int]`로 보자. 숫자는 별 상관없다. (참고로 `[Int]`는 `[] Int`로 볼 수 있어서, `t`를 `[]`로 볼 수 있는 것이다.)

```haskell
traverse :: (Applicative f) => (Int -> f b) -> [Int] -> f [b]
```

`\x -> [0..x]`의 타입은 당연히 `Int -> [Int]`다. 그럼 `f b`를 `[Int]`로 볼 수 있는 건가? 즉, 리스트는 `Applicative`한 것인가?

```haskell
traverse :: (Int -> [Int]) -> [Int] -> [[Int]]
```

물론.

## Applicative
[`Applicative`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Applicative.html#t:Applicative)는 뭔가를 담고 있는데, 거기에 뭔가 적용을 시킬 수 있는 녀석들을 말한다. 필수 구현 함수에 `pure`와 `<*>`가 있는데, 잠깐 감상하자.

```haskell
pure :: Applicative f => a -> f a
```

`pure`는 그냥 값을 `Applicative`한 녀석으로 감싸주는 함수이다. 간단한 함수.

```haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

`<*>`는 적용을 시켜주는 연산자인데

1. 왼쪽에 `f (a -> b)`라는 함수를 감싼 걸 받고
2. 오른쪽에 `f a`를 받아
3. 적용시켜서
4. `f b`를 내뱉는다.

뭐 대충 예시를 들자면

```haskell
(pure (+1) <*> pure 1) == pure 2
```

라는 소리다. 그럼 리스트는 이걸 어떻게 구현한 걸까? 감상해보자.

```haskell
instance Applicative [] where
    pure x    = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
```

`pure x = [x]`는 당연한 얘기다. `fs <*> xs`는 `fs`에서 함수 `f`를 하나 꺼내고, `xs`에서 원소 `x` 하나를 꺼내서, `f x`를 배열에 저장하는 것**처럼 보인다**. 물론 아니다. `fs`와 `xs`가 길이가 다를 수 있기 때문에, 그리 구현되진 않는다. Haskell의 List comprehension은 쉼표 왼쪽의 경우의 수 하나에 쉼표 오른쪽의 경우를 모두 계산한다. 즉, 위의 문장은:

1. `fs`에서 `f`를 하나 꺼내서, `xs`에 있는 모든 원소에 적용시킨다.
2. 이 과정을 `fs` 안에 있는 함수에 모두 적용하고 그 결과들을 나열한다.

되시겠다. 간단히 예를 들자면 이렇다:

```haskell
> [(+1), (+2), (+4)] <*> [3, 4, 5]
[4,5,6,5,6,7,7,8,9]
```

그럼 이제 `Traversable`을 다시 보도록 하자.

## 리스트의 Traversable

```haskell
instance Traversable [] where
    traverse f = foldr cons_f (pure [])
      where cons_f x ys = (:) <$> f x <*> ys
```

좀 난해하게 쓰였다. 일단 `cons_f`부터 보자.

```haskell
cons_f x ys = (:) <$> f x <*> ys
```

[`<$>`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Applicative.html#v:-60--36--62-)는 [`fmap`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Functor.html#v:fmap)이다.

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

`Applicative`와 `Traversable`은 `Functor`여야 한다는 조건이 있어서, 저 `f`를 `Applicative`나 `Traversable`로 봐도 된다.

```haskell
fmap :: Applicative f => (a -> b) -> f a -> f b
fmap :: Traversable f => (a -> b) -> f a -> f b
```

뭐 하는 함수냐면, 간단하게, `f a`에서 `a`를 받아 `a -> b`에 적용시켜서 `f b`를 내뱉는 말 그대로 [`map`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-List.html#v:map) 비슷한 함수다. 저 `f`가 리스트일 경우엔 그냥 `map`과 같은 함수가 된다. 무튼 그래서:

```haskell
cons_f x ys = fmap (:) (f x) <*> ys
```

로 다시 쓸 수 있다. 여기서 잘 보면,

```haskell
fmap :: Applicative f => (a -> b) -> f a -> f b
```

꼴로 `Applicative`에 대해 `fmap`을 적용하고 있다는 걸 알 수 있는데, 이 경우 `fmap`은 다음과 같게 작동한다:

```haskell
fmap f x = pure f <*> x
```

즉,

```haskell
cons_f x ys = pure (:) <*> f x <*> ys
```

로 다시 쓸 수 있다. `<*>`는 왼쪽 먼저 계산하기 때문에, 괄호는 필요 없다. `f x` 동작을 실행해서 나온 값과 `ys` 동작을 실행해서 나온 값을 `(:)`에 넣어서 반환하는 동작을 주는 그런 꼴이다. `do` 블럭을 이용해 다시 쓰면 다음과 같다:

```haskell
{-# LANGUAGE ApplicativeDo #-}
cons_f x ys = do
    a <- f x
    b <- ys
    pure (a : b)
```

---

```haskell
instance Traversable [] where
    traverse f = foldr cons_f (pure [])
      where cons_f x ys = pure (:) <*> f x <*> ys
```

아까 봤을 때, `traverse :: (a -> f b) -> t a -> f (t b)`로, 인자를 두 개 받는 함수였다.

```haskell
instance Traversable [] where
    traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
    traverse f arr = foldr cons_f (pure []) arr
      where cons_f x ys = pure (:) <*> f x <*> ys
```

이렇게 다시 쓸 수 있다. `traverse`의 `t`는 당연히 리스트가 되어야 한다.

```haskell
instance Traversable [] where
    traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
    traverse f arr = foldr cons_f (pure []) arr
      where cons_f x ys = pure (:) <*> f x <*> ys
```

```haskell
foldr :: Foldable t => (c -> d -> d) -> d -> t c -> d
```

[`foldr`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-List.html#v:foldr)는 현재값 `c`와 나중값 `d`를 받아 `d`를 내뱉는 함수 `c -> d -> d`와, 초기값 `d`와, `c`값들이 있는 `t c`를 받아, `d`로 접어주는 함수이다. 자주 쓰는 함수이니 순서만 헷갈릴 뿐 개념은 어렵지 않을 거다.

정리하자면, 이 함수는

1. 인자로 받은 함수 f를 실행하여
2. 그 결과값들을 새 리스트에 넣어
3. `Applicative`로 감싸 반환하는

그런 함수 되시겠다.

## 문제
```haskell
traverse (\x -> [0..x]) [0..2]
```

문제로 돌아오자. 이곳에서의 `Applicative`는 아까 봤듯이, `[]`이다.

```haskell
instance Traversable [] where
    traverse :: (Int -> [Int]) -> [Int] -> [[Int]]
    traverse f arr = foldr cons_f (pure []) arr
      where cons_f x ys = pure (:) <*> f x <*> ys
```

```haskell
foldr :: Foldable t => (c -> d -> d) -> d -> t c -> d
```
였다. 결과값인 `d`는 `[[Int]]`가 되어야 하고, `pure [] :: t c`이므로 `t`는 `[]`가 된다.

```haskell
foldr :: (c -> [[Int]] -> [[Int]]) -> [[Int]] -> [c] -> [[Int]]
```

즉

```haskell
cons_f :: c -> [[Int]] -> [[Int]]
cons_f x ys = pure (:) <*> f x <*> ys
```

가 되고, `cons_f`에 쓰인 `Applicative`는 리스트였다는 게 밝혀진다. 리스트의 `Applicative` 구현 기억나는가?

```haskell
instance Applicative [] where
    pure x    = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
```

풀어보자.

```haskell
cons_f x ys = [(:)] <*> f x <*> ys

            = [ g a   | g <- [(:)], a <- f x ] <*> ys
            = [ (:) a |             a <- f x ] <*> ys

            = [ h y       | h <- [ (:) a | a <- f x ], y <- ys ]
            = [ ((:) a) y | a <- f x,                  y <- ys ]

            = [ a : y | a <- f x, y <- ys ]
```

문제에서 `f`는 `\x -> [0..x]`였다.

```haskell
cons_f x ys = [ a : y | a <- [0..x], y <- ys ]
```

그리고 `arr`은 `[0..2]`다.

```haskell
traverse _ _ = foldr cons_f (pure []) [0..2]
    where cons_f x ys = [ a : y | a <- [0..x], y <- ys ]
```

`Applicative`가 리스트가 됐으므로, `pure []`는 `[[]]`가 된다.

```haskell
traverse _ _ = foldr cons_f [[]] [0..2]
    where cons_f x ys = [ a : y | a <- [0..x], y <- ys ]
```

뭐가 어떻게 돌아가는지 보도록 하자. `foldr`이므로 오른쪽부터 계산해서, 일단 맨 처음엔 `x = 2`, `ys = [[]]`가 된다.
```haskell
new_ys = [ a : y | a <- [0..2], y <- [[]] ]
       = [ 0 : [], 1 : [], 2 : [] ]
       = [ [0], [1], [2] ]
```
다음으론 `x = 1`, `ys = [[0],[1],[2]]`이 된다.
```haskell
new_ys = [ a : y | a <- [0..1], y <- [[0],[1],[2]] ]
       = [ 0:[0], 0:[1], 0:[2], 1:[0], 1:[1], 1:[2] ]
       = [ [0,0], [0,1], [0,2], [1,0], [1,1], [1,2] ]
```
마지막으로 `x = 0`, `ys = [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2]]`이 된다.
```haskell
new_ys = [ a : y | a <- [0..0], y <- [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2]]  ]
       = [ 0 : y |              y <- [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2]]  ]
       = [ [0,0,0], [0,0,1], [0,0,2], [0,1,0], [0,1,1], [0,1,2] ]
```

짜잔.

