---
title: 문제풀이
lang: ko
---

보기의 문항은 모두 분모가 12인 분수로 나타낼 수 있다.
$$1<2$$이므로 $$1<\sqrt2$$이다. 따라서 $$a<\sqrt2$$인 1보다 큰 실수 $$a$$가 존재하고, $$a<\sqrt2$$와 $$a^2 < 2$$는 서로 필요충분조건이다.

## 1. 유리수에 대하여
$$$
\frac{n}{12} < \sqrt2 < \frac{m}{12}\quad (n,m\in\mathbb{N},\,12\leq n,m)
$$$
$$$
\Rightarrow n < 12\sqrt2 < m
$$$
$$$
\Rightarrow n^2 < 12^2\times2 < m^2
$$$
$$$
\Rightarrow 16^2 < 12^2\times2 < 17^2
$$$
$$$
\Rightarrow 16 < \sqrt2\times12 < 17
$$$
$$$
\Rightarrow \frac43 < \sqrt2 < \frac{17}{12}
$$$

즉 유리수 중에서는 4/3 또는 17/12가 가장 가깝다.

## 2. 무리수에 대하여
분모 유리화를 시키면, 보기의 유리수는 $$(a\times\sqrt3)/3$$ 또는 $$(a+b\times\sqrt3)/6$$ 꼴로 나타낼 수 있다. $$(a,b\in\mathbb{N})$$

### 2-1. $$(a\times\sqrt3)/3=a/\sqrt3$$ 꼴에 대하여
$$$
\left( \frac2{\sqrt3} \right)^2 = \frac43 = \frac{12}9
$$$
이 값은 $$\sqrt2$$의 작은 근사값 4/3을 제곱한 16/9보다 작은 값이다.

$$$
\left( \frac4{\sqrt3} \right)^2 = \frac{16}3 = \frac{768}{144}
$$$
이 값은 $$\sqrt2$$의 큰 근사값 17/12를 제곱한 289/144보다 큰 값이다.
따라서 $$(a\times\sqrt3)/3$$ 꼴은 고려하지 않을 수 있다.

### 2-2. $$(a+b\times\sqrt3)/6$$
- $$1/6 = 1/6$$
- $$1/3 = 2/6$$
- $$1/2 = 3/6$$
- $$2/3 = 4/6$$
- $$5/6 = 5/6$$

따라서 $$a$$에는 1,2,3,4,5만 들어간다.

$$$\frac1{\sqrt3} = \frac{2\times\sqrt3}6,\quad\frac2{\sqrt3} = \frac{4\times\sqrt3}6$$$

따라서 $$b$$에는 2,4만 들어간다.

적당히 분모를 8로 잡아서 $$\sqrt3$$의 유리수 근사값을 구해보자.
$$$ \frac{n}8 < \sqrt3 < \frac{m}8 \quad(n,m\in\mathbb{N},\,12\leq n,m) $$$
$$$ \Rightarrow n < 8\sqrt3 < m $$$
$$$ \Rightarrow n^2 < 192 < m^2 $$$
$$$ \Rightarrow 13^2 < 192 < 14^2 $$$
$$$ \Rightarrow 13 < 8\sqrt3 < 14 $$$
$$$ \Rightarrow \frac{13}8 < \sqrt3 < \frac74 $$$
$$$ \Rightarrow \frac{8a+13b}{48} < \frac{a+b\times\sqrt3}6 < \frac{4a+7b}{24} $$$

#### 2-2-1. $$b=2$$일 때
$$$ \frac{8a+13\times2}{48} < \frac{a+b\times\sqrt3}6 < \frac{4a+7\times2}{24} $$$
$$$ \Rightarrow \frac{4a+13}{24} < \frac{a+b\times\sqrt3}6 < \frac{2a+7}{12} $$$

$$4/3 < \sqrt2 < 17/12$$ 였으므로, 상한이 4/3보다 작지 않고 하한이 17/12보다 크지 않아야 근사한다고 볼 수 있다.

$$$ \frac43 \leq \frac{2a+7}{12} $$$
$$$ \Rightarrow 16 \leq 2a+7 $$$
$$$ \Rightarrow 4.5 \leq a $$$

$$$ \frac{17}{12} \geq \frac{4a+13}{24} $$$
$$$ \Rightarrow 34 \geq 4a+13 $$$
$$$ \Rightarrow 5.25 \geq a $$$

따라서 $$a=5$$일 경우 근사할 수 있다.

$$$ \frac{4\times5+13}{24} < \frac{5+2\sqrt3}6 < \frac{2\times5+7}{12} $$$
$$$ \Rightarrow \frac{11}{8} < \frac{5+2\sqrt3}6 < \frac{17}{12} $$$

제곱해 보자.

$$$ \Rightarrow \frac{11^2}{8^2} < \left(\frac{5+2\sqrt3}6\right)^2 < \frac{17^2}{12^2} $$$

$$11^2 / 8^2 < 2 < 17^2/12^2$$가 참이어야 하고, 계산하면 그렇다.

#### 2-2-2. $$b=4$$일 때
$$$ \frac{8a+13\times4}{48} < \frac{a+b\times\sqrt3}6 < \frac{4a+7\times4}{24} $$$
$$$ \Rightarrow \frac{2a+13}{12} < \frac{a+b\times\sqrt3}6 < \frac{a+7}6 $$$

같은 방법으로,

$$$ \frac43 \leq \frac{a+7}6 $$$
$$$ \Rightarrow 1 \leq a $$$

$$$ \frac{17}{12} \geq \frac{2a+13}{12} $$$
$$$ \Rightarrow 1 2 \geq a $$$

가 되므로 이 경우 $$a=1,2,3,4,5$$의 경우 근사할 수 있다. 아까와 같이 제곱해 보자.

$$$ \frac{(2a+13)^2}{12^2} < \left(\frac{a+4\sqrt3}6\right)^2 < \frac{(a+7)^2}{6^2} $$$

먼저,

$$$ \frac{(2a+13)^2}{12^2} < 2 $$$

여야 한다.

$$$ \Rightarrow (2a+13)^2 < 2\times 12^2 $$$
$$$ \Rightarrow (2a+13)^2 < 288 $$$

$$15^2=225$$, $$17^2=289$$ 이므로 $$a=1$$로 추려진다.

그 다음,

$$$ 2 < \frac{(a+7)^2}{6^2} $$$

여야 한다.

$$$ 72 < (a+7)^2 $$$

$$8^2=64$$, $$9^2=81$$ 이므로 $$a=2$$로 추려진다. 따라서 $$b=4$$일 수 없다.

## 3. 정리
정리하면,

$$$
\frac43 < \sqrt2 < \frac{17}{12}
$$$

또는

$$$ \frac{11}{8} < \frac{5+2\sqrt3}6 < \frac{17}{12} $$$

가 $$\sqrt2$$에 근사한다고 볼 수 있다. 통분하면 알 수 있지만

$$$ \frac43 < \frac{11}8 < \sqrt2 < \frac{17}{12}  $$$

가 된다.

### $$(5+2\sqrt3)/6$$와 $$\sqrt2$$의 대소 관계
$$$ \frac{5+2\sqrt3}6 > \sqrt2 $$$

라고 가정하자. 양변을 제곱한다.

$$$ \Rightarrow (5+2\sqrt3)^2 > 36\times2 $$$
$$$ \Rightarrow 37+20\sqrt3 > 72 $$$
$$$ \Rightarrow 20\sqrt3 > 35 $$$
$$$ \Rightarrow \sqrt3 > \frac74 $$$

여기서 이전에

$$$ \frac{13}8 < \sqrt3 < \frac74 $$$

라고 했었고, 따라서 가정은 거짓이다. 유리수와 무리수는 같을 수 없으므로, 같지도 않다. 따라서

$$$ \frac{5+2\sqrt3}6 < \sqrt2 $$$

가 된다.

### 어느 쪽이 더 가까운가

$$$ \frac{5+2\sqrt3}6 < \sqrt2 < \frac{17}{12} $$$

다음과 같이 가정하자:

$$$ \sqrt2 - \frac{5+2\sqrt3}6 < \frac{17}{12} - \sqrt2 $$$

$$$ \Rightarrow 12\sqrt2 - 10 - 4\sqrt3 < 17 - 12\sqrt2 $$$
$$$ \Rightarrow 24\sqrt2 - 4\sqrt3 < 27 $$$

양변을 제곱하면:

$$$ \Rightarrow 2\times24^2 + 3\times4^2 - 2\times24\times4\times\sqrt2\times\sqrt3 < 27^2 $$$
$$$ \Rightarrow \frac{2\times24^2 + 3\times4^2 - 27^2}{2\times24\times4} < \sqrt6 $$$
$$$ \Rightarrow \frac{471}{192} < \sqrt6 $$$
$$$ \Rightarrow \frac{157}{64} < \sqrt6 $$$

또 제곱하면:

$$$ \Rightarrow \frac{24649}{4096} < 6 $$$
$$$ \Rightarrow 24649 < 24576 $$$

이므로 거짓. 즉

$$$ \frac{17}{12} - \sqrt2 < \sqrt2 - \frac{5+2\sqrt3}6 $$$

가 되어, 17/12가 가장 가까운 수가 된다.
