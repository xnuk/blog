% 2017학년도 9월 모평 수학

## 이건 쓸데없는 하스켈 코드야!
와우 `놀라워`
```haskell
import Data.Monoid ((<>))
import Data.Char (isSpace)
import Data.List (dropWhile)

main = do
  line <- getLine
  let ltrim = dropWhile isSpace line
      rtrim = reverse . dropWhile isSpace $ reverse ltrim -- lol
  putStrLn $ "Hell\no, " <> rtrim <> "!"
```

```js
console.log('h\ni', /^\s+/)
```

```css
#a{
  aaa: bbb;
}
.b{
  border: 3;
}
```

```html
<b> 꺄 &gt; </b>
```

## 문제 20
우선
$$\angle DFE = \frac\pi2$$
$$\angle BCE = \frac\pi2$$
$$\angle BEC = \angle DEF$$
이므로 $\triangle BCE$와 $\triangle DFE$ 는 AA 닮음. 따라서 $\angle EBC = \angle EDF = \theta$.

$$\overline{CE} = \tan\theta$$
$$\overline{DE} = 1 - \overline{CE} = 1 - \tan\theta$$

원의 중심을 $O$라 하고 $\overline{DF}$를 이등분하는 점을 $G$라 하자. 그러면
$$\overline{OG} = \overline{DO} \sin\theta = \frac{1 - \tan\theta}2\sin\theta$$
$$\overline{OG} + 2r(\theta) = \overline{DO}$$
$$= \frac{1 - \tan\theta}2\sin\theta + 2r(\theta) = \frac{1 - \tan\theta}2$$
$$\Rightarrow r(\theta) = \frac{1 - \tan\theta}4 (1-\sin\theta)$$

이제 계산해보면
$$\lim_{\theta\to\frac\pi4-0}\,\frac{r(\theta)}{\frac\pi4-\theta}$$
$$= \lim_{\theta\to\frac\pi4-0}\,\frac{\frac{1 - \tan\theta}4 (1-\sin\theta)}{\frac\pi4-\theta}$$
$$= \lim_{\theta\to\frac\pi4-0}\,\frac{(1 - \tan\theta) (1-\sin\theta)}{\pi-4\theta}$$
$$= \left(1-\frac1{\sqrt2}\right)\lim_{\theta\to\frac\pi4-0}\,\frac{1 - \tan\theta}{\pi-4\theta}$$

여기서, $t=\frac\pi4 - \theta \Rightarrow \theta=\frac\pi4-t$라 두면,
$$= \left(1-\frac1{\sqrt2}\right)\lim_{t\to0+}\frac{1 - \tan\left(\frac\pi4-t\right)}{\pi-4\times\left(\frac\pi4-t\right)}$$
$$= \left(1-\frac1{\sqrt2}\right)\lim_{t\to0+}\frac1{4t}\left(1 - \frac{\sin\frac{\pi}4\cos t-\cos\frac{\pi}4\sin t}{\cos\frac{\pi}4\cos t+\sin\frac{\pi}4\sin t}\right)$$

$\sin\frac{\pi}4 = \cos\frac{\pi}4 = \frac1{\sqrt2}$이므로

$$= \left(1-\frac1{\sqrt2}\right)\lim_{t\to0+}\frac1{4t}\left(1 - \frac{\cos t-\sin t}{\cos t+\sin t}\right)$$
$$= \left(1-\frac1{\sqrt2}\right)\lim_{t\to0+}\frac1{4t}\frac{(\cos t+\sin t)-(\cos t-\sin t)}{\cos t+\sin t}$$
$$= \left(1-\frac1{\sqrt2}\right)\lim_{t\to0+}\frac1{4t}\frac{2\sin t}{\cos t+\sin t}$$
$$= \left(1-\frac1{\sqrt2}\right)\frac1{\cos0+\sin0}\lim_{t\to0+}\frac{2\sin t}{4t}$$
$$= \left(\frac{\sqrt2}{\sqrt2}-\frac1{\sqrt2}\right)\times\frac12=\frac{\sqrt2-1}{2\sqrt2}=\frac{2-\sqrt2}4$$

## 문제 21
1. $$\left(\frac{f(x)}x\right)^'=x^2e^{-x^2}$$
2. $$g(x)=\frac4{e^4}\int^x_1e^{t^2}f(t)\;dt$$
3. $$f(1)=\frac1e$$

수식 (2)를 보면,

$$g(x)=\frac2{e^4}\int^x_{1}2t\cdot e^{t^2}\frac{f(t)}t\;dt$$
$$=\frac2{e^4}\left(\left[e^{t^2}\frac{f(t)}t\right]^x_1 - \int^x_1e^{t^2}\left(\frac{f(t)}t\right)^'\;dt\right)$$

수식 (1)에 따라,

$$g(x)=\frac2{e^4}\left(\left[e^{t^2}\frac{f(t)}t\right]^x_1 - \int^x_1e^{t^2}(t^2e^{-t^2})\;dt\right)$$
$$=\frac2{e^4}\left(e^{x^2}\frac{f(x)}x - e\cdot f(1) - \int^x_1t^2\;dt\right)$$

수식 (3)을 적용시키면,

$$g(x)=\frac2{e^4}\left(e^{x^2}\frac{f(x)}x - 1 - \frac13(x^3-1)\right)$$
$$\Rightarrow g(2)=\frac2{e^4}\left(\frac{e^4}2f(2) - 1 - \frac73\right)$$
$$\Rightarrow g(2)-f(2)=-\frac2{e^4}\left(1 + \frac73\right)$$
$$\Rightarrow f(2)-g(2)=\frac{20}{3e^4}$$
