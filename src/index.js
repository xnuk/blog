(function(){

var messages={
	// 엉엉 나도 템플릿 쓰고 십따
	en: {
		paragraph_anchor: function(v){'Link to paragraph: '+v}
	},
	ko: {
		paragraph_anchor: function(v){'단락 링크: '+v}
	}
}
var lang=document.documentElement.lang
;[].slice.call(document.querySelectorAll('h1,h2,h3,h4,h5,h6')).forEach(function(v){
	var a=document.createElement('a')
	a.textContent='§'
	a.setAttribute('href', '#'+v.id)
	a.setAttribute('class', 'self')
	a.setAttribute('aria-label', messages[lang].paragraph_anchor(v.textContent))
	v.insertBefore(a, v.firstChild)
})
if(!(bowser.firefox || bowser.safari && !bowser.ios)){
	var s=document.createElement('script')
	s.src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=MML_CHTML"
	document.body.appendChild(s)
}

})()
