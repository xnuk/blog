[].slice.call(document.querySelectorAll('h1,h2,h3,h4,h5,h6')).forEach(function(v){
	var a=document.createElement('a')
	a.textContent='§'
	a.setAttribute('href', '#'+v.id)
	a.setAttribute('class', 'self')
	v.insertBefore(a, v.firstChild)
})
if(!(bowser.firefox || bowser.safari && !bowser.ios)){
	var s=document.createElement('script')
	s.src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=MML_CHTML"
	document.body.appendChild(s)
}
