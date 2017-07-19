(function(){

;[].slice.call(document.querySelectorAll('pre[class]>code')).forEach(function(v){
	v.className=v.parentNode.className
	hljs.highlightBlock(v)
})
if(!(bowser.firefox || bowser.safari && !bowser.ios)){
	var s=document.createElement('script')
	s.src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=MML_CHTML"
	document.body.appendChild(s)
}

})()
