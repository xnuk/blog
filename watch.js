const exec=require('child_process').exec
const onchange=require('debounce')(f => {
  const name=f.replace(/\.md$/, '')
  exec(`pandoc --template src/template.html --mathml posts/${name}.md -o dest/${name}.html`).once('exit', (code, signal) => {
    console.log(code===0 ? 'Successful' : `Code ${code}: ${signal}`)
  })
}, 400)
require('fs').watch('posts/', (e, f) => { if(e==='change' && /\.md$/.test(f)) onchange(f) })
