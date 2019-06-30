import { Elm } from './Main.elm'
import './main.css'

var app = Elm.Main.init({
    node: document.querySelector('#main')
})


app.ports.cache.subscribe(function(data) {
    localStorage.setItem('cache', JSON.stringify(data))
})
