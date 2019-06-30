import { Elm } from './Main.elm'
import './main.css'

var app = Elm.Main.init({
    node: document.querySelector('#main'),
    flags: {
        items: loadItems()
    }
})


app.ports.cache.subscribe(function (data) {
    localStorage.setItem('cache', JSON.stringify(data))
})


function loadItems() {
    var items = JSON.parse(localStorage.getItem('cache'))
    if (!items) {
        return []
    } else {
        return items
    }
}
