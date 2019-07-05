import { Elm } from './Main.elm'
import './main.css'


const loadItems = () => {
    const items = JSON.parse(localStorage.getItem('cache'))
    if (!items) {
        return []
    } else {
        return items
    }
}


let app = Elm.Main.init({
    node: document.querySelector('#main'),
    flags: {
        items: loadItems()
    }
})


app.ports.cache.subscribe(
    data => localStorage.setItem('cache', JSON.stringify(data))
)
