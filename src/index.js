import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'));

global.readLocalStorage = function() {
    const savedGame = localStorage.getItem('savedGame');
    const json = JSON.parse(savedGame);
    app.ports.gameLoaded.send(json);
};

app.ports.saveGame.subscribe(function(data) {
    localStorage.setItem('savedGame', JSON.stringify(data));
});

app.ports.loadGame.subscribe(function() {
    readLocalStorage();
});

registerServiceWorker();
