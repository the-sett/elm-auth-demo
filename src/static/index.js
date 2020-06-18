const LocalStorage = require('./local-storage').ElmLocalStoragePorts;

const {Elm} = require('../elm/Main.elm');

const app = Elm.Main.init({node: document.getElementById('main')});

// Subscribe to local storage.
const localStorage = new LocalStorage();
localStorage.subscribe(app);
