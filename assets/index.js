require('../assets/images/data_center-large.png')

const LocalStorage = require('./local-storage').ElmLocalStoragePorts;

const { Elm } = require('../src/elm/Top.elm');

const app = Elm.Top.init({node: document.getElementById('application')});

// Subscribe to local storage.
const localStorage = new LocalStorage();
localStorage.subscribe(app);
