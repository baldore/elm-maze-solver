require('normalize.css/normalize.css');
require('./styles/main.scss');

var Elm = require('../elm/Main');
Elm.Main.embed(document.getElementById('main'));
