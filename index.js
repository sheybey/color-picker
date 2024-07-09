import { Elm } from  './src/Main.elm';

const app = Elm.Main.init({node: document.getElementById('root')});
app.ports.copy.subscribe((/** @type string */ output) => {
    navigator.clipboard.writeText(output).catch(console.error);
});
