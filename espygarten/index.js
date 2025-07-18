import init, { espyscript_eval } from "./pkg/espygarten.js"

await init();

let input = document.getElementById("input");
let output = document.getElementById("output");

function espyscriptRun() {
  output.innerHTML = espyscript_eval(input.value)
}

espyscriptRun();
input.addEventListener("keyup", espyscriptRun);
