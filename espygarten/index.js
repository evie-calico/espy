import init, { espy_eval } from "./pkg/espygarten.js"

await init();

let input = document.getElementById("input");
let output = document.getElementById("output");

function espyRun() {
  output.innerHTML = espy_eval(input.value)
}

espyRun();
input.addEventListener("keyup", espyRun);
