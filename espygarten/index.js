import init, { espyscript_eval, espyscript_last_result, espyscript_last_output } from "./pkg/espygarten.js"

await init();

let input = document.getElementById("input");
let output = document.getElementById("output");
let result = document.getElementById("result");

function espyscriptRun() {
  espyscript_eval(input.value);
  output.innerText = espyscript_last_output();
  result.innerText = espyscript_last_result();
}

espyscriptRun();
input.addEventListener("keyup", espyscriptRun);
