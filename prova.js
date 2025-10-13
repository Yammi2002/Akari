import {
  WASI,
  ConsoleStdout,
} from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.4.2/dist/index.min.js";

let args = [];
let env = [];
let fds = [
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
];
let wasi = new WASI(args, env, fds);

// Carica il modulo .wasm
let wasm = await WebAssembly.compileStreaming(fetch("akari.wasm"));
let inst = await WebAssembly.instantiate(wasm, {
  wasi_snapshot_preview1: wasi.wasiImport,
});

// Inizializza il WASI shim
await wasi.initialize(inst);

// Inizializza il runtime Haskell
if (typeof inst.exports.hs_init === "function") {
  inst.exports.hs_init();
  console.log("Runtime Haskell inizializzato.");
} else {
  console.warn("hs_init non trovato.");
}

let currentBoard = "";
const boardString = await fetch("/boards/board7x7.1").then((res) => res.text());
currentBoard = boardString;
drawBoard(currentBoard);

async function drawBoard(boardString) {
  const table = document.getElementById("akari-board");

  // Dividi la board in righe (split su newline)
  const rows = boardString.trim().split("\n");

  for (let rowIndex = 0; rowIndex < rows.length; rowIndex++) {
    const row = rows[rowIndex];
    const tableRow = document.createElement("tr");

    for (let colIndex = 0; colIndex < row.length; colIndex++) {
      const cell = row[colIndex];
      const tableCell = document.createElement("td");

      switch (cell) {
        case ".":
          tableCell.classList.add("vuota");
          tableCell.addEventListener("click", () => play(colIndex, rowIndex));
          break;
        case "*":
          tableCell.classList.add("lampadina");
          tableCell.addEventListener("click", () => play(colIndex, rowIndex));

          break;
        case "#":
          tableCell.classList.add("nera");
          break;
        case "+":
        case "a":
        case "b":
        case "c":
        case "5":
        case "6":
        case "7":
        case "8":
        case "9":
          tableCell.classList.add("illuminata");
          break;
        case "0":
        case "1":
        case "2":
        case "3":
        case "4":
          tableCell.classList.add("numerata");
          tableCell.textContent = cell;
          break;
      }

      tableRow.appendChild(tableCell);
    }

    table.appendChild(tableRow);
  }
}

function play(x, y) {
  const encoder = new TextEncoder();
  const inputBytes = encoder.encode(currentBoard + "\0");

  const mem = new Uint8Array(inst.exports.memory.buffer);
  const ptrIn = 0;
  mem.fill(0, ptrIn, ptrIn + inputBytes.length + 1);
  mem.set(inputBytes, ptrIn);
  console.log("Board passata a play_wasm:\n", currentBoard);

  const ptrOut = inst.exports.play_wasm(ptrIn, x, y);

  let result = "";
  const memOut = new Uint8Array(inst.exports.memory.buffer);
  for (let i = ptrOut; memOut[i] !== 0; i++) {
    result += String.fromCharCode(memOut[i]);
  }
  console.log("Board restituita da play_wasm:\n", result);

  currentBoard = result; // aggiorna lo stato della board
  const table = document.getElementById("akari-board");
  table.innerHTML = "";
  drawBoard(result);
}
