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

const boardString = await fetch("/boards/board7x7.1").then((res) => res.text());
drawBoard(boardString);

const encoder = new TextEncoder();
const inputBytes = encoder.encode(boardString + "\0"); // C stringa terminata da null

// Per ora assumiamo che il buffer wasm sia abbastanza grande e usiamo offset 0 (da migliorare)
const mem = new Uint8Array(inst.exports.memory.buffer);
const ptrIn = 0;
mem.set(inputBytes, ptrIn);

// --- CHIAMA play_wasm ---
const ptrOut = inst.exports.play_wasm(ptrIn, 2, 3); // ad esempio, mossa in (x=2, y=3)

// --- LEGGI STRINGA RISULTANTE ---
let result = "";
const memOut = new Uint8Array(inst.exports.memory.buffer);
for (let i = ptrOut; memOut[i] !== 0; i++) {
  result += String.fromCharCode(memOut[i]);
}

console.log(result);

async function drawBoard(boardString) {
  const table = document.getElementById("akari-board");

  // Dividi la board in righe (split su newline)
  const rows = boardString.trim().split("\n");

  for (const row of rows) {
    const tableRow = document.createElement("tr");

    for (const cell of row) {
      const tableCell = document.createElement("td");

      switch (cell) {
        case ".":
          tableCell.classList.add("vuota");
          break;
        case "*":
          tableCell.classList.add("lampadina");
          break;
        case "#":
          tableCell.classList.add("nera");
          break;
        case "0":
        case "1":
        case "2":
        case "3":
        case "4":
          tableCell.classList.add("nera-numerata");
          tableCell.textContent = cell;
          break;
      }

      tableRow.appendChild(tableCell);
    }

    table.appendChild(tableRow);
  }
}
