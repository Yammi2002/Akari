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

const Levels = [
  { name: "Livello 1", path: "/boards/board7x7.1" },
  { name: "Livello 2", path: "/boards/board7x7.2" },
  { name: "Livello 3", path: "/boards/board10x10.1" },
];

let timerInterval;
let secondsElapsed = 0;

let currentLevelIndex = 0;
let currentBoard = "";
const table = document.getElementById("akari-board");

setListeners();

function setListeners(){
document.getElementById("reload-button").addEventListener("click", async function () {
  table.innerHTML = "";
  await loadLevel(currentLevelIndex);
  document.getElementById("overlay").classList.add("invisible");  
  startTimer();
});

document.getElementById("start-button").addEventListener("click", function () {
  document.getElementById("levels").classList.remove("invisible");
  this.classList.add("invisible");
});


  Levels.forEach((level, index) => {
    document.getElementById(`level-${index}`).addEventListener("click", async function () {
      document.getElementById("levels").classList.add("invisible");
      currentLevelIndex = index;
      await loadLevel(index);
      document.getElementById("timer").classList.remove("invisible");
      document.getElementById("best-time").classList.remove("invisible");
      document.getElementById("rules").classList.remove("invisible");
      document.getElementById("start-panel").classList.add("invisible");
      startTimer();
    });
  });

    document.getElementById("next-level-button").addEventListener("click", async function () {
  document.getElementById("overlay").classList.add("invisible");    
    currentLevelIndex++;
    table.innerHTML = "";
    await loadLevel(currentLevelIndex);
    startTimer();
  });
}

async function drawBoard(boardString) {

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

  let mem = new Uint8Array(inst.exports.memory.buffer);
  const ptrIn = 0;
  mem.fill(0, ptrIn, ptrIn + inputBytes.length + 1);
  mem.set(inputBytes, ptrIn);

  const ptrOut = inst.exports.play_wasm(ptrIn, x, y);

  mem = new Uint8Array(inst.exports.memory.buffer);
  let result = "";
  for (let i = ptrOut; mem[i] !== 0; i++) {
    result += String.fromCharCode(mem[i]);
  }

  currentBoard = result;
  table.innerHTML = "";
  drawBoard(result);

  animateCell(x, y);

  const boardBytes = encoder.encode(currentBoard + "\0");
  const ptrCheck = 1024;
  mem = new Uint8Array(inst.exports.memory.buffer);
  mem.fill(0, ptrCheck, ptrCheck + boardBytes.length + 1);
  mem.set(boardBytes, ptrCheck);

  const isComplete = inst.exports.isComplete_wasm(ptrCheck);
  if(isComplete) {
  document.getElementById("overlay").classList.remove("invisible");    
  stopTimer();
    saveBestTime(currentLevelIndex, secondsElapsed);
  }
}

async function loadLevel(index) {
  if (index >= Levels.length) {
    alert("Hai completato tutti i livelli!");
    return;
  }

  const boardString = await fetch(Levels[index].path).then(res => res.text());
  currentBoard = boardString;
  drawBoard(boardString);
  const best = getBestTime(index);
  const bestTimeDiv = document.getElementById("best-time");
  if (best !== null) {
    const min = Math.floor(best / 60);
    const sec = best % 60;
    bestTimeDiv.textContent = `Current best time: ${min.toString().padStart(2,"0")}:${sec.toString().padStart(2,"0")}`;
  } else {
    bestTimeDiv.textContent = "Current best time: Complete this level to set a mew best time!";
  }
}

function startTimer() {
  clearInterval(timerInterval); // Reset
  secondsElapsed = 0;
  updateTimerDisplay();

  timerInterval = setInterval(() => {
    secondsElapsed++;
    updateTimerDisplay();
  }, 1000);
}

function stopTimer() {
  clearInterval(timerInterval);
}

function resetTimer() {
  clearInterval(timerInterval);
  secondsElapsed = 0;
  updateTimerDisplay();
}

function updateTimerDisplay() {
  const minutes = String(Math.floor(secondsElapsed / 60)).padStart(2, "0");
  const seconds = String(secondsElapsed % 60).padStart(2, "0");
  document.getElementById("timer").textContent = `${minutes}:${seconds}`;
}

function pulseCell(cell) {
  cell.classList.add("pulse-once");
  setTimeout(() => {
    cell.classList.remove("pulse-once");
  }, 200); 
}

function animateCell(x, y) {
  const row = table.rows[y];
  if (!row) return;
  const cell = row.cells[x];
  if (!cell) return;

  pulseCell(cell);
}

function saveBestTime(levelIndex, timeInSeconds) {
  const key = `bestTime-level-${levelIndex}`;
  const bestTime = sessionStorage.getItem(key);

  if (!bestTime || timeInSeconds < parseInt(bestTime, 10)) {
    sessionStorage.setItem(key, timeInSeconds);
  }
}

function getBestTime(levelIndex) {
  const key = `bestTime-level-${levelIndex}`;
  const bestTime = sessionStorage.getItem(key);
  return bestTime ? parseInt(bestTime, 10) : null;
}
