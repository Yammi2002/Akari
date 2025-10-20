# Akari

Akari is a project developed in Haskell and compiled to WebAssembly (WASM) to run directly in the browser. The goal is to bring Haskell code to the web in a lightweight and efficient way.

## Build Instructions

To build the project, first install the Haskell WASM toolchain by running the official bootstrap script from the ghc-wasm-meta repository:
```bash
curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | sh
```
After the installation completes, load the environment variables needed to use the compiler with:
```bash
source ~/.ghc-wasm/env
```
Then compile the Haskell source file Akari.hs to a WebAssembly module using the following command:
```bash
wasm32-wasi-ghc Akari.hs -o akari.wasm -no-hs-main \
  -optl-mexec-model=reactor \
  -optl-Wl,--export=hs_init \
  -optl-Wl,--export=play_wasm \
  -optl-Wl,--export=isComplete_wasm
```
## Run Instructions

Finally, to run the game locally in your browser, start a simple HTTP server such as Python's built-in one:
```bash
python3 -m http.server 8080
```
Then open your browser and navigate to http://127.0.0.1:8080/ to play.

## Console Version

The file Main.hs is used to play the game directly in the terminal.
You can run it by typing:
```powershell
runhaskell .\main.hs
```
## Credits

Developed by Gianmarco Bigliardi and Luca Foramacchi — University of Parma
