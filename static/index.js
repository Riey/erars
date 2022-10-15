import { WASI } from '@wasmer/wasi'
import { WasmFs } from '@wasmer/wasmfs'
import browserBindings from '@wasmer/wasi'

const wasmPath = './erars_wasm.wasm';

const wasmFs = new WasmFs();
let wasi = new WASI({
    args: [wasmPath],
    env: {},
    bindings: {
        ...browserBindings,
        fs: wasmFs.fs,
    }
})

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Async function to run our WASI module/instance
const startWasiTask =
  async pathToWasmFile => {
    // Fetch our Wasm File
    let response  = await fetch(pathToWasmFile)
    let wasmBytes = new Uint8Array(await response.arrayBuffer())

    // IMPORTANT:
    // Some WASI module interfaces use datatypes that cannot yet be transferred
    // between environments (for example, you can't yet send a JavaScript BigInt
    // to a WebAssembly i64).  Therefore, the interface to such modules has to
    // be transformed using `@wasmer/wasm-transformer`, which we will cover in
    // a later example

    // Instantiate the WebAssembly file
    let wasmModule = await WebAssembly.compile(wasmBytes);
    let instance = await WebAssembly.instantiate(wasmModule, {
       ...wasi.getImports(wasmModule)
    });

    wasi.start(instance)                      // Start the WASI instance
    let stdout = await wasmFs.getStdOut()     // Get the contents of stdout
    document.write(`Standard Output: ${stdout}`) // Write stdout data to the DOM
  }

async function main() {
    let eraFile = await fetch('./game.era');
    wasmFs.fs.writeFile('./game.era', await eraFile.arrayBuffer(), () => {
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Everything starts here
        startWasiTask(wasmPath)
    });
}

main();
