open Utils;

open Js.Promise;

let fpsNum = Document.querySelector("#fps-num");

let spEl = Document.querySelector("#sp");
let pcEl = Document.querySelector("#pc");

let raEl = Document.querySelector("#ra");
let rbEl = Document.querySelector("#rb");
let rcEl = Document.querySelector("#rc");
let rdEl = Document.querySelector("#rd");
let reEl = Document.querySelector("#re");
let rhEl = Document.querySelector("#rh");
let rlEl = Document.querySelector("#rl");
let rfEl = Document.querySelector("#rf");

let instEl = Document.querySelector("#instructions");

let weightRatio = 0.97;

let rec loop = (prevFrame, state: Emulator.state) => {
  let newState = Emulator.frame(state);
  // let newState = state;
  let time = Utils.now();
  let time_ = time *. (1.0 -. weightRatio) +. prevFrame *. weightRatio;
  let diff = time_ -. prevFrame;
  let fps = 1000. /. diff;
  // Window.requestAnimationFrame(() => loop(time_, newState)) |> ignore;
  Window.setTimeout(() => loop(time_, newState), 1000) |> ignore;
  Document.textContent(
    fpsNum,
    fps |> Js.Math.round |> int_of_float |> string_of_int,
  );

  Document.textContent(pcEl, state.cpu.registers.pc |> string_of_int);
  Document.textContent(spEl, state.cpu.registers.sp |> string_of_int);
  Document.textContent(raEl, state.cpu.registers.a |> string_of_int);
  Document.textContent(rbEl, state.cpu.registers.b |> string_of_int);
  Document.textContent(rcEl, state.cpu.registers.c |> string_of_int);
  Document.textContent(rdEl, state.cpu.registers.d |> string_of_int);
  Document.textContent(reEl, state.cpu.registers.e |> string_of_int);
  Document.textContent(rhEl, state.cpu.registers.f |> string_of_int);
  Document.textContent(rlEl, state.cpu.registers.l |> string_of_int);
  Document.textContent(rfEl, Printf.sprintf("%4x", state.cpu.registers.f));

  let debug =
    state.debugInstructions
    |> List.map((inst: Emulator.debug) =>
         Printf.sprintf(
           "[ %6s | %8s | %8s ]",
           string_of_int(inst.instruction),
           string_of_int(inst.pc),
           string_of_int(inst.sp),
         )
       )
    |> Array.of_list
    |> Js.Array.joinWith("\n");

  Document.textContent(instEl, debug);

  ();
};

Xhr.xmlHttpRequest()
|> Xhr.openUri(`GET, "../roms/cpu_instrs/cpu_instrs.gb")
|> then_(bytes => {
     Js.log(bytes);
     resolve(bytes);
   })
|> then_(bytes => resolve(Emulator.load(bytes) |> loop(Utils.now())));