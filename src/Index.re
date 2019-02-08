open Utils;

open Js.Promise;

let rec loop = state => {
  let newState = Emulator.frame(state);
  Window.requestAnimationFrame(() => loop(newState)) |> ignore;
};
/* Js.log("frame"); */

let foo =
  Xhr.xmlHttpRequest()
  |> Xhr.openUri(`GET, "../roms/cpu_instrs/cpu_instrs.gb")
  |> then_(bytes => {
       Js.log(bytes);
       resolve(bytes);
     })
  |> then_(bytes => resolve(Emulator.load(bytes) |> loop));
