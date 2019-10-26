open Utils;

open Js.Promise;

let fpsNum = Document.querySelector("#fps-num");

let weightRatio = 0.97;

let rec loop = (prevFrame, state) => {
  let newState = Emulator.frame(state);
  let time = Utils.now();
  let time_ = time *. (1.0 -. weightRatio) +. prevFrame *. weightRatio;
  let diff = time_ -. prevFrame;
  let fps = 1000. /. diff;
  Window.requestAnimationFrame(() => loop(time_, newState)) |> ignore;
  Document.textContent(
    fpsNum,
    fps |> Js.Math.round |> int_of_float |> string_of_int,
  );
};

Xhr.xmlHttpRequest()
|> Xhr.openUri(`GET, "../roms/cpu_instrs/cpu_instrs.gb")
|> then_(bytes => {
     Js.log(bytes);
     resolve(bytes);
   })
|> then_(bytes => resolve(Emulator.load(bytes) |> loop(Utils.now())));
