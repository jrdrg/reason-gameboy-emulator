type t = {
  canvas: Utils.Canvas.ctx,
  screen: Utils.Canvas.imageData,
};

let make = () => {
  open Utils;
  let canvas = Canvas.getContextFromId("screen");
  let screen = canvas |> Canvas.createImageData(160, 144);
  let screenData = screen |> Canvas.data;
  /* initialize empty screen */
  screenData |> Array.iteri((idx, _i) => screenData[idx] = 255);
  canvas |> Canvas.putImageData(screen, 0, 0);
  {canvas, screen};
};

let renderToScreen = renderer => {
  let {screen, canvas} = renderer;
  canvas |> Utils.Canvas.putImageData(screen, 0, 0);
};

let renderScan = (renderer, gpu) => {
  open Utils;
  open Belt;
  let palette = [|255, 192, 96, 0|];
  /* 255; break;
     case 1: GPU._palette.bg[i] = 192; break;
     case 2: GPU._palette.bg[i] = 96; break;
     case 3: GPU._palette.bg[i] = 0; b  */
  let {screen} = renderer;
  let screenData = screen |> Canvas.data;
  Range.forEach(
    0,
    159,
    i => {
      Js.log(i);
      Js.log(i);
    },
  );
  gpu;
};
