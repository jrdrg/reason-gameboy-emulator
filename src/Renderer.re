type t = {
  canvas: Utils.Canvas.ctx,
  screen: Utils.Canvas.imageData,
};

let make = () => {
  open Utils;
  let canvas = Canvas.getContextFromId("screen");
  let screen = canvas |> Utils.Canvas.createImageData(160, 144);
  let screenData = screen |> Canvas.data;
  /* initialize empty screen */
  screenData |> Array.iteri((idx, _i) => screenData[idx] = 255);
  canvas |> Canvas.putImageData(screen, 0, 0);
  {canvas, screen};
};

let renderToScreen = (renderer, gpu) => {
  let {screen, canvas} = renderer;
  canvas |> Utils.Canvas.putImageData(screen, 0, 0);
  gpu;
};

let renderScan = gpu => gpu;
