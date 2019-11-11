open Utils;

type t = {
  canvas: Canvas.ctx,
  screen: Canvas.imageData,
};

let make = () => {
  let canvas = Canvas.getContextFromId("screen");
  let screen = canvas |> Canvas.createImageData(160, 144);
  let screenData = screen |> Canvas.data;
  /* initialize empty screen */
  screenData |> Array.iteri((idx, _i) => screenData[idx] = 196);
  canvas |> Canvas.putImageData(screen, 0, 0);
  Js.log(
    Printf.sprintf("Initialized screen data: %d", Array.length(screenData)),
  );
  {canvas, screen};
};

let renderToScreen = renderer => {
  let {screen, canvas} = renderer;
  canvas |> Canvas.putImageData(screen, 0, 0);
};

let setPixel = (renderer, ~pixel: int, ~color: int) => {
  let {screen} = renderer;
  let screenData = screen |> Canvas.data;
  screenData[pixel] = color;
};
