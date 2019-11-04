type t = {
  canvas: Utils.Canvas.ctx,
  screen: Utils.Canvas.imageData,
};

let make: unit => t;

let renderToScreen: t => unit;

let setPixel: (t, ~pixel: int, ~color: int) => unit;
