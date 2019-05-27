type t = {
  canvas: Utils.Canvas.ctx,
  screen: Utils.Canvas.imageData,
  mode: int,
  clock: int,
  line: int,
  vram: array(int),
  oam: array(int) /* 0xFE00 to 0xFE9F */
};

let renderToScreen = gpu => {
  let {screen, canvas} = gpu;
  canvas |> Utils.Canvas.putImageData(screen, 0, 0);
  gpu;
};

let make = () => {
  open Utils;
  let canvas = Canvas.getContextFromId("screen");
  let screen = canvas |> Utils.Canvas.createImageData(160, 144);
  let screenData = screen |> Canvas.data;
  /* initialize empty screen */
  screenData |> Array.iteri((idx, _i) => screenData[idx] = 255);
  canvas |> Canvas.putImageData(screen, 0, 0);
  {
    canvas,
    screen,
    mode: 0,
    clock: 0,
    line: 0,
    vram: Array.make(8192, 0),
    oam: Array.make(160, 0),
  };
};

let reset = gpu => {
  ...gpu,
  mode: 0,
  line: 0,
  clock: 0,
  vram: [||],
  oam: [||],
};

let renderScan = gpu => gpu;

let step = (mCycles: int, gpu: t) => {
  let modeclock = gpu.clock + mCycles;
  let gpu = {...gpu, clock: modeclock};
  /*

   scanline (oam): mode 2, 20 machine cycles
   scanline (vram): mode 3, 43 machine cycles
   hblank: mode 0, 51 machine cycles
   vblank: mode 1, 1140 machine cycles - 10 lines
           1 line: 114 machine cycles
   */
  switch (gpu.mode) {
  /* hblank - after last one, render to canvas */
  | 0 =>
    if (modeclock >= 51) {
      let modeclock = 0;
      let line = gpu.line + 1;
      if (line == 143) {
        /* enter vblank */
        {...gpu, clock: modeclock, mode: 1} |> renderToScreen;
      } else {
        {...gpu, clock: modeclock, mode: 2};
      };
    } else {
      gpu;
    }
  /* vblank - 10 lines */
  | 1 => gpu
  /* oam read */
  | 2 =>
    if (modeclock >= 20) {
      {
        /* enter scanline mode */
        ...gpu,
        clock: 0,
        mode: 3,
      };
    } else {
      gpu;
    }
  /* vram read, scanline active */
  | 3 =>
    if (modeclock >= 43) {
      {
        /* enter hblank */
        ...gpu,
        clock: 0,
        mode: 0,
      }
      |> renderScan;
    } else {
      gpu;
    }
  | _ => gpu
  };
};
