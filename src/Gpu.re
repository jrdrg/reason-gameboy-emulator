type t = {
  mode: int,
  clock: int,
  line: int,
  vram: array(int),
  oam: array(int), /* 0xFE00 to 0xFE9F */
  tileset: array(array(array(int))),
};

let initTileset = () => Array.make(384, Array.make(8, Array.make(8, 0)));

let make = () => {
  mode: 0,
  clock: 0,
  line: 0,
  vram: Array.make(8192, 0),
  oam: Array.make(160, 0),
  tileset: initTileset(),
};

let reset = () => {
  mode: 0,
  line: 0,
  clock: 0,
  vram: Array.make(8192, 0),
  oam: Array.make(160, 0),
  tileset: initTileset(),
};

let step = (mCycles: int, renderer: Renderer.t, gpu: t) => {
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
      Js.log("hblank");
      let clock = 0;
      let line = gpu.line + 1;
      if (line == 143) {
        /* enter vblank */
        {...gpu, clock, line, mode: 1} |> Renderer.renderToScreen(renderer);
      } else {
        {...gpu, clock, line, mode: 2};
      };
    } else {
      gpu;
    }
  /* vblank - 10 lines */
  | 1 =>
    if (modeclock >= 114) {
      Js.log("vblank");
      let clock = 0;
      let line = gpu.line + 1;
      /* 10 lines after 143 */
      if (line > 153) {
        {...gpu, clock, line: 0, mode: 2};
      } else {
        {...gpu, clock, line};
      };
    } else {
      gpu;
    }
  /* oam read */
  | 2 =>
    if (modeclock >= 20) {
      Js.log("oam read");
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
      Js.log("vram read");
      {
        /* enter hblank */
        ...gpu,
        clock: 0,
        mode: 0,
      }
      |> Renderer.renderScan;
    } else {
      gpu;
    }
  | _ => gpu
  };
};
