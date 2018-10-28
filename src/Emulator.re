/* http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf */

module Gpu = {
  type t = {
    canvas: Utils.Canvas.ctx,
    screen: Utils.Canvas.imageData,
    vram: array(int),
    oam: array(int) /* 0xFE00 to 0xFE9F */
  };

  let make = () => {
    open Utils;
    let canvas = Canvas.getContextFromId("screen");
    let screen = canvas |> Utils.Canvas.createImageData(160, 144);
    let screenData = screen |> Canvas.data;

    /* initialize empty screen */
    screenData |> Array.iteri((idx, _i) => screenData[idx] = 100);
    canvas |> Canvas.putImageData(screen, 0, 0);

    {canvas, screen, vram: Array.make(8192, 0), oam: Array.make(160, 0)};
  };
};

type state = {
  frameCount: int,
  fps: int,
  gpu: Gpu.t,
  cpu: Cpu.t,
  mmu: Mmu.t,
};

let load = bytes => {
  Js.log2("Loaded, ROM length: ", Array.length(bytes));
  let mmu = Mmu.load(bytes);
  let b = mmu |> Mmu.read8(0x0002);
  Js.log2("Byte:", b);
  {fps: 0, frameCount: 0, gpu: Gpu.make(), cpu: Cpu.make(), mmu};
};

let reset = state => {
  ...state,
  fps: 0,
  frameCount: state.frameCount + 1,
  mmu: Mmu.reset(state.mmu),
  gpu: Gpu.make(),
};

let frame = (s: state) => {
  /* increment program counter by 1 */
  let programCount = Cpu.programCount(s.cpu) + 1;
  let cpuIncPc = {
    ...s.cpu,
    registers: {
      ...s.cpu.registers,
      pc: programCount,
    },
  };
  let (cpu, mmu) = Cpu.exec(programCount, cpuIncPc, s.mmu);
  let nextState = {
    ...s,
    mmu,
    cpu: {
      registers: {
        ...cpu.registers,
        /* wrap around pc if it's more than 2 bytes */
        pc: cpu.registers.pc land 65535,
      },
      clock: cpu.clock + cpu.registers.mCycles,
    },
  };
  /* GPU draw */
  /* update timer */
  nextState;
};