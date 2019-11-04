/* http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf */
type state = {
  mutable frameCount: int,
  mutable fps: int,
  mutable gpu: Gpu.t,
  mutable cpu: Cpu.t,
  mutable mmu: Mmu.t,
  renderer: Renderer.t,
};

let load = bytes => {
  Js.log2("Loaded, ROM length: ", Array.length(bytes));
  let mmu = Mmu.load(bytes);
  {
    fps: 0,
    frameCount: 0,
    renderer: Renderer.make(),
    gpu: Gpu.make(),
    cpu: Cpu.make(),
    mmu,
  };
};

let reset = state => {
  ...state,
  fps: 0,
  frameCount: state.frameCount + 1,
  mmu: Mmu.reset(state.mmu),
  gpu: Gpu.make(),
};

let rec execInstructionsForFrame = (s: state, currCycles: int, maxCycles: int) => {
  let programCount = Cpu.programCount(s.cpu);
  let (instruction, mmu) =
    Mmu.read8(programCount, {gpu: s.gpu, mmu: s.mmu});
  /* execute next instruction */
  let {cpu, mmu}: Cpu.Ops.state =
    Cpu.exec(instruction, {mmu, gpu: s.gpu, cpu: s.cpu});
  /* GPU draw */
  let gpu = s.gpu |> Gpu.step(cpu.registers.mCycles, s.renderer);
  s.gpu = gpu;
  s.mmu = mmu;
  s.cpu = cpu;
  /* update timer  */
  s.cpu.clock = s.cpu.clock + cpu.registers.mCycles;
  /* increment PC and wrap around pc if it's more than 2 bytes */
  s.cpu.registers.pc = (Cpu.programCount(cpu) + 1) land 0xffff;
  if (currCycles >= maxCycles) {
    s;
  } else {
    execInstructionsForFrame(s, currCycles + 1, maxCycles);
  };
};

let frame = (s: state) => {
  let mCycles_in_frame = 17556;
  let frameClock = s.cpu.clock + mCycles_in_frame;
  execInstructionsForFrame(s, s.cpu.clock, frameClock);
};
