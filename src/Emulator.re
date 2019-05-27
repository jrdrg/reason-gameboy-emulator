/* http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf */
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
  let programCount = Cpu.programCount(s.cpu);
  let (instruction, mmu) = s.mmu |> Mmu.read8(programCount);
  let (cpu, mmu) = Cpu.exec(instruction, s.cpu, mmu, s.gpu);
  /* GPU draw */
  let gpu = Gpu.step(cpu.registers.mCycles, s.gpu);
  /* update timer */
  let nextState = {
    ...s,
    mmu,
    gpu,
    cpu: {
      registers: {
        ...cpu.registers,
        /* wrap around pc if it's more than 2 bytes */
        pc: (Cpu.programCount(cpu) + 1) land 0xffff,
      },
      clock: cpu.clock + cpu.registers.mCycles,
    },
  };
  nextState;
};
