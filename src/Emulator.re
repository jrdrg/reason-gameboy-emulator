exception NoCycles(string);

type debug = {
  instruction: int,
  pc: int,
  sp: int,
};

/* http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf */
type state = {
  mutable frameCount: int,
  mutable fps: int,
  mutable gpu: Gpu.t,
  mutable cpu: Cpu.t,
  mutable mmu: Mmu.t,
  renderer: Renderer.t,
  debugInstructions: list(debug),
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
    debugInstructions: [],
  };
};

let reset = state => {
  ...state,
  fps: 0,
  frameCount: state.frameCount + 1,
  mmu: Mmu.reset(state.mmu),
  gpu: Gpu.make(),
};

let addInstructionToDebug = (s: state, instruction: int) => {
  let {cpu} = s;
  let pc = cpu.registers.pc;
  let sp = cpu.registers.sp;

  let d: debug = {instruction, pc, sp};
  let {debugInstructions} = s;
  let debugInstructions = [d, ...debugInstructions];
  let debugInstructions =
    Belt.List.take(debugInstructions, 50)
    ->Belt.Option.getWithDefault(debugInstructions);

  {...s, debugInstructions};
};

let rec execInstructionsForFrame = (s: state, currCycles: int, maxCycles: int) => {
  let (instruction, mmu) =
    Mmu.read8(Cpu.programCount(s.cpu), {gpu: s.gpu, mmu: s.mmu});

  /* increment PC and wrap around pc if it's more than 2 bytes */
  s.cpu.registers.pc = (Cpu.programCount(s.cpu) + 1) land 0xffff;

  /* execute next instruction */
  let {cpu, mmu}: Cpu_types.state =
    Cpu_ops.exec(instruction, {mmu, gpu: s.gpu, cpu: s.cpu});

  if (cpu.registers.mCycles <= 0) {
    raise(NoCycles(Printf.sprintf("mCycles=0: %x", instruction)));
  };

  /* GPU draw */
  let gpu = s.gpu |> Gpu.step(cpu.registers.mCycles, s.renderer);
  s.gpu = gpu;
  s.mmu = mmu;
  s.cpu = cpu;
  /* update timer  */
  s.cpu.clock = s.cpu.clock + cpu.registers.mCycles;

  // let s = addInstructionToDebug(s, instruction);

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
