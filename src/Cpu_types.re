type state = {
  cpu: Cpu.t,
  mmu: Mmu.t,
  gpu: Gpu.t,
};

type op = state => state;

let newState = (~cpu=?, ~mmu=?, ~gpu=?, old: state) =>
  Belt.{
    cpu: Option.getWithDefault(cpu, old.cpu),
    mmu: Option.getWithDefault(mmu, old.mmu),
    gpu: Option.getWithDefault(gpu, old.gpu),
  };
