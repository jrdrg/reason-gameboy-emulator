type t;

type state = {
  gpu: Gpu.t,
  mmu: t,
};

let reset: t => t;

let load: array(int) => t;

let read8: (int, state) => (int, t);

let read16: (int, state) => (int, t);

let write8: (int, int, state) => (t, Gpu.t);

let write16: (int, int, state) => (t, Gpu.t);
