open Cpu;
open Cpu_types;

module Swap = {
  let swap = (cpu, value) => {
    let nibble1 = (value land 0xf0) lsr 4;
    let nibble2 = (value land 0x0f) lsl 4;
    let swapped = nibble1 + nibble2;
    (
      swapped,
      cpu
      |> setFlag(Z, swapped === 0 |> b2i)
      |> setFlag(N, 0)
      |> setFlag(H, 0)
      |> setFlag(C, 0),
    );
  };

  let swap_a: op =
    ({cpu} as s) => {
      let (a, cpu) = swap(cpu, cpu.registers.a);
      newState(~cpu=cpu |> setRegisters(~a) |> machineCycles(2), s);
    };
  let swap_b: op =
    ({cpu} as s) => {
      let (b, cpu) = swap(cpu, cpu.registers.b);
      newState(~cpu=cpu |> setRegisters(~b) |> machineCycles(2), s);
    };
  let swap_c: op =
    ({cpu} as s) => {
      let (c, cpu) = swap(cpu, cpu.registers.c);
      newState(~cpu=cpu |> setRegisters(~c) |> machineCycles(2), s);
    };
  let swap_d: op =
    ({cpu} as s) => {
      let (d, cpu) = swap(cpu, cpu.registers.d);
      newState(~cpu=cpu |> setRegisters(~d) |> machineCycles(2), s);
    };
};
