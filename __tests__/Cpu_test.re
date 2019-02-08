open Jest;

describe("Cpu", () => {
  open Expect;
  describe("Flags", () => {
    open Cpu.Flags;
    testAll(
      "Setting flags",
      [
        (Z, 0b10000000),
        (N, 0b01000000),
        (H, 0b00100000),
        (C, 0b00010000),
      ],
      ((flag, expected)) => {
        let cpu = Cpu.make() |> Cpu.setFlag(flag, 1);
        expect(cpu |> Cpu.getFlag(flag)) |> toBe(expected);
      },
    );
    test("Setting a flag with an initial value", () => {
      let initialValue = 0b00010000;
      let cpu = Cpu.make() |> Cpu.setFlag(Z, 1, ~initialValue);
      expect(cpu.registers.f) |> toBe(0b10010000);
    });
  });
  describe("Ops", () =>
    test("INC BC", () => {
      let c = Cpu.make();
      let cpu = {
        ...c,
        registers: {
          ...c.registers,
          c: 255,
        },
      };
      let mmu = Mmu.load(Array.make(4096, 0));
      let (cpu1, _) = Cpu.Ops.inc_bc(cpu, mmu);
      expect((Cpu.rBc(cpu1), cpu1.registers.b, cpu1.registers.c))
      |> toEqual((256, 0x1, 0x0));
    })
  );
});
