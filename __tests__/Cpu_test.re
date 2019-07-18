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
  describe("Ops", () => {
    test("INC BC", () => {
      let cpu = Cpu.make() |> Cpu.setRegisters(~c=255);
      let mmu = Mmu.load(Array.make(4096, 0));
      let gpu = Gpu.make();
      let (cpu1, _) = Cpu.Ops.inc_bc({mmu, gpu, cpu});
      expect((Cpu.rBc(cpu1), cpu1.registers.b, cpu1.registers.c))
      |> toEqual((256, 0x1, 0x0));
    });
    testAll(
      "RLCA",
      [
        (0b10001000, 0b00010001, 0b00010000),
        (0b11111111, 0b11111111, 0b00010000),
        (0b00000001, 0b00000010, 0b00000000),
      ],
      ((input, expected, carry)) => {
        let cpu = Cpu.make() |> Cpu.setRegisters(~a=input);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        let (cpu1, _) = Cpu.Ops.rlca({cpu, mmu, gpu});
        expect((cpu1.registers.a, Cpu.getFlag(Cpu.Flags.C, cpu1)))
        |> toEqual((expected, carry));
      },
    );
    describe("INC B", () => {
      test("increments register b", () => {
        let c = Cpu.make();
        let cpu = {
          ...c,
          registers: {
            ...c.registers,
            b: 9,
          },
        };
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        let (cpu1, _) = Cpu.Ops.inc_b({cpu, mmu, gpu});
        expect(cpu1.registers.b) |> toEqual(0x0a);
      });
      test("updates the carry flag", () => {
        let c = Cpu.make();
        let cpu = {
          ...c,
          registers: {
            ...c.registers,
            b: 255,
          },
        };
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        let (cpu1, _) = Cpu.Ops.inc_b({cpu, mmu, gpu});
        expect((cpu1.registers.b, cpu1.registers.f))
        |> toEqual((0x0, 0x0 lor 0x1 lsl 7));
      });
    });
  });
});
