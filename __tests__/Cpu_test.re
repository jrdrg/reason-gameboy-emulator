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
    describe("Increment", () => {
      let makeState = (~a=0, ~b=0, ~c=0, ~d=0, ~e=0, ()) => {
        let cpu = Cpu.make();
        let cpu = cpu |> Cpu.setRegisters(~a, ~b, ~c, ~d, ~e);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        (cpu, mmu, gpu);
      };
      describe("INC B", () => {
        test("increments register b", () => {
          let (cpu, mmu, gpu) = makeState(~b=9, ());
          let cpu1 = Cpu.Ops.Increment8.inc_b({cpu, mmu, gpu}).cpu;
          expect(cpu1.registers.b) |> toEqual(0x0a);
        });
        test("updates the carry flag", () => {
          let (cpu, mmu, gpu) = makeState(~b=255, ());
          let cpu1 = Cpu.Ops.Increment8.inc_b({cpu, mmu, gpu}).cpu;
          expect((cpu1.registers.b, cpu1.registers.f))
          |> toEqual((0x0, 0x0 lor 0x1 lsl 7));
        });
      });
      describe("INC C", () => {
        test("increments register c", () => {
          let (cpu, mmu, gpu) = makeState(~c=9, ());
          let cpu1 = Cpu.Ops.Increment8.inc_c({cpu, mmu, gpu}).cpu;
          expect(cpu1.registers.c) |> toEqual(0x0a);
        });
        test("updates the carry flag", () => {
          let (cpu, mmu, gpu) = makeState(~c=255, ());
          let cpu1 = Cpu.Ops.Increment8.inc_c({cpu, mmu, gpu}).cpu;
          expect((cpu1.registers.c, cpu1.registers.f))
          |> toEqual((0x0, 0x0 lor 0x1 lsl 7));
        });
      });
      test("INC BC", () => {
        let (cpu, mmu, gpu) = makeState(~c=255, ());
        let cpu1 = Cpu.Ops.Increment16.inc_bc({mmu, gpu, cpu}).cpu;
        expect((Cpu.rBc(cpu1), cpu1.registers.b, cpu1.registers.c))
        |> toEqual((256, 0x1, 0x0));
      });
      test("INC DE", () => {
        let cpu = Cpu.make() |> Cpu.setRegisters(~e=255);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        let cpu1 = Cpu.Ops.Increment16.inc_de({mmu, gpu, cpu}).cpu;
        expect((Cpu.rDe(cpu1), cpu1.registers.d, cpu1.registers.e))
        |> toEqual((256, 0x1, 0x0));
      });
      test("INC HL", () => {
        let cpu = Cpu.make() |> Cpu.setRegisters(~l=255);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        let cpu1 = Cpu.Ops.Increment16.inc_hl({mmu, gpu, cpu}).cpu;
        expect((Cpu.rHl(cpu1), cpu1.registers.h, cpu1.registers.l))
        |> toEqual((256, 0x1, 0x0));
      });
    });
    describe("Decrement", () => {
      test("DEC BC", () => {
        let cpu = Cpu.make() |> Cpu.setRegisters(~b=1, ~c=0);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        let cpu1 = Cpu.Ops.Decrement16.dec_bc({mmu, gpu, cpu}).cpu;
        expect((Cpu.rBc(cpu1), cpu1.registers.b, cpu1.registers.c))
        |> toEqual((255, 0x0, 0xff));
      });
      test("DEC DE", () => {
        let cpu = Cpu.make() |> Cpu.setRegisters(~d=1, ~e=0);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        let cpu1 = Cpu.Ops.Decrement16.dec_de({mmu, gpu, cpu}).cpu;
        expect((Cpu.rDe(cpu1), cpu1.registers.d, cpu1.registers.e))
        |> toEqual((255, 0x0, 0xff));
      });
      test("DEC HL", () => {
        let cpu = Cpu.make() |> Cpu.setRegisters(~h=1, ~l=0);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        let cpu1 = Cpu.Ops.Decrement16.dec_hl({mmu, gpu, cpu}).cpu;
        expect((Cpu.rHl(cpu1), cpu1.registers.h, cpu1.registers.l))
        |> toEqual((255, 0x0, 0xff));
      });
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
        let cpu1 = Cpu.Ops.Rotation.rlca({cpu, mmu, gpu}).cpu;
        expect((cpu1.registers.a, Cpu.getFlag(Cpu.Flags.C, cpu1)))
        |> toEqual((expected, carry));
      },
    );
  });
});
