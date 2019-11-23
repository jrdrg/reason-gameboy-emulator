open Jest;

describe("Cpu registers", () => {
  open Expect;

  describe("8-bit registers", () => {
    test("A", () => {
      let cpu = Cpu.make() |> Cpu.setRegisters(~a=3);
      expect(cpu.registers.a) |> toBe(3);
    });
    test("B", () => {
      let cpu = Cpu.make() |> Cpu.setRegisters(~b=3);
      expect(cpu.registers.b) |> toBe(3);
    });
    test("C", () => {
      let cpu = Cpu.make() |> Cpu.setRegisters(~c=3);
      expect(cpu.registers.c) |> toBe(3);
    });
    test("D", () => {
      let cpu = Cpu.make() |> Cpu.setRegisters(~d=3);
      expect(cpu.registers.d) |> toBe(3);
    });
    test("E", () => {
      let cpu = Cpu.make() |> Cpu.setRegisters(~e=3);
      expect(cpu.registers.e) |> toBe(3);
    });
    test("H", () => {
      let cpu = Cpu.make() |> Cpu.setRegisters(~h=3);
      expect(cpu.registers.h) |> toBe(3);
    });
    test("L", () => {
      let cpu = Cpu.make() |> Cpu.setRegisters(~l=3);
      expect(cpu.registers.l) |> toBe(3);
    });
  });

  describe("16-bit registers", () => {
    test("AF", () => {
      let cpu = Cpu.make() |> Cpu.writeRegister16(AF, 10);
      expect(Cpu.rAf(cpu)) |> toBe(10);
    });
    test("BC", () => {
      let cpu = Cpu.make() |> Cpu.writeRegister16(BC, 10);
      expect(Cpu.rBc(cpu)) |> toBe(10);
    });
    test("DE", () => {
      let cpu = Cpu.make() |> Cpu.writeRegister16(DE, 10);
      expect(Cpu.rDe(cpu)) |> toBe(10);
    });
    test("HL", () => {
      let cpu = Cpu.make() |> Cpu.writeRegister16(HL, 10);
      expect(Cpu.rHl(cpu)) |> toBe(10);
    });
  });
});
