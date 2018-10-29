/* 256 bytes */
let bios = [|
  0x31,
  0xFE,
  0xFF,
  0xAF,
  0x21,
  0xFF,
  0x9F,
  0x32,
  0xCB,
  0x7C,
  0x20,
  0xFB,
  0x21,
  0x26,
  0xFF,
  0x0E,
  0x11,
  0x3E,
  0x80,
  0x32,
  0xE2,
  0x0C,
  0x3E,
  0xF3,
  0xE2,
  0x32,
  0x3E,
  0x77,
  0x77,
  0x3E,
  0xFC,
  0xE0,
  0x47,
  0x11,
  0x04,
  0x01,
  0x21,
  0x10,
  0x80,
  0x1A,
  0xCD,
  0x95,
  0x00,
  0xCD,
  0x96,
  0x00,
  0x13,
  0x7B,
  0xFE,
  0x34,
  0x20,
  0xF3,
  0x11,
  0xD8,
  0x00,
  0x06,
  0x08,
  0x1A,
  0x13,
  0x22,
  0x23,
  0x05,
  0x20,
  0xF9,
  0x3E,
  0x19,
  0xEA,
  0x10,
  0x99,
  0x21,
  0x2F,
  0x99,
  0x0E,
  0x0C,
  0x3D,
  0x28,
  0x08,
  0x32,
  0x0D,
  0x20,
  0xF9,
  0x2E,
  0x0F,
  0x18,
  0xF3,
  0x67,
  0x3E,
  0x64,
  0x57,
  0xE0,
  0x42,
  0x3E,
  0x91,
  0xE0,
  0x40,
  0x04,
  0x1E,
  0x02,
  0x0E,
  0x0C,
  0xF0,
  0x44,
  0xFE,
  0x90,
  0x20,
  0xFA,
  0x0D,
  0x20,
  0xF7,
  0x1D,
  0x20,
  0xF2,
  0x0E,
  0x13,
  0x24,
  0x7C,
  0x1E,
  0x83,
  0xFE,
  0x62,
  0x28,
  0x06,
  0x1E,
  0xC1,
  0xFE,
  0x64,
  0x20,
  0x06,
  0x7B,
  0xE2,
  0x0C,
  0x3E,
  0x87,
  0xF2,
  0xF0,
  0x42,
  0x90,
  0xE0,
  0x42,
  0x15,
  0x20,
  0xD2,
  0x05,
  0x20,
  0x4F,
  0x16,
  0x20,
  0x18,
  0xCB,
  0x4F,
  0x06,
  0x04,
  0xC5,
  0xCB,
  0x11,
  0x17,
  0xC1,
  0xCB,
  0x11,
  0x17,
  0x05,
  0x20,
  0xF5,
  0x22,
  0x23,
  0x22,
  0x23,
  0xC9,
  0xCE,
  0xED,
  0x66,
  0x66,
  0xCC,
  0x0D,
  0x00,
  0x0B,
  0x03,
  0x73,
  0x00,
  0x83,
  0x00,
  0x0C,
  0x00,
  0x0D,
  0x00,
  0x08,
  0x11,
  0x1F,
  0x88,
  0x89,
  0x00,
  0x0E,
  0xDC,
  0xCC,
  0x6E,
  0xE6,
  0xDD,
  0xDD,
  0xD9,
  0x99,
  0xBB,
  0xBB,
  0x67,
  0x63,
  0x6E,
  0x0E,
  0xEC,
  0xCC,
  0xDD,
  0xDC,
  0x99,
  0x9F,
  0xBB,
  0xB9,
  0x33,
  0x3E,
  0x3c,
  0x42,
  0xB9,
  0xA5,
  0xB9,
  0xA5,
  0x42,
  0x4C,
  0x21,
  0x04,
  0x01,
  0x11,
  0xA8,
  0x00,
  0x1A,
  0x13,
  0xBE,
  0x20,
  0xFE,
  0x23,
  0x7D,
  0xFE,
  0x34,
  0x20,
  0xF5,
  0x06,
  0x19,
  0x78,
  0x86,
  0x23,
  0x05,
  0x20,
  0xFB,
  0x86,
  0x20,
  0xFE,
  0x3E,
  0x01,
  0xE0,
  0x50,
|];

type t = {
  finishedBios: bool,
  cartType: int,
  oam: array(int),
  rom: array(int),
  externalRam: array(int),
  videoRam: array(int),
  workRam: array(int),
};

let load = bytes => {
  finishedBios: true,
  cartType: bytes[0x147], /* from docs - http://gameboy.mongenel.com/dmg/asmmemmap.html */
  oam: [||],
  rom: bytes,
  externalRam: [||],
  videoRam: [||],
  workRam: [||],
};

let reset = mmu => {
  ...mmu,
  finishedBios: false,
  oam: [||],
  externalRam: Array.make(8192, 0),
  videoRam: Array.make(8192, 0),
  workRam: Array.make(8192, 0),
};

let read8 = (addr, mmu) => {
  Js.log(Printf.sprintf("Reading %x", addr));
  /* switch on the first byte */
  switch (addr land 0xf000) {
  | 0x0000 =>
    switch (mmu.finishedBios, addr <= 0xff, addr === 0x100) {
    | (false, true, _) => (bios[addr], mmu)
    | (false, _, true) => (bios[addr], {...mmu, finishedBios: true})
    | (true, _, _) => (mmu.rom[addr], mmu)
    | _ => (mmu.rom[addr], mmu)
    }
  | 0x1000
  | 0x2000
  | 0x3000 => (mmu.rom[addr], mmu)
  | 0x4000
  | 0x5000
  | 0x6000
  | 0x7000 => ((-1), mmu) /* ROM bank from cartridge */
  | 0x8000
  | 0x9000 => ((-1), mmu) /* Video RAM */
  | 0xA000
  | 0xB000 => ((-1), mmu) /* External RAM */
  /*
    Work RAM
    This removes the first nibble of the address
    0xC000 + 0x1fff = 0xDFFF
    <addr> & 0x1fff means that Cxxx & Dxxx are the same as Exxx & Fxxx
   */
  | 0xC000
  | 0xD000
  | 0xE000 => (mmu.workRam[addr land 0x1fff], mmu)
  | 0xF000 =>
    switch (addr land 0x0f00) {
    | 0xE00 => ((-1), mmu) /* OAM */
    | 0xF00 => ((-1), mmu) /* hardware stuff */
    | _ => (mmu.workRam[addr land 0x1fff], mmu) /* same as Work RAM above */
    }
  | _ => (0x0000, mmu)
  };
};

/*
     Read one byte then the next byte left shifted by 8 bits
     It's little-endian, i.e.
     0x34, 0x12
     0x34 + (0x12 << 8) = 0x1234
 */
let read16 = (addr, mmu) => {
  let (a, mmu1) = read8(addr, mmu);
  let (b, mmu2) = read8(addr + 1, mmu1);
  let c = b lsl 8;
  /* read8(addr, mmu) + read8(addr + 1, mmu) lsl 8 */
  (a + c, mmu2);
};

let write8 = (addr, v, mmu: t) => {
  Js.log(Printf.sprintf("Writing %x to %x", addr, v));
  switch (addr land 0xf000) {
  /* | 0x0000 | 0x1000  => */
  | 0x6000
  | 0x7000 =>
    /* select memory model to use - 0: 16/8 mode, 1: 4/32 mode */
    let mode = v land 0x1;
    mmu;
  | _ => mmu
  };
};
