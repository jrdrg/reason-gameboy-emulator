# gameboy

Gameboy emulator written in ReasonML

## Docs

http://imrannazar.com/GameBoy-Emulation-in-JavaScript:-The-CPU

http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf

http://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html

https://fms.komkon.org/GameBoy/Tech/Software.html

https://gekkio.fi/files/gb-docs/gbctr.pdf

http://www.devrs.com/gb/files/opcodes.html

https://realboyemulator.wordpress.com/2013/01/03/a-look-at-the-game-boy-bootstrap-let-the-fun-begin/

## Run Project

```sh
npm install
npm start
# in another tab
npm run webpack
```

After you see the webpack compilation succeed (the `npm run webpack` step), open up `src/index.html` (**no server needed!**). Then modify whichever `.re` file in `src` and refresh the page to see the changes.

**For more elaborate ReasonReact examples**, please see https://github.com/reasonml-community/reason-react-example

## Build for Production

```sh
npm run build
npm run webpack:production
```

This will replace the development artifact `build/Index.js` for an optimized version.

**To enable dead code elimination**, change `bsconfig.json`'s `package-specs` `module` from `"commonjs"` to `"es6"`. Then re-run the above 2 commands. This will allow Webpack to remove unused code.
