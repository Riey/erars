import { ErarsContext, init_logger } from "../pkg";

const game = fetch("./game.era").then(g => g.arrayBuffer());

init_logger();

async function start() {
  const config = await fetch("./emuera.config").then(g => g.text());
  const ctx = new ErarsContext(new Uint8Array(await game), config, {
    input: async(con) => {
      console.log("console display: ", con);
      throw "Not implemented";
    },
    redraw: async(con) => {
      console.log("console display: ", con);
      throw "Not implemented";
    },
  });
  console.log("Load success!");
  await ctx.run();
}

start();
