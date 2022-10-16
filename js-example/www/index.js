import { ErarsContext, init_logger } from "../pkg/erars";

const game = fetch("./game.era").then(g => g.arrayBuffer());

init_logger();

async function start() {
  const config = await fetch("./emuera.config").then(g => g.text());
  const ctx = new ErarsContext(new Uint8Array(await game), config);
  console.log("Load success!");

  const ret = ctx.run(0);
  console.log("console display: ", ret);
}

start();
